package snnaccel

import chisel3._
import chisel3.util.{Decoupled, Counter, switch, is, log2Ceil, DecoupledIO,
  MuxCase}

import dsl._
//import chisel3.experimental.{ChiselAnnotation, annotate}
//import firrtl2.annotations.MemoryArrayInitAnnotation

import MemoryFileFormat._

case class TopOptions(
  useVerilator : Boolean = false,
  neuronCores : Seq[Int] = Seq(),
  upToLayer : Int = 1,
  inputTypes : Seq[snn.IOType] = Seq(snn.Activations),
  timeout : Int = 1000,
  outputSpikes : Boolean = false) {
  val memFileFormat = if(useVerilator) MemoryFileFormat.Verilator else
    MemoryFileFormat.Treadle
}

object chiselTypeFromDslType {
  def apply(ioType : snn.IOType) = ioType match {
    case snn.SpikeTrain => new Spike()
    case snn.Activations => new TopInputBundle(32)
    case snn.EventQueue => new EventQueueBundle(32)
    case snn.MemPots => new TopInputBundle(32)
  }
}

object Top {
  def generateAccelerator(model : ujson.Value, options : TopOptions) = {
    val numLayers = math.min(model("layers").arr.size, options.upToLayer)
    val numCores = if(options.neuronCores.size == 0)
      (0 until numLayers).map(_ => 1) else
      options.neuronCores
    require(numCores.size == numLayers)

    var lastOutputType : snn.IOType = null
    val layerCores = (0 until numLayers).map { i =>
      val dslInputType = if(options.inputTypes.size - 1 < i) snn.SpikeTrain else
        options.inputTypes(i)
      val dslOutputType = if(options.inputTypes.size - 1 < i + 1) {
        if(!options.outputSpikes && i == numLayers - 1)
          snn.MemPots else snn.SpikeTrain } else
        options.inputTypes(i + 1)
      lastOutputType = dslOutputType
      val inputType = chiselTypeFromDslType(dslInputType)
      val dataType = if(dslInputType == snn.Activations) new SIntDataType() else
        new SpikeDataType()
      val layer = model("layers")(i)
      val beta = layer("beta").num.toInt.S
      val weights = layer("weights").arr.map(
        _.arr.map(x => x.num.toInt).toSeq).toSeq.transpose
      //println("%d %d".format(weights.size, weights(0).size))
      //throw new RuntimeException("stop");

      val numNeurons = weights.size
      val numInputs = weights(0).size
      val neuronsPerCore = numNeurons / numCores(i)

      val formattedWeights = weights.grouped(numCores(i)).toSeq.transpose.
        map(_.flatten.map(_.S(32.W))).toSeq

      () => new FCInputLayer(options, formattedWeights, neuronsPerCore,
        numCores(i), dslInputType, dslOutputType,
        numInputs, layer("bias").arr.map(_.num.toInt.S).toSeq,
        beta, numNeurons, inputType, dataType)
    }

    //println(formattedWeights.size)
    new Top(layerCores, options.inputTypes(0), lastOutputType)
  }
}


class SpikingLayer(val id : Int, layer : => FCInputLayer,
  dataType : DataType) extends StreamingModule {
  val layerCore = Module(layer)
  val numNeurons = layerCore.numNeurons
  val accumType = chiselTypeOf(layerCore.output.bits.value)
  //val accumType = SInt(35.W)
  val outputType = layerCore.dslOutputType

  val input = IO(Flipped(Decoupled(layerCore.inputType)))
  val output = IO(Decoupled(chiselTypeFromDslType(outputType)))
  layerCore.input <> input

  val mem = new MemWrapper2(numNeurons, accumType)
  //TODO ZeroInitialize
  loadMemoryFromSeq(mem.mem, (0 until mem.size).map(_ => 0.S),
    layerCore.options.memFileFormat)
  val incrementAddress = Wire(Bool())
  incrementAddress := false.B
  val (addressCounter, _) = Counter(incrementAddress, numNeurons)
  mem.readAddress := addressCounter
  val address = Seq.fill(3)(Reg(chiselTypeOf(mem.readAddress)))

  val inputAccum = Seq.fill(2)(Reg(accumType))
  val newMemPot = Seq.fill(2)(Reg(accumType))
  val threshold = 16348.S
  val beta = layerCore.beta

  mem.writeAddress := address(1)
  mem.writeEnable := false.B
  val newMemPotValue = Wire(accumType)
  newMemPotValue := DontCare
  mem.input := newMemPotValue

  val outputAccum = Wire(new TopInputBundle(32))
  outputAccum.value := newMemPot(1)

  val outputValue = Wire(new Spike())
  val spike = Seq.fill(2)(RegInit(false.B))
  val outputSpike = Seq.fill(1)(RegInit(false.B))
  outputValue.value := outputSpike(0)

  val outputEvent = Reg(new EventQueueBundle(32))
  outputEvent.eoq := true.B

  val pipeline = new ReadProcessWrite2(layerCore.output, output, Seq(
    Stage { ctrl =>
      //printf(cf"stage1\n")
      inputAccum(0) := layerCore.output.bits.value
      //printf(cf"id=${id} inputAccum=${layerCore.output.bits.result}\n")
      address(0) := addressCounter
      incrementAddress := true.B
    },
    Stage { ctrl =>
      address(1) := address(0)
      spike(1) := spike(0)
      inputAccum(1) := inputAccum(0)

      //TODO enable hazard detection conditionally
      //printf(cf"bubble=${ctrl.outgoingBubble}\n")
      //TODO why doesn't stageActive work here?
//      val oldMemPot = MuxCase(
//        mem.output, Seq(
//          (address(0) === address(1)) -> newMemPot(0),
//          (address(0) === address(2)) -> newMemPot(1)))
        // && ctrl.stageActive(+1),
      val oldMemPot = mem.output
      val foo = newMemPot(1)
      //printf(cf"id=${id} spike=${spike(0)} spike=${spike(1)} accum=${inputAccum} old=${oldMemPot} newMemPot=${newMemPot(0)} newMemPot2=${foo} address=${ctrl.stageActive(+1)}\n")
      spike(0) := Mux(oldMemPot > threshold, true.B, false.B)
      val newMemPotValue = dataType.rescalingMul(beta, oldMemPot)
      newMemPot(0) := newMemPotValue
      ctrl.passOn()
    },
    Stage { ctrl =>
      address(2) := address(1)

      newMemPotValue := inputAccum(1) + newMemPot(0) - Mux(spike(0), beta, 0.S)
      //printf(cf"stage3 newMemPot=${newMemPot(0)} newValue=${newMemPotValue}\n")
      val outputSpikeValue = newMemPotValue > threshold
      outputSpike(0) := outputSpikeValue
      newMemPot(1) := newMemPotValue
      //printf(cf"id=${id} outputSpike=${newMemPotValue > threshold}\n")
      //printf(cf"id=${id} newValue=${newMemPotValue}\n")
      //printf(cf"id=${id} inputAccum=${inputAccum(1)}\n")

      mem.writeEnable := true.B
      ctrl.passOn()
      if(outputType == snn.EventQueue) {
        val lastMemPot = address(1) === (numNeurons - 1).U
        when(!outputSpikeValue && !lastMemPot) {
          ctrl.passBubble()
        }.otherwise {
          //printf(cf"id=${id} event address=${address(1)} valid=${outputSpikeValue} eoq=${lastMemPot}\n")
        }
        outputEvent.value := address(1)
        outputEvent.eoq := lastMemPot
        outputEvent.valid := outputSpikeValue
      }
    }
  ), if(outputType == snn.SpikeTrain) outputValue else 
    if(outputType == snn.EventQueue) outputEvent else outputAccum)

  pipeline()

//  val outputValue = Wire(new Spike())
//  val spike = Wire(Bool())
//  spike := false.B
//  outputValue.value := spike
//  val pipeline = new ReadProcessWrite(layerCore.output, output, 
//  { readAccum : TopOutputBundle =>
//    spike := Mux(readAccum.result > 16348.S, true.B, false.B)
//    outputValue
//  })
//  pipeline()

//  val outputValue = Wire(new Spike())
//  val spike = Reg(Bool())
//  val memPots = Buffer(numNeurons, UInt(32.W))
//  val address = RegInit(0.U)
//  val oldMemPot = Reg(chiselTypeOf(memPots.elem))
//  val accum = Reg(chiselTypeOf(memPots.elem))
//  val address = Reg(chiselTypeOf(memPots.elem))
//  val address2 = Reg(chiselTypeOf(memPots.elem))
//  memPots.write(address2)
//  memPots.read(address)
//  outputValue.value := spike
//  spike := false.B
//  val pipeline = new ReadProcessWrite(layerCore.output, output, 
//  { readAccum : TopOutputBundle => val stages = Seq(
//    Stage { ctrl =>
//      spike := Mux(readAccum.result > 16348.S, true.B, false.B)
//      oldMemPot := memPots.readWord
//      accum := readAccum
//      address2 := address
//    },
//    Stage { ctrl =>
//      memPots.input := oldMemPot + readAccum
//      ctrl.passOn()
//    }
//    Stage { ctrl =>
//      memPots.enableWrite()
//      ctrl.passOn()
//    })
//    (stages, spike)
//  })
//  pipeline()

//  val readAccum = Reg(chiselTypeOf(layerCore.output.bits.result))
//  val spike = Reg(Bool())
//  val outputValue = Wire(new Spike())
//  outputValue.value := spike
//  output.noenq()
//  layerCore.output.nodeq()
//  when(output.fire) {
//    printf(cf"id=${id} spike=${spike}\n")
//  }
//  val processing = Pipeline(
//    Stage { ctrl =>
//      layerCore.output.deq()
//      when(layerCore.output.fire) {
//        readAccum := layerCore.output.bits.result
//        //printf(cf"readAccum=${layerCore.output.bits.result}\n")
//        ctrl.passOn()
//      }
//    },
//    Stage { ctrl =>
//      spike := Mux(readAccum > 16384.S, true.B, false.B)
//      ctrl.passOn()
//    },
//    Stage { ctrl =>
//      output.enq(outputValue)
//
//      when(!output.fire) {
//        ctrl.multicycle { ctrl2 =>
//          output.enq(outputValue)
//          when(output.fire) {
//            ctrl2.goBack()
//          }
//        }
//      }
//    }
//  )
//  processing()
}

class Top(layers : Seq[() => FCInputLayer],
  inputType : snn.IOType,
  outputType : snn.IOType) extends Module {
  val input = IO(Flipped(Decoupled(chiselTypeFromDslType(inputType))))
  val output = IO(Decoupled(chiselTypeFromDslType(outputType)))

  var previousLayer : StreamingModule = null
  for((layer, i) <- layers.zipWithIndex) {
    val layerCore = Module(new SpikingLayer(i, layers(i)(),
      new SIntDataType()))
    if(i == 0)
      layerCore.input <> input
    else
      layerCore.input <> previousLayer.output
    if(i == layers.size - 1)
      layerCore.output <> output
    previousLayer = layerCore
  }
}
