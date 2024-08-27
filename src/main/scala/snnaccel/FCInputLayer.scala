package snnaccel

import chisel3._
import chisel3.util.{Decoupled, Counter, switch, is, log2Ceil, DecoupledIO,
  MuxCase}

import dsl._

abstract class InputBundle extends Bundle {
  val value : Bits
}
class TopInputBundle(val w: Int) extends InputBundle {
  override val value = SInt(w.W)
}

class EventQueueBundle(val w: Int) extends InputBundle {
  val value = UInt(w.W)
  val valid = Bool()
  val eoq = Bool()
}
class Spike extends InputBundle {
  override val value = Bool()
}

abstract class StreamingModule extends Module {
  val input : DecoupledIO[Data]
  val output : DecoupledIO[Data]
}

trait DataType {
  def rescalingMul(op1 : Data, op2 : SInt) : SInt
}
class SpikeDataType extends DataType {
  def rescalingMul(op1 : Data, op2 : SInt) : SInt = {
    //op1.asInstanceOf[Bool] * op2
    //printf(cf"op1=${op1} op2=${op2}\n")
    Mux(op1.asInstanceOf[Bool] === true.B, op2, 0.S)
  }
}
class SIntDataType extends DataType {
  def rescalingMul(op1 : Data, op2 : SInt) : SInt = {
    (op1.asInstanceOf[SInt] * op2) >> 14
  }
}

class FCInputLayer(val options : TopOptions, weights : Seq[Seq[SInt]],
  neuronsPerCore : Int,
  numCores : Int,
  val dslInputType : snn.IOType,
  val dslOutputType : snn.IOType,
  numInputs : Int,
  bias : Seq[SInt],
  val beta : SInt,
  val numNeurons : Int,
  val inputType : InputBundle,
  val dataType : DataType) extends StreamingModule {
  val input = IO(Flipped(Decoupled(inputType)))
  val output = IO(Decoupled(new TopInputBundle(32)))

  val resultValid = RegInit(false.B)
  output.valid := resultValid
  val bufferingActivations = RegInit(true.B)

  val biasMem = VecInit(bias)
  val weightMems = weights.map(MemContaining(_, options.memFileFormat))
  val activationBuffer = Buffer(numInputs, chiselTypeOf(input.bits))
  val biasAddr = RegInit(0.U(log2Ceil(bias.size).W))

  val acc = Seq.fill(weights.size)(RegInit(0.S(32.W)))
  output.bits.value := acc(0)

  input.ready := false.B

  val eventInputs = dslInputType == snn.EventQueue
  val countInput = Wire(Bool())
  countInput := false.B
  val (inputCounter, allInputsReceived) = Counter(countInput, numInputs)
  val weightReadAddress = Wire(chiselTypeOf(weightMems(0).readAddress))
  val weightReadOffset = RegInit(chiselTypeOf(weightReadAddress), 0.U)
  weightReadAddress := 0.U
  if(eventInputs) {
    weightMems.foreach { _.readAddress := weightReadAddress }
  }
  val eventType = new EventQueueBundle(32)
  val inputEvent = Seq.fill(1)(Reg(eventType))
  val stopInput = RegInit(false.B)
  //TODO replace this with resetStage (it doesn't work for some reason)
  val ownBubble = RegInit(true.B)

  val fsm = 
    While(true.B) {
    Stmt { weightMems.foreach { _.pop() } } ::
    For(0 until neuronsPerCore) { intra_tile_id : UInt => 
      Stmt { finish => 
        val stages = Seq(
          Stage { ctrl =>
            when(bufferingActivations && !stopInput) {
              input.deq()
            }
            val firing = Mux(bufferingActivations, input.fire, true.B)
            val inputValue = Mux(bufferingActivations, input.bits,
              activationBuffer.word)
            when(firing) {
              countInput := true.B

              if(!eventInputs) {
                when(allInputsReceived) {
                  finish()
                }
                for(i <- 0 until weights.size)
                  acc(i) := acc(i) + dataType.rescalingMul(inputValue.value, weightMems(i).word)
              } else {
                inputEvent(0) := inputValue
//                if(eventInputs) {
//                  val foo = inputValue.asInstanceOf[EventQueueBundle]
//                  printf(cf"passing index=${foo.value} valid=${foo.valid} eoq=${foo.eoq}\n")
//                }
                weightReadAddress := weightReadOffset + inputValue.value.asUInt
                weightMems.foreach { _.readEnable := true.B }
              }

              when(bufferingActivations) {
                if(eventInputs) {
                  when(inputValue.asInstanceOf[EventQueueBundle].eoq) {
                    stopInput := true.B
                  }
                }
                activationBuffer.append(inputValue)
              }.otherwise {
//                if(eventInputs) {
//                  val foo = activationBuffer.word.asInstanceOf[EventQueueBundle]
//                  printf(cf"popped index=${foo.value} valid=${foo.valid} eoq=${foo.eoq}\n")
//                }
                activationBuffer.pop()
              }

              if(!eventInputs) {
                weightMems.foreach { _.pop() }
              }
              ctrl.passOn()
              ownBubble := false.B
            }
          }
        ) ++ (if(!eventInputs) Seq() else Seq(
          Stage { ctrl =>
            when(!ownBubble) {
              //printf(cf"buffering=${bufferingActivations} offset=${weightReadOffset} valid=${inputEvent(0).valid} eoq=${inputEvent(0).eoq} index=${inputEvent(0).value} word=${weightMems(0).word}\n")
              when(inputEvent(0).valid) {
                for(i <- 0 until weights.size)
                  acc(i) := acc(i) + dataType.rescalingMul(true.B, weightMems(i).word)
              }
              when(inputEvent(0).eoq) {
                ctrl.resetStage()
                ownBubble := true.B
                finish()
                stopInput := false.B
              }
            }
          }))
        //TODO second constructor
        Pipeline(stages : _*)()
      } ::
    //TODO write in this cycle or in the next
      Stmt {
        for(i <- 0 until weights.size)
          acc(i) := acc(i) + biasMem(biasAddr + i.U)
        biasAddr := biasAddr + numCores.U
        weightReadOffset := weightReadOffset + numInputs.U
        activationBuffer.resetPointers()
        bufferingActivations := false.B
        resultValid := true.B
      } ::
      For(0 until weights.size) { output_id : UInt => Stmt { finish =>
        accessSeq(acc, output_id) { accElem =>
          output.bits.value := accElem
        }
        when(output.ready && resultValid) {
          //TODO reset accumulators differently
          accessSeq(acc, output_id) { accElem =>
            accElem := 0.S
          }
          finish()
        }
      } } + Stmt { 
        activationBuffer.pop()
        resultValid := false.B
      }
    } ::
    Stmt { 
      weightMems.foreach { _.resetPointers() }
      bufferingActivations := true.B
      activationBuffer.resetPointers()
      biasAddr := 0.U
      weightReadOffset := 0.U
    }
    }
  fsm()

//  val fsm = 
//    While(true.B) {
//    Stmt { weightMems.foreach { _.pop() } } ::
//    For(0 until neuronsPerCore) { intra_tile_id : UInt => 
//      For(0 until numInputs) { When(bufferingActivations) thenDo {
//        UponInput(input) trigger Stmt {
//          weightMems.foreach { _.pop() }
//          val activation = input.deq()
//          //printf(cf"activation=${activation} weight=${weightMems(0).word}\n")
//          for(i <- 0 until weights.size)
//            acc(i) := acc(i) + dataType.rescalingMul(activation.value, weightMems(i).word)
//          activationBuffer.append(activation)
//        }} otherwise Stmt {
//          weightMems.foreach { _.pop() }
//          activationBuffer.pop()
//          for(i <- 0 until weights.size)
//            acc(i) := acc(i) + dataType.rescalingMul(activationBuffer.word.value, weightMems(i).word)
//        }
//      } ::

//  val loopcounter1 = fsm.body.asInstanceOf[Sequential].steps(1).asInstanceOf[Loop].counter
//  val loopcounter2 = fsm.body.asInstanceOf[Sequential].steps(1).asInstanceOf[Loop].body.asInstanceOf[Sequential].steps(0).asInstanceOf[Loop].counter
//  val innerState = fsm.body.asInstanceOf[Sequential].steps(1).asInstanceOf[Loop].body.asInstanceOf[Sequential].state
//  if(numInputs == 100)
//    printf(cf"innerState=${innerState}, act1=${input.bits.value}, act2=${activationBuffer.word}, readEnable=${activationBuffer.readEnable}, readAddress=${activationBuffer.readAddress}, resultValid=${resultValid}, loopcounter1=$loopcounter1,  loopcounter2=$loopcounter2, counter=${weightMems(0).readAddress}, acc=${acc(0)}%d, weight=${weightMems(0).word} \n")
}
