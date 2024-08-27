package snnaccel.dsl {

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Paths, Files}
import firrtl.annotations.MemoryLoadFileType
import chisel3.experimental.AffectsChiselPrefix
import chisel3.util.{Counter, log2Ceil, DecoupledIO}

import scala.collection.mutable.ListBuffer
import scala.util.Using

object MemoryFileFormat extends Enumeration {
  type MemoryFileFormat = Value
  val Verilator, Treadle = Value
}
import MemoryFileFormat._

object loadMemoryFromSeq {
  var counter = 0

  private def asUnsigned(x : BigInt, width : Int) : BigInt = {
    var ret = x
    if(x >= 0)
      return x
    else {
      ret *= -1
      for(i <- 0 until width)
        ret = ret.flipBit(i)
      ret += 1
    }
    ret
  }
  private def format(fileFormat : MemoryFileFormat)(
    x : BigInt, width : Int) : String =
    (if(x < 0 && fileFormat == MemoryFileFormat.Treadle) "-" else "") +
    ((width - 1) to 0 by -1).map(i => if(x.abs.testBit(i)) "1" else "0").mkString
    

  private def writeData[T <: Data](writer : BufferedWriter,
    data : Seq[T], fileFormat : MemoryFileFormat) : Unit = {
    writer.write("")
    //val toUInt = (x : BigInt, w : Int) => if(x < 0) (~x) + 1 else x
    val formatFunc = 
      if(fileFormat == MemoryFileFormat.Treadle)
        format(fileFormat) _
      else 
        (x : BigInt, w : Int) => {
          val ret = asUnsigned(x, w)
          format(fileFormat)(ret, w)
        }

    for(value <- data) {
      val formatted = formatFunc(value.litValue, value.getWidth)
      writer.write(formatted + "\n")
    }
  }

  def apply[T <: Data](memory : MemBase[T], data : Seq[T],
    memFileFormat : MemoryFileFormat) : String = {
    val folder = Paths.get("mem_files")
    if(!Files.isDirectory(folder))
      Files.createDirectories(folder)
    val filePath = "mem%d.mem".format(counter)
    Using(new BufferedWriter(new FileWriter(filePath))) { writer =>
      writeData(writer, data, memFileFormat)
    }
    val filePath2 = "%s/mem%d.mem".format(folder, counter)
    Using(new BufferedWriter(new FileWriter(filePath2))) { writer =>
      writeData(writer, data, memFileFormat)
    }
    //val absolutePath = Paths.get(filePath).toAbsolutePath.toString
    loadMemoryFromFileInline(memory, filePath, MemoryLoadFileType.Binary)

    counter += 1
    filePath
  }
}

trait LightModule extends AffectsChiselPrefix { }

object Buffer {
  def apply[T <: Data](size : BigInt, t : T) = new Buffer(size, t)
}
class ReadMem[T <: Data](size : BigInt, t : T) extends LightModule {
  val mem = SyncReadMem(size, t)
  val readEnable = Wire(Bool())
  val readAddressReg = RegInit(0.U(log2Ceil(size).W))
  val readAddress = Wire(UInt(log2Ceil(size).W))
  readAddress := readAddressReg
  readEnable :=  false.B
  val word = mem.read(readAddress, readEnable)

  when(readEnable) {
    readAddressReg := readAddressReg + 1.U
  }

  def pop() = {
    readEnable := true.B
  }
  def cancelPop() = {
    readEnable := false.B
  }
  def resetPointers() = {
    readAddressReg := 0.U
  }
}
class Buffer[T <: Data](size : BigInt, t : T) extends ReadMem(size, t) {
  val writeAddress = RegInit(0.U(log2Ceil(size).W))

  def append(elem : T) = {
    mem.write(writeAddress, elem)
    writeAddress := writeAddress + 1.U
  }
  override def resetPointers() = {
    super.resetPointers()
    writeAddress := 0.U
  }
}
object accessSeq {
  def apply[T](xs : Seq[T], i : UInt)(process : T => Unit) : Unit = {
    (0 until xs.size).foldLeft(when(false.B) {}) {
      case (whenContext, j) =>
        whenContext.elsewhen(i === j.U) { process(xs(j)) }
    }
  }
}
class MemWrapper2[T <: Data](val size : Int, t : T) extends LightModule {
  val mem = SyncReadMem(size, t, SyncReadMem.WriteFirst)
  val readAddress = Wire(UInt(log2Ceil(size).W))
  val writeAddress = Wire(UInt(log2Ceil(size).W))
  val writeEnable = Wire(Bool())
  writeEnable := false.B
  val output = Wire(t)
  val input = Wire(t)
  output := mem.read(readAddress)
  when(writeEnable) {
    mem.write(writeAddress, input)
  }
}
object ConstInit {
  def apply[T <: Data](mem : MemWrapper2[T], value : T, 
    format : MemoryFileFormat) : Unit = {
      loadMemoryFromSeq(mem.mem, (0 until mem.size).map(_ => value),
        format)
  }
}
class MemWrapper[T <: Data](content : Seq[T], enable : Bool,
  memFileFormat : MemoryFileFormat) extends LightModule {
  val mem = SyncReadMem(content.size, chiselTypeOf(content(0)))
  loadMemoryFromSeq(mem, content, memFileFormat)
  val address = RegInit(0.U(log2Ceil(content.size).W))
  val readWord = mem.read(address, enable)
}
class ROM[T <: Data](content : Seq[T], memFileFormat : MemoryFileFormat) 
  extends ReadMem(content.size, chiselTypeOf(content(0))) {
  loadMemoryFromSeq(mem, content, memFileFormat)
}
object MemContaining {
  def apply[T <: Data](content : Seq[T], 
    memFileFormat : MemoryFileFormat = MemoryFileFormat.Treadle) =
      new ROM(content, memFileFormat)
}

trait Hardware {
  def apply(finish : Hardware) : Unit
  def apply() : Unit = {
    this(new NoHardware)
  }
  def enable() : Unit = { }

  def +(additionalFinish : Hardware) : Hardware =
    new AdditionalFinish(this, additionalFinish)
  def ::(before : Hardware) : Sequential = new Sequential(before :: List(this))
}
class NoHardware extends Hardware with LightModule {
  override def apply(finish : Hardware) : Unit = { }
}

object Stmt {
  def apply(run : => Unit) = new Statement(() => run)
  def apply(run : Hardware => Unit) = new FinishStatement(run)
}
class Statement(val run : () => Unit) extends Hardware with LightModule {
  override def apply(finish : Hardware) : Unit = {
    run()
    finish()
  }
}
//object HardwareConversions {
//  import scala.language.implicitConversions
//  implicit def code2Hardware(code : => Unit) : Statement = new Statement(() => code)
////  implicit def func2Hardware(code : Hardware => Unit) : FinishStatement =
////    new FinishStatement(code)
//}

class AdditionalFinish(val base : Hardware,
  val additionalFinish : Hardware) extends Hardware with LightModule {
  override def enable() : Unit = {
    base.enable()
  }
  override def apply(finish : Hardware) : Unit = {
    base(Stmt { finish(); additionalFinish() })
  }
}
class FinishStatement(val run : Hardware => Unit) extends Hardware 
  with LightModule {
  override def apply(finish : Hardware) : Unit = {
    run(finish)
  }
}
object Sequential {
  def apply(steps : Hardware*) = new Sequential(steps.toList)
}
class Sequential(val steps : List[Hardware]) extends Hardware
  with LightModule {
  val state = RegInit(0.U(log2Ceil(steps.size).W))

  override def apply(finish : Hardware) : Unit = {
    for((stepAndNextStep, i) <- (steps ++ Seq(null)).sliding(2).zipWithIndex) {
      val step = stepAndNextStep(0)
      val nextStep = stepAndNextStep(1)
      when(state === i.U) {
        step(Stmt {
          if(i == steps.size - 1) {
            state := 0.U
            finish()
          } else {
            state := state + 1.U
          }
          if(nextStep != null)
            nextStep.enable()
        })
      }
    }
  }

  override def ::(before : Hardware) = new Sequential(before :: steps)
}
object Nop {
  def apply(numClockCycles : Int) = new Nop(numClockCycles)
  def apply() = new NopForever
}
class Nop(numClockCycles : Int) extends Hardware with LightModule {
  val (counter, counterWrap) = Counter(true.B, numClockCycles)

  override def apply(finish : Hardware) : Unit = {
    when(counterWrap) {
      finish()
    }
  }
}
class NopForever extends Hardware with LightModule {
  override def apply(h : Hardware) : Unit = { }
}
object While {
  def apply(condition : Bool) = new WhileBuilder(condition)
}
class WhileBuilder(condition : Bool) {
  def apply(body : Hardware) = new While(condition, body)
}
class While(condition : Bool, val body : Hardware)
  extends Hardware
  with LightModule {
  val checkCondition = Wire(Bool())

  override def enable() : Unit = {
    body.enable()
  }

  override def apply(finish : Hardware) : Unit = {
    checkCondition := false.B
    body(Stmt(checkCondition := true.B))
    when(checkCondition && !condition) {
      finish()
    }
  }
}

object For {
  def apply(range : Range) = new LoopBuilder(range)
  //def apply(bound : UInt) = new DynamicLoopBuilder(bound)
}
class LoopBuilder(range : Range) {
  def apply(body : Hardware) = new Loop(range, (_ : UInt) => body)
  def apply(body : (UInt) => Hardware) = new Loop(range, body)
}
class Loop(range : Range, bodyConstructor : (UInt) => Hardware)
  extends Hardware
  with LightModule {
  val counterEnable = Wire(Bool())
  counterEnable := false.B
  val (counter, counterWrap) = Counter(range, counterEnable)
  val body = bodyConstructor(counter)

  override def enable() : Unit = {
    body.enable()
  }

  override def apply(finish : Hardware) : Unit = {
    body(Stmt(counterEnable := true.B))
    when(counterEnable && counterWrap) {
      finish()
    }
  }
}
class WhenBuilder(condition : Bool) {
  def thenDo(body : Hardware) = new When(Seq(Some(condition)), Seq(body))
}
object When {
  def apply(condition : Bool) = new WhenBuilder(condition)
}
class When(conditions : Seq[Option[Bool]], bodies : Seq[Hardware])
  extends Hardware with LightModule {
  override def enable() : Unit = {
    conditions.zip(bodies).foldLeft(when(false.B) {}) {
      case (whenContext, (Some(condition), body)) =>
        whenContext.elsewhen(condition) { body.enable() }
      case (whenContext, (None, body)) =>
        whenContext.elsewhen(true.B) { body.enable() }
    }
  }

  override def apply(finish : Hardware) : Unit = {
    conditions.zip(bodies).foldLeft(when(false.B) {}) {
      case (whenContext, (Some(condition), body)) =>
        whenContext.elsewhen(condition) { body(finish) }
      case (whenContext, (None, body)) =>
        whenContext.elsewhen(true.B) { body(finish) }
    }
  }

  def otherwise(body : Hardware) = new When(conditions :+ None,
    bodies :+ body)
}
class InputTriggeredBuilder[T <: Data](input : DecoupledIO[T]) {
  def trigger(body : Hardware) = new InputTriggered(input, body)
}
object UponInput {
  def apply[T <: Data](input : DecoupledIO[T]) =
    new InputTriggeredBuilder(input)
}
class InputTriggered[T <: Data](input : DecoupledIO[T],
  body : Hardware) extends Hardware with LightModule {
  val ready = RegInit(false.B)
  override def enable() : Unit = {
    ready := true.B
  }

  override def apply(finish : Hardware) : Unit = {
    input.ready := ready
    when(input.valid) {
      ready := false.B
      body(finish)
    }
  }
}

//object WriteOutput {
//  def apply[T <: Data](output : DecoupledIO[T], body : Hardware) =
//    new InputTriggered(input, body)
//}
//class InputTriggered[T <: Data](input : DecoupledIO[T],
//  body : FinishableHardware) extends FinishableHardware with LightModule {
//    //TODO init ready??
//  //input.ready
//
//  override def apply(finish : Hardware) : Unit = {
//    when(input.valid) {
//      body(finish)
//    }
//  }
//}

class ExtraStageControl(val state : UInt) {
  def goBack() : Unit = {
    //prevState?
    state := 0.U
  }
}
class ExtraStage(val execute : (ExtraStageControl) => Unit, val number : Int) {}
class PipelineControl(val state : UInt, var outgoingBubble : Bool) {
  var _incomingBubble : Option[Bool] = None
  var extraStageNum : Int = 1
  var extraStages = ListBuffer[ExtraStage]()
  var currentStageId : Int = 0

  def passOn() : Unit = {
    outgoingBubble := false.B
  }
  def passBubble() : Unit = {
    outgoingBubble := true.B
  }

  def next[T <: Data](reg : PipelineReg[T]) : T = reg(currentStageId)
  def prev[T <: Data](reg : PipelineReg[T]) : T = reg(currentStageId - 1)

  def resetStage() : Unit = {
    //TODO write with .get()
    _incomingBubble match {
      case Some(x) => x := true.B
      case None =>
    }
  }

  def multicycle(execute : (ExtraStageControl) => Unit) : Unit = {
    extraStages += new ExtraStage(execute, extraStageNum)
    state := extraStageNum.U
    extraStageNum += 1
  }

  def stageActive(offset : Int) : Bool = {
    //TODO implement
    require(offset == 1)
    if(offset == 1)
      outgoingBubble === false.B
    else
      false.B
  }

  def instantiateExtraStages() : Unit = {
    val ctrl = new ExtraStageControl(state)
    for(extraStage <- extraStages) {
      when(state === extraStage.number.U) {
        extraStage.execute(ctrl)
      }
    }
  }
}
object Stage {
  def apply(execute : (PipelineControl) => Unit) = new Stage(execute)
}
class Stage(val execute : (PipelineControl) => Unit) extends LightModule {
  def apply(ctrl : PipelineControl) : Unit = {
    ctrl.outgoingBubble := true.B
    ctrl._incomingBubble match {
      case None => {
        execute(ctrl)
      }
      case Some(incomingBubble) => when(!incomingBubble) {
        execute(ctrl)
      }
    }
  }
}
object Pipeline {
  def apply(stages : Stage*) = new Pipeline(stages)
}
class Pipeline(stages : Seq[Stage]) extends LightModule {
  var bubbles = ListBuffer[Bool]()
  val state = RegInit(0.U)

  def apply() : Unit = {
    var bubble = RegInit(true.B)
    bubbles += bubble
    //correct width?
    val ctrl = new PipelineControl(state, bubble)

    for((stage, i) <- stages.zipWithIndex) {
      if(i != 0) {
        bubble = RegInit(true.B)
        bubbles += bubble
        ctrl._incomingBubble = Some(ctrl.outgoingBubble)
        ctrl.outgoingBubble = bubble
        ctrl.currentStageId = i
      }

      when(state === 0.U) {
        stage(ctrl)
      }
    }

    ctrl.instantiateExtraStages()

//    bubbles.zipWithIndex.foreach { _ match {
//      case (b, i) => printf(cf"b${i}=${b} ")
//    } }
//    printf(cf"\n")
  }

  def connect(ports : PipelinedPort*) : Unit = { }
}

object PipelineReg {
  def apply[T <: Data](firstStage : Int, lastStage : Int,
    t : T) = new PipelineReg[T](firstStage, lastStage, t)
}
class PipelineReg[T <: Data](val firstStage : Int, lastStage : Int, t : T) {
  val regs = Seq.fill(lastStage - firstStage + 1)(Reg(t))
  def apply(stageId : Int) : T = {
    require(stageId >= firstStage)
    regs(stageId - firstStage)
  }
  def advance(ctrl : PipelineControl) : Unit = {
    ctrl.next(this) := ctrl.prev(this)
  }
}

class PipelinedPort {
  def :=>(receiver : PipelinedPort) = this
  def ->(receiver : PipelinedPort) = this
}
class MemReadWriteChain[T <: Data](read : MemReadPort[T],
  write : MemWritePort[T],
  address : PipelineReg[T], writeData : T) {
  def forwardedOrReadWord = {
    val currentStageId = read.x + 1

    val numberOfProcStages = write.x - read.x - 1

    if(numberOfProcStages >= 1) {
      //TODO check bubbles
      val sameAddress = address(read.x) === address(write.x - 1)
      //printf(cf"sameAddress=${sameAddress} writeData=${writeData}\n")
      //TODO also use pipelinereg?
      Mux(sameAddress, writeData, read.readWord)
    } else
      read.readWord
  }
}
class MemReadPort[T <: Data](mem : MemWrapper2[T], val x : Int, address : UInt)
  extends PipelinedPort {
    //TODO test show warning when used without forwarding
  def readWord = mem.output
  mem.readAddress := address
}
class MemWritePort[T <: Data](mem : MemWrapper2[T], val x : Int, address : UInt)
  extends PipelinedPort {
  mem.writeAddress := address
  mem.input := DontCare
  def write(value : T) : Unit = {
    mem.input := value
    mem.writeEnable := true.B
  }
}
class StreamingInputPort[T <: Data](val in : DecoupledIO[T])
  extends PipelinedPort {

  in.nodeq()

  def read()(accept : => Unit) : Unit = {
    in.deq()
    when(in.fire) {
      accept
    }
  }
}
class StreamingOutputPort[T <: Data](val out : DecoupledIO[T])
  extends PipelinedPort {

  val outputValue = Reg(chiselTypeOf(out.bits))
  out.noenq()

  def write(ctrl : PipelineControl, value : T) : Unit = {
    out.enq(value)

    when(!out.fire) {
      outputValue := value
      ctrl.multicycle { ctrl2 =>
        out.enq(outputValue)
        when(out.fire) {
          ctrl2.goBack()
        }
      }
    }
  }
}

//  val stateCounter = Counter(stages.size)
//
//  def operate() : Unit = {
//    when(stateCounter.value < (stages.size - 1).U) {
//      stateCounter.inc()
//    }
//    for((stage, i) <- stages.zipWithIndex) {
//      when(stateCounter.value >= i.U) {
//        stage.execute()
//      }
//    }
//  }
//}

class ReadProcessWrite[I <: Data, O <: Data](
  input : DecoupledIO[I],
  output : DecoupledIO[O],
  process : I => O) {

  val readValue = Reg(chiselTypeOf(input.bits))
  val outputValue = Reg(chiselTypeOf(output.bits))
  val pipeline = Pipeline(
    Stage { ctrl =>
      input.deq()
      when(input.fire) {
        readValue := input.bits
        //printf(cf"readAccum=${layerCore.output.bits.result}\n")
        ctrl.passOn()
      }
    },
    Stage { ctrl =>
      outputValue := process(readValue)
      ctrl.passOn()
    },
    Stage { ctrl =>
      output.enq(outputValue)

      when(!output.fire) {
        ctrl.multicycle { ctrl2 =>
          output.enq(outputValue)
          when(output.fire) {
            ctrl2.goBack()
          }
        }
      }
    }
  )

  def apply() : Unit = {
    input.nodeq()
    output.noenq()
    pipeline()
  }
}

class ReadProcessWrite2[I <: Data, O <: Data](
  input : DecoupledIO[I],
  output : DecoupledIO[O],
  processStages : Seq[Stage],
  result : O) {

  val readValue = Reg(chiselTypeOf(input.bits))
  val outputValue = Reg(chiselTypeOf(output.bits))
  val stages = Seq(
    Stage { ctrl =>
      input.deq()
      when(input.fire) {
        processStages(0)(ctrl)
        //printf(cf"readAccum=${layerCore.output.bits.result}\n")
        ctrl.passOn()
      }
    }) ++
    processStages.tail ++ Seq(
    Stage { ctrl =>
      output.enq(result)

      when(!output.fire) {
        outputValue := result
        ctrl.multicycle { ctrl2 =>
          output.enq(outputValue)
          when(output.fire) {
            ctrl2.goBack()
          }
        }
      }
    }
  )

  val pipeline = new Pipeline(stages)

  def apply() : Unit = {
    input.nodeq()
    output.noenq()
    pipeline()
  }
}


package snn {
  class IOType {
    def ->(layer : Layer) : Layer = {
      layer.inputType = this
      layer
    }
  }

  object SpikeTrain extends IOType { }
  object EventQueue extends IOType { }
  object MemPots extends IOType { }
  object Activations extends IOType { }

  trait Topology {
    def toOptions : snnaccel.TopOptions
  }

  object Layer {
    def apply() = new Layer()
  }
  class Layer extends Topology {
    var inputType : IOType = null
    var outputType : IOType = null

    def ->(outputType : IOType) : Layer = {
      this.outputType = outputType
      this
    }

    override def toOptions = {
      val inputTypes = Seq(inputType, outputType)
      snnaccel.TopOptions(inputTypes = inputTypes)
    }
  }
}

}
