package snnaccel

import dsl._

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers._
import chisel3.util.{Decoupled, DecoupledIO}

case class PipelineTestFixture(val tester : ChiselScalatestTester) {
  def test[T <: chisel3.Module](input : Seq[Int], expected : Seq[Int],
    instantiate : (DecoupledIO[UInt], DecoupledIO[UInt]) => Unit) : Unit = {
    tester.test {
      class TestModule extends Module {
        val out = IO(Decoupled(UInt(4.W)))
        val in = IO(Flipped(Decoupled(UInt(4.W))))
        instantiate(in, out)
      }
      new TestModule()
    }.withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.in.initSource().setSourceClock(dut.clock)
      dut.out.initSink().setSinkClock(dut.clock)
      fork {
        dut.in.enqueueSeq(input.map(_.U))
      }.fork {
        dut.out.expectDequeueSeq(expected.map(_.U))
      }.join()
    }
  }
}

class PipelineTest extends AnyFreeSpec with ChiselScalatestTester 
  with should.Matchers {
  "A streaming pipeline should be able to read and update memory words" in {
    val fixture = PipelineTestFixture(this)
    fixture.test(
      Seq(1, 1, 2, 1),
      Seq(5, 6, 5, 7), { (in, out) =>
        val mem = new MemWrapper2(3, chiselTypeOf(out.bits))
        loadMemoryFromSeq(mem.mem, (0 until mem.size).map(_ => 5.U),
          MemoryFileFormat.Treadle)
        mem.readAddress := in.bits
        val address = Seq.fill(2)(Reg(chiselTypeOf(mem.readAddress)))
        mem.writeAddress := address(0)

        //TODO in Treadle simulation, address 0 is written before clock starts
        val newWord = Reg(chiselTypeOf(out.bits))
        val newWordValue = Wire(chiselTypeOf(out.bits))
        newWordValue := DontCare
        val oldWord = Reg(chiselTypeOf(out.bits))
        mem.input := newWordValue

        new ReadProcessWrite2(in, out, Seq(
          Stage { ctrl =>
            address(0) := in.bits
          },
          Stage { ctrl =>
            //printf(cf"active=${ctrl.stageActive(+1)}\n")
            //printf(cf"bubble=${ctrl.outgoingBubble}\n")
            val oldWordValue = Mux(address(0) === address(1) && ctrl.stageActive(+1),
              newWord, mem.output)
            address(1) := address(0)
            newWordValue := oldWordValue + 1.U
            newWord := newWordValue
            oldWord := oldWordValue
            mem.writeEnable := true.B
            ctrl.passOn()
          }),
          oldWord)()
    })
  }

  "A two-stage R-P-W pipeline should be able to handle address pattern AA" in {
    val fixture = PipelineTestFixture(this)
    fixture.test(
      Seq(1, 2, 3, 1, 2, 3, 1, 1),
      Seq(5, 5, 5, 6, 6, 6, 7, 8), { (in, out) =>
        val mem = new MemWrapper2(5, chiselTypeOf(out.bits))
        ConstInit(mem, 5.U, MemoryFileFormat.Treadle)
        val address = Reg(chiselTypeOf(in.bits))
        val output = Reg(chiselTypeOf(out.bits))

        val read = new MemReadPort(mem, 0, in.bits)
        val write = new MemWritePort(mem, 1, address)
        val inPort = new StreamingInputPort(in)
        val outPort = new StreamingOutputPort(out)
        inPort -> read :=> write
        val pipeline = Pipeline(
          Stage { ctrl =>
            inPort.read() {
              address := in.bits
              ctrl.passOn()
            }
          },
          Stage { ctrl => 
            write.write(read.readWord + 1.U)
            output := read.readWord
            ctrl.passOn()
          },
          Stage { ctrl =>
            outPort.write(ctrl, output)
          })
        pipeline.connect(read, write, inPort, outPort)
        pipeline()
      })
  }

//read.forwardedOrReadWord
//readWord
//unforwardedReadWord
//
//val writeStageId
//val readStageId
//val currentStageId
//assert currentStageId == readStageId + 1
//val numberOfProcStages = writeStageId - readStageId - 1
//
//if(numberOfProcStages >= 1) {
//  val sameAddress = address(writeStageId) == address(currentStageId)
//  Mux(sameAddress, word(writeStageId), readWord)
//}
  "A three-stage R-P-W pipeline should be able to handle address pattern AA" in {
    val fixture = PipelineTestFixture(this)
    fixture.test(
      Seq(1, 2, 3, 1, 2, 3, 1, 1),
      Seq(5, 5, 5, 6, 6, 6, 7, 8), { (in, out) =>
        val mem = new MemWrapper2(5, chiselTypeOf(out.bits))
        ConstInit(mem, 5.U, MemoryFileFormat.Treadle)
        //val address = Seq.fill(2)(Reg(chiselTypeOf(in.bits)))
        val address = PipelineReg(0, 1, chiselTypeOf(in.bits))
        val output = Reg(chiselTypeOf(out.bits))

        val read = new MemReadPort(mem, 0, in.bits)
        val write = new MemWritePort(mem, 2, address(1))
        val inPort = new StreamingInputPort(in)
        val outPort = new StreamingOutputPort(out)
        val newWord = Reg(chiselTypeOf(out.bits))
        val readWriteChain = new MemReadWriteChain(read, write, address, newWord)
        //TODO
        val oldWord = Reg(chiselTypeOf(out.bits))
        //inPort -> read :=> newWord :=> write
        val pipeline = Pipeline(
          Stage { ctrl =>
            inPort.read() {
              ctrl.next(address) := in.bits
              ctrl.passOn()
            }
          },
          Stage { ctrl => 
            newWord := readWriteChain.forwardedOrReadWord + 1.U
            address.advance(ctrl)
            oldWord := readWriteChain.forwardedOrReadWord
            ctrl.passOn()
          },
          Stage { ctrl => 
            write.write(newWord)
            output := oldWord
            ctrl.passOn()
          },
          Stage { ctrl =>
            outPort.write(ctrl, output)
          })
        pipeline.connect(read, write, inPort, outPort)
//        read.associate(pipeline.stages(0), in.bits)
//        write.associate(pipeline.stages(1), address(1))
        pipeline()
      })
  }

//  "A pipeline should be able to implement a RISC processor" in {
//    //pipeline registers
//  }
}

//      class TestModule extends Module {
//        val out = IO(Decoupled(UInt(4.W)))
//        val mem = new MemWrapper2(3, chiselTypeOf(out.bits))
//        loadMemoryFromSeq(mem.mem, (0 until mem.size).map(_ => 5.U),
//          MemoryFileFormat.Treadle)
//        val in = IO(Flipped(Decoupled(chiselTypeOf(mem.readAddress))))
//        mem.readAddress := in.bits
//        val address = PipelineReg(0 to 1, chiselTypeOf(mem.readAddress))
//        mem.writeAddress := address(0)
//
//        //TODO in Treadle simulation, address 0 is written before clock starts
//        val newWord = Reg(chiselTypeOf(out.bits))
//        val newWordValue = Wire(chiselTypeOf(out.bits))
//        newWordValue := DontCare
//        val oldWord = Reg(chiselTypeOf(out.bits))
//        mem.input := newWordValue
//
//        val read = PipelinedReadPort(mem, 0, in.bits)
//        val write = PipelinedWritePort(mem, 2, address)
//        read -> newWord -> write
//        //how far can process pipelines overlap?
//        //Q: At which points do two stages process the same element??
//        //invariant write address == read address?
//
//        new ReadProcessWrite2(in, out, Seq(
//          Stage { ctrl =>
//            //TODO is word already read here??
//            ctrl(address) := in.bits
//          },
//          Stage { ctrl =>
//            //printf(cf"active=${ctrl.stageActive(+1)}\n")
//            //printf(cf"bubble=${ctrl.outgoingBubble}\n")
//            val oldWordValue = Mux(address(0) === address(1) && ctrl.stageActive(+1),
//              newWord, read.result)
//            ctrl.passRegs(address)
//            newWordValue := oldWordValue + 1.U
//            newWord := newWordValue
//            oldWord := oldWordValue
//            mem.writeEnable := true.B
//            ctrl.passOn()
//          }),
//          oldWord)()
//      }
