package snnaccel

import dsl._

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers._
import chisel3.util.{Decoupled}

class DslTest extends AnyFreeSpec with ChiselScalatestTester 
  with should.Matchers {
  "While true should run forever" in {
    test {
      class TestModule extends Module {
        val out = IO(UInt())
        val i = RegInit(0.U(10.W))
        out := i

        While(true.B) {
          Stmt { i := i + 1.U }
        }()
      }

      new TestModule()
    } { dut =>
      //c.io.in.poke(0.U)
      //println("Last output value :" + c.io.out.peek().litValue)
      dut.out.expect(0.U)
      dut.clock.step()
      dut.out.expect(1.U)
      dut.clock.step()
      dut.out.expect(2.U)
    }
  }
}
