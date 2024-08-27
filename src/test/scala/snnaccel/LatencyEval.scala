package snnaccel

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers._
import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Paths, Files}
import snnaccel.dsl.snn

class LatencyEval extends AnyFreeSpec with ChiselScalatestTester 
  with should.Matchers {
//  "Evaluate latencies" in {
//    for(point <- GeneratePoints()) {
//      val dir = "DSE/" + point.name
//      val outputFile = dir + "/cycles"
//      if(Files.exists(Paths.get(dir)) && !Files.exists(Paths.get(outputFile))) {
//        println("Running", point.name)
//        val fixture = new TestFixture(this, Some(point.model))
//        val steps = 1//fixture.snnModel("steps").num.toInt
//        val inputValues = fixture.snnModel("inputs")(0)(0).arr.map(
//          x => x.num.toInt).toSeq
//        val inputs = (0 until steps).flatMap(_ => inputValues).map(x =>
//            new TopInputBundle(32).Lit(_.value -> x.S)).toSeq
//        val options = point.options.copy(timeout = 0)
//
//        fixture.getFromInput(inputs, options, 10 * steps)
//
//        println("Got", fixture.simulatedCycles, "for", point.name)
//
//        val writer = new BufferedWriter(new FileWriter(outputFile))
//        try {
//          writer.write(fixture.simulatedCycles.toString)
//        } finally {
//          writer.close()
//        }
//      }
//    }
//  }

  "Evaluate single latency" in {
    val fixture = new TestFixture(this)
    val steps = 20
    val inputValues = fixture.snnModel("inputs")(0)(0).arr.map(
      x => x.num.toInt).toSeq
    val inputs = (0 until steps).flatMap(_ => inputValues).map(x =>
        new TopInputBundle(32).Lit(_.value -> x.S)).toSeq

    var options = TopOptions()
    options = options.copy(neuronCores = Seq(100, 100, 10), upToLayer = 3,
      outputSpikes = true,
      useVerilator = true,
      inputTypes = Seq(snn.Activations, snn.EventQueue, snn.EventQueue, snn.SpikeTrain))
    fixture.getFromInput(inputs, options, 10 * steps)
    println("Got", fixture.simulatedCycles)
  }
}
