package snnaccel
import circt.stage.ChiselStage
import java.io.{BufferedWriter, FileWriter}
import scala.sys.process._
import snnaccel.dsl.snn
import java.nio.file.{Paths, Files}

import chiseltest.ChiselScalatestTester

case class DesignPoint(
  title : String,
  model : ujson.Value, 
  numCores : Seq[Int]) {
  def name = {
    title + "_" + numCores.mkString("_")
  }
  def options = TopOptions(
    useVerilator = true,
    upToLayer = 100,
    neuronCores = numCores,
    inputTypes = Seq(snn.Activations, snn.EventQueue, snn.EventQueue, snn.SpikeTrain),
    outputSpikes = true
    )
}

object GeneratePoints {
  def apply() = {
    val jsonModelFile = scala.io.Source.fromFile("snn-model.json")
    val jsonCode = try jsonModelFile.getLines() mkString "\n" finally
      jsonModelFile.close()
    val snnModel = ujson.read(jsonCode)

//    Seq(1, 128, 256, 512).flatMap { i =>
//      Seq(1, 128, 256).flatMap { j =>
//        Seq(1, 128).flatMap { k =>
//        Seq(1, 10).map { l =>
//          DesignPoint("mnist", snnModel, Seq(i, j, k, l))
//    }}}}.toSeq

    Seq(100, 1, 25, 50).flatMap { i =>
      Seq(100, 1, 10, 25).flatMap { j =>
        Seq(10, 1).map { k =>
          DesignPoint("mnist", snnModel, Seq(i, j, k))
    }}}.toSeq

    Seq(DesignPoint("mnist", snnModel, Seq(1, 1, 1)))

//    Seq(50).flatMap { i =>
//      Seq(1).flatMap { j =>
//        Seq(2).map { k =>
//          DesignPoint("mnist", snnModel, Seq(i, j, k))
//    }}}.toSeq
  }
}

object Program {
  def synthesize(point : DesignPoint) : Unit = {
    val dir = "DSE/" + point.name
    if(Files.exists(Paths.get(dir)))
      return
    Seq("mkdir", dir).!
    Seq("mkdir", "-p", dir + "/src").!

    val writer = new BufferedWriter(new FileWriter(
      dir + "/src/Top.sv"))
    try {
      val code = ChiselStage.emitSystemVerilog(Top.generateAccelerator(
        point.model, point.options))
      code.split("\n").foreach { line =>
        val removeLine = (line contains "ifndef SYNTHESIS") ||
          (line contains "not def SYNTHESIS")
        if(!removeLine)
          writer.write(line + "\n")
      }
    } finally {
      writer.close()
    }

    Seq("python", "vivado/project.py", dir, point.name).!
  }

  def main(args : Array[String]) : Unit = {

//xczu9eg-ffvb1156-2-e
//xc7z020clg484-1

    for(point <- GeneratePoints()) {
      synthesize(point)
    }
  }
}
