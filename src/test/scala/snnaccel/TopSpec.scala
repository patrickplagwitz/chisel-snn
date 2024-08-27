package snnaccel

import chisel3._
import chisel3.util.ReadyValidIO
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers._
//import chiseltest.treadle.WriteVcdAnnotation
//import ujson
      //println(ujson.write(ujson.Num(1)))
import snnaccel.dsl.snn

import scala.collection.mutable.ListBuffer

class NumberFormatter(val fractionalBits : Int) {
  def format(x : Double) : Int =
    (x * Math.pow(2, fractionalBits)).toInt
}
class TestFixture(val tester : ChiselScalatestTester,
  model : Option[ujson.Value] = None,
  useVerilator_ : Boolean = true) {
  val snnModel = model match {
    case Some(m) => m
    case _ => {
      val jsonModelFile = scala.io.Source.fromFile("snn-model.json")
      val jsonCode = try jsonModelFile.getLines() mkString "\n" finally
        jsonModelFile.close()
      ujson.read(jsonCode)
    }
  }
  val fixedPoint = new NumberFormatter(
    snnModel("data_type")("fractional_bits").num.toInt)
  var simulatedCycles : Long = 0

  val useVerilator = useVerilator_

  var topology = None : Option[snn.Topology]

  def use(topology : snn.Topology) : Unit = {
    this.topology = Some(topology)
  }

  def toFixedPoint(x : Double) : Int = fixedPoint.format(x)

  def makeDut(initialOptions : TopOptions) = {
    var options = initialOptions
    options = options.copy(useVerilator = useVerilator)
    topology match {
      case Some(topology) =>
        val topologyOptions = topology.toOptions
        options = options.copy(
          inputTypes = topologyOptions.inputTypes)
      case None =>
    }
    Top.generateAccelerator(snnModel, options)
  }

  def getAnnotations = {
    var ret = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
    if(!useVerilator)
      ret = Seq(WriteVcdAnnotation)
    ret
  }

  def testExpectedFromInput(input : Seq[TopInputBundle],
    expectedValues : Seq[InputBundle],
    options : TopOptions) : Unit = {
    tester.test(this.makeDut(options)).withAnnotations(getAnnotations) { dut =>
      dut.clock.setTimeout(options.timeout)
      dut.input.initSource().setSourceClock(dut.clock)
      dut.output.initSink().setSinkClock(dut.clock)

      fork {
        val (input1, input2) = input.splitAt(input.size / 3)
        dut.input.enqueueSeq(input1)
        dut.clock.step(15)
        dut.input.enqueueSeq(input2)
      }.fork {
        //2 should be(5)

        val (output1, output2) = expectedValues.splitAt(expectedValues.size / 2)
        dut.output.expectDequeueSeq(output1)
        dut.clock.step(10)
        dut.output.expectDequeueSeq(output2)
        //dut.output.expectDequeueSeq(expectedValues)
      }.join()

      simulatedCycles = dut.clock.getStepCount
    }
  }

  def getFromInput(input : Seq[InputBundle],
    options : TopOptions, numOfOutputs : Int) : Seq[Bundle] = {
    var rets = ListBuffer[Bundle]()

    tester.test(this.makeDut(options)).withAnnotations(getAnnotations) { dut =>
      dut.clock.setTimeout(options.timeout)
      dut.input.initSource().setSourceClock(dut.clock)
      dut.output.initSink().setSinkClock(dut.clock)

      fork {
        val (input1, input2) = input.splitAt(input.size / 3)
        dut.input.enqueueSeq(input1)
        dut.clock.step(15)
        dut.input.enqueueSeq(input2)
      }.fork {
        val len1 = numOfOutputs / 2
        val len2 = numOfOutputs - len1
        for(_ <- 0 until len1)
          rets += dequeue(dut.clock, dut.output)
        dut.clock.step(10)
        for(_ <- 0 until len2)
          rets += dequeue(dut.clock, dut.output)
      }.join()

      simulatedCycles = dut.clock.getStepCount
    }

    return rets.toSeq
  }

  def dequeue[T >: Null <: Data](clock : Clock, interface : ReadyValidIO[T]) : T = {
    var ret : T = null
    interface.ready.poke(true)
    fork
      .withRegion(Monitor) {
        while (!interface.valid.peekBoolean()) {
          clock.step(1)
        }
        interface.valid.expect(true.B)
        ret = interface.bits.peek()
      }
      .joinAndStep(clock)
    interface.ready.poke(false)
    ret
  }

//  def numbersToInputs(expected : Seq[Int]) : Seq[Bundle] = {
//    val input = this.snnModel("inputs")(0)(0).arr.map(x => 
//        new TopInputBundle(32).Lit(_.value -> x.num.toInt.S)).toSeq
//  }

  def testExpectedBundles(expectedValues : Seq[InputBundle],
    options : TopOptions) : Unit = {
    val input = this.snnModel("inputs")(0)(0).arr.map(x => 
        new TopInputBundle(32).Lit(_.value -> x.num.toInt.S)).toSeq
    testExpectedFromInput(input, expectedValues, options)
  }

  def testExpected(expectedValues : Seq[Int],
    options : TopOptions = TopOptions()) : Unit = {
    val expected = expectedValues.map(x => new TopInputBundle(32).Lit(
      _.value -> x.toInt.S)).toSeq
    testExpectedBundles(expected, options)
  }
}

class TopSpec extends AnyFreeSpec with ChiselScalatestTester 
  with should.Matchers {
  "Top should return all matrix-vector results of first layer sequentially" in {
    val fixture = new TestFixture(this)
    val expected = fixture.snnModel("debug_info_fixed")("spikes")(0).arr.take(20).map(
      x => fixture.toFixedPoint(x.num)).toSeq
    //println(expected)
    fixture.testExpected(expected)
    println(fixture.simulatedCycles)
  }

  "Top should be able to correctly process all neurons fully in parallel" in {
    val fixture = new TestFixture(this)
    val expected = fixture.snnModel("debug_info_fixed")("spikes")(0).arr.map(
      x => fixture.toFixedPoint(x.num)).toSeq
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(100))
    fixture.testExpected(expected, options)
    fixture.simulatedCycles.toInt should be <= 784 + 100 + 50
  }

  "Top should be able to correctly process neurons in tiles of 20" in {
    val fixture = new TestFixture(this)
    val expected = fixture.snnModel("debug_info_fixed")("spikes")(0).arr.map(
      x => fixture.toFixedPoint(x.num)).toSeq
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(5))
    fixture.testExpected(expected, options)
//    println(".....\n")
//    val stderrFile = scala.io.Source.fromFile("stderr")
//    val stderr = try stderrFile.getLines() mkString "\n"
//      finally stderrFile.close()
//    println(stderr).length)
    fixture.simulatedCycles.toInt should be <= 20 * (784 + 5 + 2) + 20
  }

  "Top should be able to correctly produce spikes of first layer" in {
    val fixture = new TestFixture(this)
    val expected = fixture.snnModel("debug_info_fixed")("spikes")(1).arr.map(
      x => new Spike().Lit(_.value -> x.num.toInt.B)).toSeq
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(5), outputSpikes = true)
    fixture.testExpectedBundles(expected, options)
  }

  "Top should be able to correctly produce matrix-vector results up to layer 3" in {
    val fixture = new TestFixture(this)
    val expected = fixture.snnModel("debug_info_fixed")("spikes")(4).arr.map(
      x => fixture.toFixedPoint(x.num)).toSeq
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(5, 5, 5), upToLayer = 3,
      timeout=1000000)
    fixture.testExpected(expected, options)
  }

  "First layer should produce correct results repeatedly" in {
    val fixture = new TestFixture(this)
    //TODO refactor input duplication
    //TODO fix with steps = 10
    val steps = 4
    val inputValues = fixture.snnModel("inputs")(0)(0).arr.map(
      x => x.num.toInt).toSeq
    val inputs = (0 until steps).flatMap(_ => inputValues).map(x =>
        new TopInputBundle(32).Lit(_.value -> x.S)).toSeq
    val expected = (0 until steps).flatMap { i => 
      fixture.snnModel("debug_info_fixed")("mems")(i * 3).arr.map(
        x => fixture.toFixedPoint(x.num)).toSeq
    }
    //println(expected(161), expected(261), expected(361), expected(461), expected(561), expected(661), expected(761))
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(5), outputSpikes = false)
    //TODO refactor extraction
    val actual = fixture.getFromInput(inputs, options, expected.size)
    val actualValues = actual.map(_.asInstanceOf[TopInputBundle].value.litValue)
    actualValues should be(expected)
  }

  "Top should be able to correctly produce final membrane potentials" in {
    val fixture = new TestFixture(this)
    val steps = 3
    val inputValues = fixture.snnModel("inputs")(0)(0).arr.map(
      x => x.num.toInt).toSeq
    val inputs = (0 until steps).flatMap(_ => inputValues).map(x =>
        new TopInputBundle(32).Lit(_.value -> x.S)).toSeq

//    fixture.snnModel("debug_info_fixed")("spikes").arr.foreach(x =>
//        println(x.arr.size))

//    (0 until 20).foreach { i =>
//      fixture.snnModel("debug_info_fixed")("mems")(i).arr.foreach { mem =>
//        println(i, fixture.toFixedPoint(mem.num))
//      }
//    }
//
//    (0 until 2).foreach { i =>
//      fixture.snnModel("debug_info_fixed")("spikes")(i*6).arr.foreach { mem =>
//        println(i, fixture.toFixedPoint(mem.num))
//      }
//    }
//    (0 until 2).foreach { i =>
//      fixture.snnModel("debug_info_fixed")("spikes")(3+i*6).arr.foreach { mem =>
//        println(i, fixture.toFixedPoint(mem.num))
//      }
//    }

    var options = TopOptions()
    options = options.copy(neuronCores = Seq(5, 5, 5), upToLayer = 3,
      timeout = 1000000)// inputTypes = Seq(snn.Activations, snn.EventQueue, snn.EventQueue, snn.MemPots))
    val actual = fixture.getFromInput(inputs, options, 10 * steps)

    val expected = (0 until steps).flatMap(i => fixture.snnModel(
      "debug_info_fixed")("mems")(2 + (i * 3)).arr.map(
        x => fixture.toFixedPoint(x.num))).toSeq

    val actualValues = actual.map(_.asInstanceOf[TopInputBundle].value.litValue)
    println(fixture.simulatedCycles)
    actualValues.take(10 * steps) should be(expected)
  }

}
