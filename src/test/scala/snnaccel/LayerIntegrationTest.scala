package snnaccel

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers._
import scala.collection.mutable.ListBuffer
import snnaccel.dsl.snn._

case class LayerConfig(weights : Seq[Seq[Double]], biases : Seq[Double]) { }

object MakeModel {
  def apply(numberFormat : NumberFormatter, layers : LayerConfig*) = {
    var jsonLayers = ListBuffer[ujson.Obj]()
    for(layer <- layers) {
      val jsonWeights = layer.weights.transpose.map(_.map(x =>
          ujson.Num(numberFormat.format(x)))).map(xs => xs.toBuffer).toBuffer
      val jsonBiases = layer.biases.map(x => ujson.Num(
        numberFormat.format(x))).toBuffer
      jsonLayers += ujson.Obj(
        "beta" -> numberFormat.format(1),
        "weights" -> jsonWeights,
        "bias" -> jsonBiases)
    }

    ujson.Obj(
      "data_type" -> ujson.Obj("fractional_bits" ->
        numberFormat.fractionalBits),
      "layers" -> jsonLayers.toBuffer
    )
  }
}

class LayerIntegrationTest extends AnyFreeSpec with ChiselScalatestTester 
  with should.Matchers {
  "Top should be able to produce spikes of layer" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat, LayerConfig(Seq(
      Seq(0.4, 0.0),
      Seq(0.3, 0.42),
      Seq(0.3, 0.1)),
      Seq(0, 0, 0.2)))

    val fixture = new TestFixture(this, Some(model), false)

    //TODO refactor: fixture.toSpikes
    val input = Seq(2, 1).map(x => new TopInputBundle(32).Lit(
        _.value -> numberFormat.format(x).S)).toSeq
    val expected = Seq(0, 1, 0).map(
      x => new Spike().Lit(_.value -> x.B)).toSeq
    var options = TopOptions()
    options = options.copy(outputSpikes = true)
    //TODO 
    //1. testExpectedFromInput
    //2. find test values
    //3. test spikes
    //4. also test accums
    //
    //NOW:
    //1. test accums
    //2. below
    //fixture.testExpectedFromInput(input, expected, options)

    val expectedAccums = Seq(0.8, 1.02, 0.9).map(numberFormat.format(_))
    options = options.copy(outputSpikes = false)
//    fixture.testExpectedFromInput(input, expectedAccums, options)

    val actual = fixture.getFromInput(input, options, 3)
    for(i <- 0 until actual.size) {
      actual(i).asInstanceOf[TopInputBundle].value.litValue.toInt should be (
        expectedAccums(i) +- 1)
    }
  }

  "Top should pass spikes from layer to layer" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat,
      LayerConfig(Seq(
        Seq(0.4),
        Seq(1.3),
        Seq(0.7)),

        Seq(0, 0, 0)
      ),
      LayerConfig(Seq(
        Seq(5, 0.6, 3),
        Seq(5, 0.8, 1)),

        Seq(0.5, 0.0)
        ))

    val input = Seq(1).map(x => new TopInputBundle(32).Lit(
        _.value -> numberFormat.format(x).S)).toSeq
    val expected = Seq(1, 0).map(
      x => new Spike().Lit(_.value -> x.B)).toSeq

    val fixture = new TestFixture(this, Some(model), false)
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(1, 1), upToLayer = 2,  outputSpikes = true)
    fixture.testExpectedFromInput(input, expected, options)
  }

  "Top should keep membrane potential per neuron" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat,
      LayerConfig(Seq(
        Seq(0.7)),

        Seq(0, 0)
      ))

    val expectedSpikes = Seq(0, 0, 1, 1, 0).map( 
      x => new Spike().Lit(_.value -> x.B)).toSeq

    val fixture = new TestFixture(this, Some(model), false)

    val input = Seq(1, 0, 1, 1, 0).map(x => new TopInputBundle(32).Lit(
        _.value -> numberFormat.format(x).S)).toSeq
    var options = TopOptions()
    options = options.copy(outputSpikes = true)

    fixture.testExpectedFromInput(input, expectedSpikes, options)
  }
  
  "Top should continuously stimulate neurons until inputs stop" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat,
      LayerConfig(Seq(
        Seq(0.7)),

        Seq(0, 0)
      ),
      LayerConfig(Seq(
        Seq(0.6),
        Seq(0.8)),

        Seq(0, 0.3)
        ))

    val expectedAccumValues = Seq(
      Seq(0, 0.3), //0.7
      Seq(0, 0.6), //0.7
      Seq(0.6, 1.7), //1.4
      Seq(1.2, 1.8)) //1.1

    val fixture = new TestFixture(this, Some(model), false)

    val input = Seq(1, 0, 1, 1).map(x => new TopInputBundle(32).Lit(
        _.value -> numberFormat.format(x).S)).toSeq
    var options = TopOptions()
    options = options.copy(neuronCores = Seq(1, 1), upToLayer = 2, outputSpikes = false)

    val expectedAccums = expectedAccumValues.flatten.map(
      numberFormat.format(_))

    val actual = fixture.getFromInput(input, options, 8)
    for(i <- 0 until actual.size) {
      actual(i).asInstanceOf[TopInputBundle].value.litValue.toInt should be (
        expectedAccums(i) +- 40)
    }
  }

//  "" in {
//    SpikeTrain
//    Activations
//    EventQueue
//    MemPots (only output)
//    bit width adapted automatically
//
//    foo
//    SpikeTrain -> ConvertToEventQueue -> Layer.withCores(100).forNetworkLayers(0, 1, 2, 3)
//
//    Activations -> Layer.withCores(5).forNetworkLayers(0, 1) -> EventQueue
//
//    Activations -> Layer().withCores(5) -> SpikeTrain ->
//      Layer.withCores(5) -> SpikeTrain ->
//      Layer.withCores(5)
//  }

  "Layer should be able to convert spikes outputs to event queues" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat,
      LayerConfig(Seq(
        Seq(0.4),
        Seq(1.2),
        Seq(1.3),
        Seq(0.55)),

        Seq(0, 0, 0, 0)
      ))

    val input = Seq(0, 1, 1, 1)
    val endOfQueue = 1
    val valid = 1
    val expectedQueues = Seq(
      (0, 0, endOfQueue),
      (1, 1, 0), (2, 1, 0), (0, 0, endOfQueue),
      (1, 1, 0), (2, 1, 0), (3, 1, endOfQueue),
      (0, 1, 0), (1, 1, 0), (2, 1, 0), (0, 0, endOfQueue))

    val topology = SpikeTrain -> Layer() -> EventQueue

    val fixture = new TestFixture(this, Some(model), false)

    fixture.use(topology)

    val inputBundles = input.map { x => 
      val value = if(x == 0) false.B else true.B
      new Spike().Lit(_.value -> value) }.toSeq

    val actual = fixture.getFromInput(inputBundles, TopOptions(),
      expectedQueues.size)
    for(i <- 0 until actual.size) {
      val bundle = actual(i).asInstanceOf[EventQueueBundle]
      if(expectedQueues(i)._2 != 0)
        bundle.value.litValue.toInt should be (expectedQueues(i)._1)
      bundle.valid.litValue.toInt should be (expectedQueues(i)._2)
      bundle.eoq.litValue.toInt should be (expectedQueues(i)._3)
    }
  }

  "Layer should be able to accept event queues as input" in {
    val numberFormat = new NumberFormatter(14)
    val model = MakeModel(numberFormat,
      LayerConfig(Seq(
        Seq(0.1, 0.2, 0.3),
        Seq(0.2, 0.4, 0.6)),

        Seq(0, 0)
      ))

    val input = Seq(
      (1, 1, 0), (2, 0, 1), //0 1 0
      (0, 1, 0), (2, 1, 1)) //1 0 1
    val expectedAccums = Seq(
      0.2, 0.4,
      0.6, 1.2).map(numberFormat.format(_)).toSeq

    val topology = EventQueue -> Layer() -> MemPots

    val fixture = new TestFixture(this, Some(model), false)

    fixture.use(topology)

    val inputBundles = input.map { x => 
      new EventQueueBundle(32).Lit(
        _.value -> x._1.U,
        _.valid -> x._2.B,
        _.eoq -> x._3.B) }.toSeq

    val actual = fixture.getFromInput(inputBundles, TopOptions(),
      expectedAccums.size)
    for(i <- 0 until actual.size) {
      val bundle = actual(i).asInstanceOf[TopInputBundle]
      bundle.value.litValue.toInt should be(expectedAccums(i) +- 1)
    }
  }

//  //TODO delete this, just a try
//  "Top should continuously stimulate neurons until inputs stop 2" in {
//    val numberFormat = new NumberFormatter(14)
//    val model = MakeModel(numberFormat,
//      Layer(Seq(
//        Seq(0.4, 0.7),
//        Seq(0.7, 0.1),
//        Seq(1.2, 2.0)),
//
//        Seq(0, 0)
//      ),
//        )
//
//    val expectedAccumValues = Seq(
//      Seq(0.7, 0.1, 2.0),
//      Seq(1.8, 0.9, 4.2))
//
//    val fixture = new TestFixture(this, Some(model), false)
//
//    val input = Seq(0, 1,  1, 1).map(x => new TopInputBundle(32).Lit(
//        _.value -> numberFormat.format(x).S)).toSeq
//    var options = TopOptions()
//    options = options.copy(neuronCores = 1, outputSpikes = false)
//
//    val expectedAccums = expectedAccumValues.flatten.map(
//      numberFormat.format(_))
//
//    val actual = fixture.getFromInput(input, options, expectedAccums.size)
//    for(i <- 0 until actual.size) {
//      actual(i).asInstanceOf[TopInputBundle].value.litValue.toInt should be (
//        expectedAccums(i) +- 2)
//    }
//  }
}
