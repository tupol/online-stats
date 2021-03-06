package org.tupol.stats

import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class EWeightedStatsOpsSpec extends FunSuite with Matchers {

  val alpha = 0.1

  test("average precision check for large numbers") {

    val testSeqRefNumbers = Seq(4.0, 7.0, 13.0, 16.0)
    val testSeqNumbers = testSeqRefNumbers.map(_ + 1E+12)
    val expectedAverage = testSeqNumbers.sum / testSeqNumbers.size
    val toleranceDefault = 0.1 * expectedAverage // 0.1%

    EWeightedStats.fromDoubles(alpha, testSeqNumbers).avg shouldBe expectedAverage +- toleranceDefault
    EWeightedStats.fromDoubles(alpha, testSeqNumbers).avg shouldBe expectedAverage +- toleranceDefault

  }

  test("average precision check for unitary (naturally small) numbers numbers") {

    val testSeqNumbers = Seq(4.0, 7.0, 13.0, 16.0)
    val expectedAverage = testSeqNumbers.sum / testSeqNumbers.size
    val toleranceDefault = 0.5 * expectedAverage // 0.1%

    EWeightedStats.fromDoubles(alpha, testSeqNumbers).avg shouldBe expectedAverage +- toleranceDefault
    EWeightedStats.fromDoubles(alpha, testSeqNumbers).avg shouldBe expectedAverage +- toleranceDefault

  }

  test("average precision check for very small numbers") {

    val testSeqRefNumbers = Seq(4.0, 7.0, 13.0, 16.0)
    val testData = testSeqRefNumbers.map(_ / 1E+12)
    val expectedAverage = testData.sum / testData.size
    val toleranceDefault = 0.5 * expectedAverage // 0.1%

    EWeightedStats.fromDoubles(alpha, testData).avg shouldBe toleranceDefault +- toleranceDefault
    EWeightedStats.fromDoubles(alpha, testData).avg shouldBe toleranceDefault +- toleranceDefault

  }

  test("test constructor vs composition") {

    val MaxValue = 1000
    val randomizer = new Random(7773)
    val testData = (0 to 100).map(_ => randomizer.nextDouble() * MaxValue)

    val statsComposed = testData.tail.foldLeft(EWeightedStats.fromDouble(alpha, testData.head))((result, input) => result |+| input)
    val statsFromDoubles = EWeightedStats.fromDoubles(alpha, testData)

    statsComposed.count shouldBe statsFromDoubles.count
    statsComposed.min shouldBe statsFromDoubles.min
    statsComposed.max shouldBe statsFromDoubles.max
    statsComposed.avg shouldBe statsFromDoubles.avg
    statsComposed.average shouldBe statsFromDoubles.avg
    statsComposed.mean shouldBe statsFromDoubles.avg
    statsComposed.variance() shouldBe statsFromDoubles.variance()
    statsComposed.skewness shouldBe statsFromDoubles.skewness
    statsComposed.kurtosis shouldBe statsFromDoubles.kurtosis

  }

}
