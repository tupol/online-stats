package org.tupol.stats

import org.apache.commons.math3.stat._
import org.apache.commons.math3.stat.descriptive.moment.{ Kurtosis, Skewness, StandardDeviation, Variance }
import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class StatsOpsSpec extends FunSuite with Matchers {

  test("zero |+| zero = zero") {
    Stats.Nil |+| Stats.Nil shouldBe Stats.Nil
  }

  test("stats |+| zero = stats") {
    Stats.fromDouble(1) |+| Stats.Nil shouldBe Stats.fromDouble(1)
    Stats.fromDoubles(Seq(1.0, 2, 3)) |+| Stats.Nil shouldBe Stats.fromDoubles(Seq(1.0, 2, 3))
  }

  test("stats1 |+| stats2 = stats2 |+| stats1") {
    Stats.fromDoubles(Seq(1.0, 2)) |+| Stats.fromDoubles(Seq(3.0, 4)) shouldBe Stats.fromDoubles(Seq(3.0, 4)) |+| Stats.fromDoubles(Seq(1.0, 2))
    Stats.fromDoubles(Seq(1.0, 2)) |+| Stats.fromDoubles(Seq(3.0, 4)) shouldBe Stats.fromDoubles(Seq(1.0, 2, 3, 4))

    Stats.fromDoubles(Seq(1.0, 2)) |+| Stats.fromDoubles(Seq(3.0, 4)) shouldBe Stats.fromDoubles(Seq(3.0, 4)) |+| Stats.fromDoubles(Seq(1.0, 2))
    Stats.fromDoubles(Seq(1.0, 2)) |+| Stats.fromDoubles(Seq(3.0, 4)) shouldBe Stats.fromDoubles(Seq(1.0, 2, 3, 4))
  }

  test("variance precision check") {

    val testSeqNormalNumbers = Seq(4.0, 7.0, 13.0, 16.0)
    val testSeqLargeNumbers = testSeqNormalNumbers.map(_ + 1E+12)

    val testSeqSmallNumbers = testSeqNormalNumbers.map(_ / 1E+12)

    Stats.fromDoubles(testSeqNormalNumbers).variance shouldBe 22.5
    Stats.fromDoubles(testSeqNormalNumbers).varianceBC shouldBe 30.0

    Stats.fromDoubles(testSeqLargeNumbers).variance shouldBe 22.5
    Stats.fromDoubles(testSeqLargeNumbers).varianceBC shouldBe 30.0

    Stats.fromDoubles(testSeqSmallNumbers).variance shouldBe new Variance(false).evaluate(testSeqSmallNumbers.toArray)
    Stats.fromDoubles(testSeqSmallNumbers).varianceBC shouldBe new Variance(true).evaluate(testSeqSmallNumbers.toArray)
  }

  test("test against reference implementation") {

    val toleranceDefault = 1E-9
    val toleranceLenient = 1E-2

    val randomizer = new Random(7773)
    val testData = (0 to 100).map(_ => randomizer.nextDouble() * 1000)

    val statsComposed = testData.foldLeft(Stats.Nil)((result, input) => result |+| input)
    val statsFromDoubles = Stats.fromDoubles(testData)

    statsComposed.count shouldBe statsFromDoubles.count
    statsComposed.sum shouldBe testData.sum +- toleranceDefault
    statsComposed.min shouldBe statsFromDoubles.min
    statsComposed.max shouldBe statsFromDoubles.max
    statsComposed.avg shouldBe statsFromDoubles.avg +- toleranceDefault
    statsComposed.average shouldBe statsComposed.avg
    statsComposed.mean shouldBe statsComposed.avg
    statsComposed.variance() shouldBe statsFromDoubles.variance() +- toleranceDefault
    statsComposed.skewness shouldBe statsFromDoubles.skewness +- toleranceDefault
    statsComposed.kurtosis shouldBe statsFromDoubles.kurtosis +- toleranceDefault

    statsComposed.count shouldBe testData.size
    statsComposed.min shouldBe StatUtils.min(testData.toArray)
    statsComposed.max shouldBe StatUtils.max(testData.toArray)
    statsComposed.avg shouldBe StatUtils.mean(testData.toArray) +- toleranceDefault
    statsComposed.variance shouldBe new Variance(false).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.varianceBC shouldBe new Variance(true).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.stdev shouldBe new StandardDeviation(false).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.stdevBC shouldBe new StandardDeviation(true).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.skewness shouldBe new Skewness().evaluate(testData.toArray) +- toleranceLenient
    statsComposed.kurtosis shouldBe new Kurtosis().evaluate(testData.toArray) +- toleranceLenient
  }

}
