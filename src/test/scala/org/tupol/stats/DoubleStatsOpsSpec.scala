package org.tupol.stats

import org.tupol.stats.StatsOps._
import org.apache.commons.math3.stat._
import org.apache.commons.math3.stat.descriptive.moment.{ Kurtosis, Skewness, StandardDeviation, Variance }
import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class DoubleStatsOpsSpec extends FunSuite with Matchers {

  test("zero |+| zero = zero") {
    DoubleStats.zeroDouble |+| DoubleStats.zeroDouble shouldBe DoubleStats.zeroDouble
  }

  test("stats |+| zero = stats") {
    DoubleStats.fromDoubles(1) |+| DoubleStats.zeroDouble shouldBe DoubleStats.fromDoubles(1)
    DoubleStats.fromDoubles(Seq(1.0, 2, 3)) |+| DoubleStats.zeroDouble shouldBe DoubleStats.fromDoubles(Seq(1.0, 2, 3))
  }

  test("stats1 |+| stats2 = stats2 |+| stats1") {
    DoubleStats.fromDoubles(Seq(1.0, 2)) |+| DoubleStats.fromDoubles(Seq(3.0, 4)) shouldBe DoubleStats.fromDoubles(Seq(3.0, 4)) |+| DoubleStats.fromDoubles(Seq(1.0, 2))
    DoubleStats.fromDoubles(Seq(1.0, 2)) |+| DoubleStats.fromDoubles(Seq(3.0, 4)) shouldBe DoubleStats.fromDoubles(Seq(1.0, 2, 3, 4))

    DoubleStats.fromDoubles(Seq(1.0, 2)) |+| DoubleStats.fromDoubles(Seq(3.0, 4)) shouldBe DoubleStats.fromDoubles(Seq(3.0, 4)) |+| DoubleStats.fromDoubles(Seq(1.0, 2))
    DoubleStats.fromDoubles(Seq(1.0, 2)) |+| DoubleStats.fromDoubles(Seq(3.0, 4)) shouldBe DoubleStats.fromDoubles(Seq(1.0, 2, 3, 4))
  }

  test("variance precision check") {

    val testSeqNormalNumbers = Seq(4.0, 7.0, 13.0, 16.0)
    val testSeqLargeNumbers = testSeqNormalNumbers.map(_ + 1E+12)

    val testSeqSmallNumbers = testSeqNormalNumbers.map(_ / 1E+12)

    DoubleStats.fromDoubles(testSeqNormalNumbers).variance(false) shouldBe 22.5
    DoubleStats.fromDoubles(testSeqNormalNumbers).variance(true) shouldBe 30.0

    DoubleStats.fromDoubles(testSeqLargeNumbers).variance(false) shouldBe 22.5
    DoubleStats.fromDoubles(testSeqLargeNumbers).variance(true) shouldBe 30.0

    DoubleStats.fromDoubles(testSeqSmallNumbers).variance(false) shouldBe new Variance(false).evaluate(testSeqSmallNumbers.toArray)
    DoubleStats.fromDoubles(testSeqSmallNumbers).variance(true) shouldBe new Variance(true).evaluate(testSeqSmallNumbers.toArray)
  }

  test("test against reference implementation") {

    val toleranceDefault = 1E-9
    val toleranceLenient = 1E-2

    val randomizer = new Random(7773)
    val testData = (0 to 100).map(_ => randomizer.nextDouble() * 1000)

    val statsComposed = testData.foldLeft(DoubleStats.zeroDouble)((result, input) => result |+| input)
    val statsFromDoubles = DoubleStats.fromDoubles(testData)

    statsComposed.count shouldBe statsFromDoubles.count
    statsComposed.min shouldBe statsFromDoubles.min
    statsComposed.max shouldBe statsFromDoubles.max
    statsComposed.avg shouldBe statsFromDoubles.avg +- toleranceDefault
    statsComposed.variance() shouldBe statsFromDoubles.variance() +- toleranceDefault
    statsComposed.skewness shouldBe statsFromDoubles.skewness +- toleranceDefault
    statsComposed.kurtosis shouldBe statsFromDoubles.kurtosis +- toleranceDefault

    statsComposed.count shouldBe testData.size
    statsComposed.min shouldBe StatUtils.min(testData.toArray)
    statsComposed.max shouldBe StatUtils.max(testData.toArray)
    statsComposed.avg shouldBe StatUtils.mean(testData.toArray) +- toleranceDefault
    statsComposed.variance(false) shouldBe new Variance(false).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.variance(true) shouldBe new Variance(true).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.stdev(false) shouldBe new StandardDeviation(false).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.stdev(true) shouldBe new StandardDeviation(true).evaluate(testData.toArray) +- toleranceLenient
    statsComposed.skewness shouldBe new Skewness().evaluate(testData.toArray) +- toleranceLenient
    statsComposed.kurtosis shouldBe new Kurtosis().evaluate(testData.toArray) +- toleranceLenient
  }

}
