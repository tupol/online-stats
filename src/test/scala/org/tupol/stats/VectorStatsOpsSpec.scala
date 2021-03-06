package org.tupol.stats

import scala.collection.immutable.Seq
import org.apache.commons.math3.stat._
import org.apache.commons.math3.stat.descriptive.moment.{ Kurtosis, Skewness, StandardDeviation, Variance }
import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class VectorStatsOpsSpec extends FunSuite with Matchers {

  test("zero |+| zero = zero") {
    VectorStats.Nil |+| VectorStats.Nil shouldBe VectorStats.Nil
  }

  test("stats |+| zero = stats") {
    VectorStats.fromDVector(Seq(1)) |+| VectorStats.Nil shouldBe VectorStats.fromDVector(Seq(1))
    VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0), Seq(3.0))) |+| VectorStats.Nil shouldBe VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0), Seq(3.0)))
  }

  test("stats1 |+| stats2 = stats2 |+| stats1") {
    VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0))) |+| VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) shouldBe VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) |+| VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0)))
    VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0))) |+| VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) shouldBe VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0), Seq(3.0), Seq(4.0)))

    VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0))) |+| VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) shouldBe VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) |+| VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0)))
    VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0))) |+| VectorStats.fromDVectors(Seq(Seq(3.0), Seq(4.0))) shouldBe VectorStats.fromDVectors(Seq(Seq(1.0), Seq(2.0), Seq(3.0), Seq(4.0)))
  }

  test("variance precision check") {

    val testSeqNormalNumbers = Seq(Seq(4.0), Seq(7.0), Seq(13.0), Seq(16.0))
    val testSeqLargeNumbers = testSeqNormalNumbers.map(_.map(_ + 1E+12))

    val testSeqSmallNumbers = testSeqNormalNumbers.map(_.map(_ / 1E+12))

    VectorStats.fromDVectors(testSeqNormalNumbers).variance(false).head shouldBe 22.5
    VectorStats.fromDVectors(testSeqNormalNumbers).variance(true).head shouldBe 30.0

    VectorStats.fromDVectors(testSeqLargeNumbers).variance(false).head shouldBe 22.5
    VectorStats.fromDVectors(testSeqLargeNumbers).variance(true).head shouldBe 30.0

    VectorStats.fromDVectors(testSeqSmallNumbers).variance(false).head shouldBe new Variance(false).evaluate(testSeqSmallNumbers.map(_.head).toArray)
    VectorStats.fromDVectors(testSeqSmallNumbers).variance(true).head shouldBe new Variance(true).evaluate(testSeqSmallNumbers.map(_.head).toArray)
  }

  test("test against reference implementation") {

    val toleranceDefault = 1E-9
    val toleranceLenient = 1E-2

    val randomizer = new Random(7773)
    val testData: Seq[Vector] = (0 to 100).map(_ => Seq(randomizer.nextDouble() * 1000))

    val statsComposed = testData.foldLeft(VectorStats.Nil)((result, input) => result |+| input)
    val statsFromDoubles = VectorStats.fromDVectors(testData)

    statsComposed.count shouldBe statsFromDoubles.count
    statsComposed.min shouldBe statsFromDoubles.min
    statsComposed.max shouldBe statsFromDoubles.max
    statsComposed.avg.head shouldBe statsFromDoubles.avg.head +- toleranceDefault
    statsComposed.average.head shouldBe statsComposed.avg.head
    statsComposed.mean.head shouldBe statsComposed.avg.head
    statsComposed.variance().head shouldBe statsFromDoubles.variance().head +- toleranceDefault
    statsComposed.skewness.head shouldBe statsFromDoubles.skewness.head +- toleranceDefault
    statsComposed.kurtosis.head shouldBe statsFromDoubles.kurtosis.head +- toleranceDefault

    statsComposed.count shouldBe testData.size
    statsComposed.min.head shouldBe StatUtils.min(testData.map(_.head).toArray)
    statsComposed.max.head shouldBe StatUtils.max(testData.map(_.head).toArray)
    statsComposed.avg.head shouldBe StatUtils.mean(testData.map(_.head).toArray) +- toleranceDefault
    statsComposed.variance(false).head shouldBe new Variance(false).evaluate(testData.map(_.head).toArray) +- toleranceLenient
    statsComposed.variance(true).head shouldBe new Variance(true).evaluate(testData.map(_.head).toArray) +- toleranceLenient
    statsComposed.stdev(false).head shouldBe new StandardDeviation(false).evaluate(testData.map(_.head).toArray) +- toleranceLenient
    statsComposed.stdev(true).head shouldBe new StandardDeviation(true).evaluate(testData.map(_.head).toArray) +- toleranceLenient
    statsComposed.skewness.head shouldBe new Skewness().evaluate(testData.map(_.head).toArray) +- toleranceLenient
    statsComposed.kurtosis.head shouldBe new Kurtosis().evaluate(testData.map(_.head).toArray) +- toleranceLenient
  }

}
