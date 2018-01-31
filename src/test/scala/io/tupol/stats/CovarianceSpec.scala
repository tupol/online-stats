package io.tupol.stats

import io.tupol.utils.DefaultTolerance
import org.scalatest.{ FunSuite, Matchers }

import scala.util.Random

class CovarianceSpec extends FunSuite with Matchers with DefaultTolerance {

  test("Covariance.fromDoubles of 2 numbers") {
    val result = Covariance.fromDoubles(1, 1)
    result.covariance shouldBe 0.0
  }

  test("Covariance.fromDoubles of 2 sequences of numbers ") {
    val seq1 = Seq(1.0, 2.0)
    val seq2 = Seq(5.0, 10.0)

    val referenceCovar = new org.apache.commons.math3.stat.correlation.Covariance()
    val expected = referenceCovar.covariance(seq1.toArray, seq2.toArray, false)
    val actual = Covariance.fromDoubles(seq1, seq2).covariance

    actual shouldBe expected
  }

  test("Covariance.fromDoubles of 2 sequences of random numbers") {
    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = (0 to 100).map(_ => Random.nextDouble())

    val referenceCovar = new org.apache.commons.math3.stat.correlation.Covariance()
    val expected = referenceCovar.covariance(seq1.toArray, seq2.toArray, false)
    val actual = Covariance.fromDoubles(seq1, seq2).covariance

    actual shouldBe expected +- tolerance.epsilon
  }

  test("Covariance(X, Y) should equal Covariance(Y, X)") {
    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = (0 to 100).map(_ => Random.nextDouble())

    val referenceCovar = new org.apache.commons.math3.stat.correlation.Covariance()
    val expected = referenceCovar.covariance(seq1.toArray, seq2.toArray, false)

    val actualXY = Covariance.fromDoubles(seq1, seq2).covariance
    val actualYX = Covariance.fromDoubles(seq2, seq1).covariance

    actualXY shouldBe expected +- tolerance.epsilon
    actualYX shouldBe expected +- tolerance.epsilon
  }

}
