package io.tupol.stats

import io.tupol.stats.CovarianceOps._
import io.tupol.utils.DefaultTolerance
import io.tupol.utils.Tolerance._
import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

class CovarianceOpsSpec extends FunSuite with Matchers with DefaultTolerance {

  test("Zero + doubles: Covariance.zeroDouble |+| (x, y)") {

    val result = Covariance.zeroDouble |+| (1, 2)

    result shouldBe Covariance.fromDoubles(1, 2)
  }

  test("Update with doubles: covar1 |+| (x, y)") {

    val seq = (0 to 100).map(_ => Random.nextDouble())
    val result = seq.foldLeft(Covariance.zeroDouble)((acc, s) => acc |+| (s, s))
    val expected = Covariance.fromDoubles(seq, seq).covariance
    result.covariance shouldBe expected +- tolerance.epsilon
  }

  test("Update with doubles: covar2 |+| (x, y)") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = (0 to 100).map(_ => Random.nextDouble())

    val result = seq1.zip(seq2).foldLeft(Covariance.zeroDouble)((acc, s) => acc |+| (s._1, s._2))
    val expected = Covariance.fromDoubles(seq1, seq2).covariance
    result.covariance shouldBe expected +- tolerance.epsilon
  }

  test("Zero + covariance: Covariance.zeroDouble |+| covar1") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = (0 to 100).map(_ => Random.nextDouble())

    val covar = Covariance.fromDoubles(seq1, seq2)
    val result1 = Covariance.zeroDouble |+| covar
    val result2 = covar |+| Covariance.zeroDouble

    result1 shouldBe covar
    result2 shouldBe covar
  }

  test("Commutativity: covar1 |+| covar2 = covar2 |+| covar1") {
    val covar1 = Covariance.fromDoubles(Seq(1.0, 2), Seq(1.0, 2))
    val covar2 = Covariance.fromDoubles(Seq(5.0, 6), Seq(5.0, 6))

    covar1 |+| covar2 shouldBe covar2 |+| covar1
    covar1 |+| covar2 shouldBe Covariance.fromDoubles(Seq(1.0, 2, 5, 6), Seq(1.0, 2, 5, 6))
  }

  test("Associativity: (covar1 |+| covar2) + covar3 = covar1 |+| (covar2 + covar3)") {
    val covar1 = Covariance.fromDoubles(Seq(1.0, 2), Seq(1.0, 2))
    val covar2 = Covariance.fromDoubles(Seq(3.0, 4), Seq(3.0, 4))
    val covar3 = Covariance.fromDoubles(Seq(5.0, 6), Seq(5.0, 6))
    (covar1 |+| covar2) |+| covar3 shouldBe covar1 |+| (covar2 |+| covar3)
    (covar1 |+| covar2) |+| covar3 shouldBe covar1 |+| covar2 |+| covar3

    (covar1 |+| covar2 |+| covar3).approx(Covariance.fromDoubles(Seq(1.0, 2, 3, 4, 5, 6), Seq(1.0, 2, 3, 4, 5, 6))) shouldBe true

  }

  test("Update with Covariances: covar1 |+| covar2 |+| ....") {

    val seqs = (0 to 10).map(_ => ((0 to 10).map(_ => (Random.nextDouble())), (0 to 10).map(_ => (Random.nextDouble()))))
    val flatSeqs = seqs.reduce((s1, s2) => (s1._1 ++ s2._1, s1._2 ++ s2._2))

    val expected = Covariance.fromDoubles(flatSeqs._1, flatSeqs._2)
    val actual = seqs.tail.foldLeft(Covariance.fromDoubles(seqs.head._1, seqs.head._2))((acc, x) => acc |+| Covariance.fromDoubles(x._1, x._2))

    actual.approx(expected) shouldBe true

  }

  test("Pearsons Correlation Coefficient: 1 (Highly similar features)") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = seq1.map(2 * _ + 3)
    val pcc = new org.apache.commons.math3.stat.correlation.PearsonsCorrelation()

    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation shouldBe 1.0 +- tolerance.epsilon
    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation shouldBe pcc.correlation(seq1.toArray, seq2.toArray) +- tolerance.epsilon
  }

  test("Pearsons Correlation Coefficient: -1 (Orthogoal features)") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = seq1.map(-1 * _)
    val pcc = new org.apache.commons.math3.stat.correlation.PearsonsCorrelation()

    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation shouldBe -1.0 +- tolerance.epsilon
    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation shouldBe pcc.correlation(seq1.toArray, seq2.toArray)
  }

  test("Pearsons Correlation Coefficient tested against a reference implementation") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = (0 to 100).map(_ => Random.nextDouble())
    val pcc = new org.apache.commons.math3.stat.correlation.PearsonsCorrelation()

    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation shouldBe pcc.correlation(seq1.toArray, seq2.toArray) +- tolerance.epsilon
  }

  test("Pearsons Correlation Coefficient should be NaN") {

    val seq1 = (0 to 100).map(_ => Random.nextDouble())
    val seq2 = seq1.map(_ => 1.0)

    val pcc = new org.apache.commons.math3.stat.correlation.PearsonsCorrelation()
    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation.isNaN shouldBe true
    Covariance.fromDoubles(seq1, seq2).pearsonCorrelation.isNaN shouldBe pcc.correlation(seq1.toArray, seq2.toArray).isNaN
  }

}
