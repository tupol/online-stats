package org.tupol.stats

import org.scalatest.{ FunSuite, Matchers }
import vectorops._

class VectorOpsSpec extends FunSuite with Matchers {

  test("try to operate two vectors of different sizes results in an exception") {
    val v1 = Seq(1.0)
    val v2 = Seq(1.0, 2.0)
    an[IllegalArgumentException] shouldBe thrownBy(v1 + v2)
  }

  test("vector + vector") {
    val v1 = Seq(2.0, -2.0)
    val v2 = Seq(-1.0, 2.0)
    val expected = Seq(1.0, 0.0)
    v1 + v2 shouldBe expected
  }

  test("vector - vector") {
    val v1 = Seq(2.0, -2.0)
    val v2 = Seq(-1.0, 2.0)
    val expected = Seq(3.0, -4.0)
    v1 - v2 shouldBe expected
  }

  test("vector * vector") {
    val v1 = Seq(1.0, 1.0, -1.0, -1.0)
    val v2 = Seq(0.5, -0.5, 0.5, -0.5)
    val expected = Seq(0.5, -0.5, -0.5, 0.5)
    v1 * v2 shouldBe expected
  }

  test("vector / vector") {
    val v1 = Seq(1.0, 1.0, -1.0, -1.0)
    val v2 = Seq(2.0, -2.0, 2.0, -2.0)
    val expected = Seq(0.5, -0.5, -0.5, 0.5)
    v1 / v2 shouldBe expected
  }

  test("vector + scalar") {
    val v1 = Seq(2.0, -2.0)
    val x = 2
    val expected = Seq(4.0, 0.0)
    v1 + x shouldBe expected
  }

  test("vector - scalar") {
    val v1 = Seq(2.0, -2.0)
    val x = 2
    val expected = Seq(0.0, -4.0)
    v1 - x shouldBe expected
  }

  test("vector * scalar") {
    val v1 = Seq(2.0, -2.0)
    val x = 2
    val expected = Seq(4.0, -4.0)
    v1 * x shouldBe expected
  }

  test("vector / scalar") {
    val v1 = Seq(2.0, -2.0)
    val x = 2
    val expected = Seq(1.0, -1.0)
    v1 / x shouldBe expected
  }

  test("unary -") {
    val v1 = Seq(2.0, -2.0)
    val expected = Seq(-2.0, 2.0)
    -v1 shouldBe expected
  }

  test("abs") {
    val v1 = Seq(2.0, -2.0, -0.0)
    val expected = Seq(2.0, 2.0, 0.0)
    v1.abs shouldBe expected
  }

  test("pow") {
    val v1 = Seq(2.0, -2.0, 1.0, -0.0)
    val expected = Seq(4.0, 4.0, 1.0, 0.0)
    v1.pow(2) shouldBe expected
  }

}
