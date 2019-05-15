package org.tupol.stats

import org.scalatest.{ FunSuite, Matchers }
import optionops._

class OptionOpsSpec extends FunSuite with Matchers {

  test("None + None is None") {
    None + None shouldBe None
  }

  test("None - None is None") {
    None - None shouldBe None
  }

  test("None * None is None") {
    None * None shouldBe None
  }

  test("None / None is None") {
    None / None shouldBe None
  }

  test("double2Option(Double.NaN) is None") {
    double2Option(Double.NaN) shouldBe None
  }

}
