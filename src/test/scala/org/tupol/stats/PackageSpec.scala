package org.tupol.stats

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.{ FunSuite, Matchers }

class PackageSpec extends FunSuite with Matchers {

  test("probability density function") {
    val nd = new NormalDistribution()
    pdf(1, 0, 1) shouldBe nd.density(1) +- 1E-3
  }

  test("probability density function for the average value") {
    val nd = new NormalDistribution()
    pdf(0.1, 0, 1) shouldBe nd.density(0.1) +- 1E-3
  }

  test("probability density function with degenerate solution") {
    val degenerateSolution = 1E-6
    pdf(0.1, 0, 0, degenerateSolution) shouldBe degenerateSolution
  }

  test("probability") {
    val nd = new NormalDistribution()
    probability(0.001, 0, 1, 1, 0.1) shouldBe nd.probability(-1, 1) +- 1E-3
  }

}
