package org.tupol.stats

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.{ FunSuite, Matchers }

class PackageSpec extends FunSuite with Matchers {

  test("probability density function") {
    val nd = new NormalDistribution()
    pdf(1, 0, 1, 1) shouldBe nd.density(1) +- 1E-3
    pdf(0.1, 0, 1, 1) shouldBe nd.density(0.1) +- 1E-3
  }

  test("probability density function for the average value") {
    val nd = new NormalDistribution()
    pdf(0, 0, 1, 1) shouldBe nd.density(0) +- 1E-3
  }

  test("probability density function with degenerate solution") {
    val degenerateSolution = 1E-6
    pdf(0.1, 0, 0, 0, degenerateSolution) shouldBe degenerateSolution
  }

  test("probability") {
    val nd = new NormalDistribution()
    probability(0, 0, 1, 1, 1, 0.01) shouldBe nd.probability(-1, 1) +- 1E-3
  }

  test("probability failes for epsilon outside reasonable range") {
    an[IllegalArgumentException] shouldBe thrownBy(probability(0, 0, 1, 1, 1, -0.01))
    an[IllegalArgumentException] shouldBe thrownBy(probability(0, 0, 1, 1, 1, 0.11))
  }

  test("probability for average value and null standard deviation") {
    val nd = new NormalDistribution()
    probability(0, 0, 0, 0, 1, 0.01) shouldBe 1.0
  }

}
