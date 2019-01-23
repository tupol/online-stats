package org.tupol.stats

object EWeightedStatsOps {

  implicit val doubleStatsOps = DoubleEWeightedStats

  trait EWeightedStatsOps[T] {
    def append(x: T): EWeightedStats[T]
    def |+|(x: T): EWeightedStats[T] = append(x)
    def pdf(x: T, degenerateSolution: Double): T
    def probability(x: T, range: T, epsilon: T, degenerateSolution: Double): T
    def probabilityNSigma(x: T, epsilon: T, nSigma: T, degenerateSolution: Double): T
  }

  implicit class DoubleEWeightedStatsOps(val stats: EWeightedStats[Double]) extends EWeightedStatsOps[Double] {
    override def append(x: Double) = {
      val a = stats.alpha
      val newMin = math.min(stats.min, x)
      val newMax = math.max(stats.max, x)
      val m1 = (1 - a) * stats.m1 + a * x
      val m2 = (1 - a) * (stats.m2 + a * (x - stats.m1) * (x - stats.m1))
      val m3 = math.pow(m2, 3 / 2.0)
      val m4 = m2 * m2

      DoubleEWeightedStats(stats.alpha, stats.count + 1, newMin, newMax, m1, m2, m3, m4)
    }
    override def |+|(x: Double) = append(x)

    override def pdf(x: Double, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.pdf(x, stats.avg, stats.variance(), degenerateSolution)

    override def probability(x: Double, range: Double, epsilon: Double, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.probability(x, stats.avg, stats.stdev(), range, epsilon, degenerateSolution)

    override def probabilityNSigma(x: Double, epsilon: Double, nSigma: Double = 3, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.probability(x, stats.avg, stats.stdev(), nSigma * stats.stdev(), epsilon, degenerateSolution)

  }

}
