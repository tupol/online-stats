package org.tupol.stats

/**
 * Exponentially Weighted Moving Stats Calculator
 *
 * `exponentially weighted average = (1 - alpha) * previous_ewma + alpha * current_value`
 *
 * @param alpha
 * @param count the number of values in the given set
 * @param min the minimum value of the given set
 * @param max the maximum value of the given set
 * @param m1 moment 1
 * @param m2 moment 2
 * @param m3 moment 3
 * @param m4 moment 4
 */
case class EWeightedStats(alpha: Double, count: Double, min: Double, max: Double,
  m1: Double, m2: Double, m3: Double, m4: Double) {

  import math._

  /** * The mean value in the given set */
  def avg: Double = m1
  def average: Double = m1
  def mean: Double = m1
  /** The total sum of all the squared errors. Essentially this is just an alias for the second moment. */
  def sse: Double = m2
  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   */
  def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count / (count - 1) else 1.0
    (sse / count) * correction
  }
  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   */
  def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))
  /** Skewness. See [[https://en.wikipedia.org/wiki/Skewness]] */
  def skewness: Double = if (variance(false) <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)
  /** Kurtosis. See [[https://en.wikipedia.org/wiki/Kurtosis]] */
  def kurtosis: Double = if (variance(false) <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

  /** @inheritdoc */
  def sum: Double = count * avg

}

object EWeightedStats {

  def fromDoubles(alpha: Double, population: Iterable[Double]): EWeightedStats =
    population.tail.foldLeft(fromDouble(alpha, population.head))((result, x) => result |+| x)

  def fromDouble(alpha: Double, value: Double): EWeightedStats =
    EWeightedStats(alpha, 1, value, value, value, 0.0, 0.0, 0.0)

  def zeroDouble(alpha: Double, value: Double): EWeightedStats =
    EWeightedStats(alpha, 0, value, value, value, 0.0, 0.0, 0.0)

  def zeroDouble(alpha: Double): EWeightedStats = zeroDouble(alpha, 0)

  implicit class EWeightedStatsOps(val stats: EWeightedStats) {

    def append(x: Double) = {
      val a = stats.alpha
      val newMin = math.min(stats.min, x)
      val newMax = math.max(stats.max, x)
      val m1 = (1 - a) * stats.m1 + a * x
      val m2 = (1 - a) * (stats.m2 + a * (x - stats.m1) * (x - stats.m1))
      val m3 = math.pow(m2, 3 / 2.0)
      val m4 = m2 * m2

      EWeightedStats(stats.alpha, stats.count + 1, newMin, newMax, m1, m2, m3, m4)
    }
    def |+|(x: Double) = append(x)

    def pdf(x: Double, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.pdf(x, stats.avg, stats.variance(), degenerateSolution)

    def probability(x: Double, range: Double, epsilon: Double, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.probability(x, stats.avg, stats.stdev(), range, epsilon, degenerateSolution)

    def probabilityNSigma(x: Double, epsilon: Double, nSigma: Double = 3, degenerateSolution: Double = 1E-12): Double =
      org.tupol.stats.probability(x, stats.avg, stats.stdev(), nSigma * stats.stdev(), epsilon, degenerateSolution)
  }
}
