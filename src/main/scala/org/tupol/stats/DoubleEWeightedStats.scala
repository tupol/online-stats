package org.tupol.stats

/**
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
case class DoubleEWeightedStats(alpha: Double, count: Double, min: Double, max: Double,
                                m1: Double, m2: Double, m3: Double, m4: Double) extends EWeightedStats[Double] {

  import math._

  /** @inheritdoc */
  override def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count / (count - 1) else 1.0
    (sse / count) * correction
  }

  /** @inheritdoc */
  override def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))

  /** @inheritdoc */
  override def skewness: Double = if (variance(false) <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)

  /** @inheritdoc */
  override def kurtosis: Double = if (variance(false) <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

  /** @inheritdoc */
  override def sum: Double = count * avg

}

object DoubleEWeightedStats {

  import EWeightedStatsOps._

  def fromDoubles(alpha: Double, population: Iterable[Double]): EWeightedStats[Double] =
    population.tail.foldLeft(fromDouble(alpha, population.head))((result, x) => result |+| x)

  def fromDouble(alpha: Double, value: Double): EWeightedStats[Double] =
    DoubleEWeightedStats(alpha, 1, value, value, value, 0.0, 0.0, 0.0)

  def zeroDouble(alpha: Double, value: Double): EWeightedStats[Double] =
    DoubleEWeightedStats(alpha, 0, value, value, value, 0.0, 0.0, 0.0)

  def zeroDouble(alpha: Double): EWeightedStats[Double] = zeroDouble(alpha, 0)

}
