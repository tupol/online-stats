package org.tupol.stats

case class DoubleEWeightedStats(alpha: Double, min: Double, max: Double, m1: Double, m2: Double, m3: Double, m4: Double) extends EWeightedStats[Double] {

  import math._

  override def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count / (count - 1) else 1.0
    (sse / count) * correction
  }

  override def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))

  override def skewness: Double = if (variance(false) <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)

  override def kurtosis: Double = if (variance(false) <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

  /**
   * The total sum of all the values
   *
   * @return
   */
  override def sum: Double = count * avg

}

object DoubleEWeightedStats {

  import EWeightedStatsOps._

  def fromDoubles(alpha: Double, population: Iterable[Double]): EWeightedStats[Double] =
    population.tail.foldLeft(zeroDouble(alpha, population.head))((result, x) => result |+| x)

  def fromDouble(alpha: Double, value: Double): EWeightedStats[Double] =
    fromDoubles(alpha, Iterable(value))

  def zeroDouble(alpha: Double, value: Double): EWeightedStats[Double] =
    DoubleEWeightedStats(alpha, value, value, value, 0.0, 0.0, 0.0)

  def zeroDouble(alpha: Double): EWeightedStats[Double] = zeroDouble(alpha, 0)

}
