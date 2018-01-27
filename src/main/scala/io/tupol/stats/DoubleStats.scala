package io.tupol.stats

import io.tupol.stats.StatsOps.StatsOps

/**
 * The Stats implementation for doubles
 *
 * @param count the number of values in the given set
 * @param min the minimum value of the given set
 * @param max the maximum value of the given set
 * @param sum sum of all elements
 * @param m2 moment 2
 * @param m3 moment 3
 * @param m4 moment 4
 */
case class DoubleStats(count: Long, min: Double, max: Double, sum: Double, m2: Double, m3: Double, m4: Double) extends Stats[Double] {

  import math._

  override def m1 = sum.toDouble / count

  override def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count.toDouble / (count - 1) else 1.0
    (sse / count) * correction
  }

  override def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))

  override def skewness: Double = if (variance(false) <= 1E-20) 0.0 else sqrt(count.toDouble) * m3 / pow(m2, 1.5)

  override def kurtosis: Double = if (variance(false) <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

}

/** Calculates statistics data */
object DoubleStats {

  /**
   * Calculates statistics data of the [[Iterable]] of Double's
   *
   * @param population non-empty Iterable[Double]
   * @return [[Stats]]
   */
  def fromDoubles(population: Iterable[Double]): Stats[Double] =
    if (population.isEmpty)
      zeroDouble
    else {
      val n = population.size
      val total = population.sum
      val m1 = total / n.toDouble
      val sxe: Iterable[(Double, Double, Double)] =
        population.map { d => val er = (d - m1); val er2 = er * er; val er3 = er * er2; val er4 = er * er3; (er2, er3, er4) }
      val m2 = sxe.map(_._1).sum
      val m3 = sxe.map(_._2).sum
      val m4 = sxe.map(_._3).sum

      new DoubleStats(n, population.min, population.max, total, m2, m3, m4)
    }

  /**
   * Initialises the stats from a single double.
   * @param value
   * @return
   */
  def fromDoubles(value: Double): Stats[Double] = DoubleStats(1, value, value, value, 0.0, 0.0, 0.0)

  val zeroDouble: Stats[Double] = DoubleStats(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  def zeroDouble(value: Double): Stats[Double] = DoubleStats(0, value, value, value, 0.0, 0.0, 0.0)

}

object DoubleStatsOps extends StatsOps[Double] {

  override def append(x: Stats[Double], y: Stats[Double]): Stats[Double] = {
    if (x.count == 0) y
    else if (y.count == 0) x
    else {
      val n = (x.count + y.count).toDouble
      val total = x.sum + y.sum
      val min = math.min(x.min, y.min)
      val max = math.max(x.max, y.max)
      val delta1 = y.avg - x.avg
      val delta2 = delta1 * delta1
      val delta3 = delta1 * delta2
      val delta4 = delta1 * delta3
      val n2 = n * n
      val n3 = n * n2
      def nx = x.count
      val nx2 = nx * nx
      def ny = y.count
      val ny2 = ny * ny
      // See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
      //        val m1 = (nx * x.m1 + ny * y.m1) / n // Moment 1
      val m1 = total / n.toDouble
      val m2 = (x.m2 + y.m2) + delta2 * nx * ny / n // Moment 2
      val m3 = (x.m3 + y.m3) +
        1.0 * delta3 * nx * ny * (nx - ny) / n2 +
        3.0 * delta1 * (nx * y.m2 - ny * x.m2) / n
      val m4 = (x.m4 + y.m4) +
        1.0 * delta4 * nx * ny * (nx2 - nx * ny + ny2) / n3 +
        6.0 * delta2 * (nx2 * y.m2 + ny2 * x.m2) / n2 +
        4.0 * delta1 * (nx * y.m3 - ny * x.m3) / n

      DoubleStats(n.toLong, min, max, total, m2, m3, m4)
    }
  }

  /**
   * Probability density function
   * @param x
   * @param mean
   * @param variance `sigma ^ 2`
   * @param degenerateSolution Sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  def pdf(x: Double, mean: Double, variance: Double, degenerateSolution: Double = 1E-9) = {
    import math._
    if (variance == 0)
      if (x == mean) 1.0 else degenerateSolution
    else
      (1 / sqrt(2 * Pi * variance)) * exp(-pow(x - mean, 2) / (2 * variance))
  }

  override def pdf(s: Stats[Double], x: Double, degenerateSolution: Double): Double = pdf(x, s.avg, s.variance())
}

