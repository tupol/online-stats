package org.tupol.stats

import org.tupol.stats
import org.tupol.stats.StatsOps.StatsOps
import org.tupol.stats.vectorops._

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
case class DVectorStats(count: Double, min: DVector, max: DVector, sum: DVector, m2: DVector, m3: DVector, m4: DVector) extends Stats[DVector] {

  import math._

  override def m1 = sum / count

  override def variance(biasCorrected: Boolean = false): DVector = {
    val correction = if (biasCorrected && count > 1) count.toDouble / (count - 1) else 1.0
    (sse / count) * correction
  }

  override def stdev(biasCorrected: Boolean = false): DVector = variance(biasCorrected).sqrt

  override def skewness: DVector = variance(false).zip(m2).zip(m3).map {
    case ((v, m2), m3) =>
      if (v <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)
  }

  override def kurtosis: DVector = variance(false).zip(m2).zip(m4).map {
    case ((v, m2), m4) =>
      if (v <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0
  }

}

/** Calculates statistics data */
object DVectorStats {

  /**
   * Calculates statistics data of the [[Iterable]] of Double's
   *
   * @param population non-empty Iterable[Double]
   * @return [[Stats]]
   */
  def fromDVectors(population: Iterable[DVector]): Stats[DVector] =
    if (population.isEmpty)
      zeroDouble
    else {
      val n = population.size
      val min = population.reduce((x, y) => x.zip(y).map(x => math.min(x._1, x._2)))
      val max = population.reduce((x, y) => x.zip(y).map(x => math.max(x._1, x._2)))
      val total = population.reduce(_ + _)
      val m1 = total / n.toDouble
      val sxe: Iterable[(DVector, DVector, DVector)] =
        population.map { d => val er = (d - m1); val er2 = er * er; val er3 = er * er2; val er4 = er * er3; (er2, er3, er4) }
      val m2 = sxe.map(_._1).reduce(_ + _)
      val m3 = sxe.map(_._2).reduce(_ + _)
      val m4 = sxe.map(_._3).reduce(_ + _)

      new DVectorStats(n, min, max, total, m2, m3, m4)
    }

  /**
   * Initialises the stats from a single double.
   * @param value
   * @return
   */
  def fromDVector(value: DVector): Stats[DVector] = {
    require(!value.isEmpty, "We can not initialize ")
    val zeroes = value.map(_ => 0.0)
    DVectorStats(1, value, value, value, zeroes, zeroes, zeroes)
  }

  val zeroDouble: Stats[DVector] = DVectorStats(0, IndexedSeq[Double](), IndexedSeq[Double](),
    IndexedSeq[Double](), IndexedSeq[Double](), IndexedSeq[Double](), IndexedSeq[Double]())

}

object DVectorStatsOps extends StatsOps[DVector] {

  override def append(x: Stats[DVector], value: DVector): Stats[DVector] = append(x, DVectorStats.fromDVector(value))

  override def append(x: Stats[DVector], y: Stats[DVector]): Stats[DVector] = {
    if (x.count == 0) y
    else if (y.count == 0) x
    else {
      require(x.avg.size == y.avg.size, "The size of the vectors we are composing must be the same.")
      val n = (x.count + y.count).toDouble
      val total = x.sum + y.sum
      val min = x.min.zip(y.min).map(x => math.min(x._1, x._2))
      val max = x.max.zip(y.max).map(x => math.max(x._1, x._2))
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

      stats.DVectorStats(n.toLong, min, max, total, m2, m3, m4)
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
  def pdf(x: DVector, mean: DVector, variance: DVector, degenerateSolution: Double = 1E-9): DVector = {
    require(x.size == mean.size && x.size == variance.size, "All input vectors must have the same size.")
    x.zip(mean).zip(variance).map { case ((x, mean), variance) => stats.pdf(x, mean, variance, degenerateSolution) }
  }

  override def pdf(s: Stats[DVector], x: DVector, degenerateSolution: Double): DVector = pdf(x, s.avg, s.variance(), degenerateSolution)

  override def probability(s: Stats[DVector], x: DVector, range: DVector, epsilon: DVector, degenerateSolution: Double): DVector =
    x.zip(s.mean).zip(s.stdev()).zip(range.zip(epsilon)).map {
      case (((x, mean), sigma), (range, epsilon)) =>
        stats.probability(x, mean, sigma, range, epsilon, degenerateSolution)
    }

  override def probabilityNSigma(s: Stats[DVector], x: DVector, epsilon: DVector, nSigma: DVector, degenerateSolution: Double): DVector =
    probability(s, x, nSigma * s.stdev(), epsilon, degenerateSolution)
}
