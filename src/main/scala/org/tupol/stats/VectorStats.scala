/*
MIT License

Copyright (c) 2018 Tupol (github.com/tupol)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
package org.tupol.stats

import scala.collection.immutable.Seq
import org.tupol.stats
import org.tupol.stats.vectorops._

import scala.collection.parallel.ParIterable

/**
 * The Stats implementation for doubles
 *
 * @param count the number of values in the given set
 * @param min the minimum value of the given set
 * @param max the maximum value of the given set
 * @param m1 moment 1; average
 * @param m2 moment 2; sum of squared errors
 * @param m3 moment 3; sum of cubed errors
 * @param m4 moment 4; sum of quartic errors
 */
case class VectorStats(count: Double, min: Vector, max: Vector, m1: Vector, m2: Vector, m3: Vector, m4: Vector) {

  import math._

  /** Sum of all elements  */
  def sum = m1 * count
  /** * The mean value in the given set */
  def avg = m1
  def average = m1
  def mean = m1

  /** The total sum of all the squared errors. Essentially this is just an alias for the second moment. */
  def sse = m2

  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def variance(biasCorrected: Boolean = false): Vector = {
    val correction = if (biasCorrected && count > 1) count.toDouble / (count - 1) else 1.0
    (sse / count) * correction
  }

  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def stdev(biasCorrected: Boolean = false): Vector = variance(biasCorrected).sqrt

  /** Skewness. See [[https://en.wikipedia.org/wiki/Skewness]] */
  def skewness: Vector = variance(false).zip(m2).zip(m3).map {
    case ((v, m2), m3) =>
      if (v <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)
  }

  /** Kurtosis. See [[https://en.wikipedia.org/wiki/Kurtosis]] */
  def kurtosis: Vector = variance(false).zip(m2).zip(m4).map {
    case ((v, m2), m4) =>
      if (v <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0
  }

}

/** Calculates statistics data */
object VectorStats {

  /**
   * Calculates statistics data of the [[Iterable]] of Double's
   *
   * @param population non-empty Iterable[Double]
   * @return [[Stats]]
   */
  def fromDVectors(population: Iterator[Vector]): VectorStats = fromDVectors(population.toParArray)
  def fromDVectors(population: Iterable[Vector]): VectorStats = fromDVectors(population.toParArray)
  def fromDVectors(population: ParIterable[Vector]): VectorStats =
    if (population.isEmpty)
      zeroDouble
    else {
      val n = population.size
      val min = population.reduce((x, y) => x.zip(y).map(x => math.min(x._1, x._2)))
      val max = population.reduce((x, y) => x.zip(y).map(x => math.max(x._1, x._2)))
      val total = population.reduce(_ + _)
      val m1 = total / n.toDouble
      val sxe: ParIterable[(Vector, Vector, Vector)] =
        population.map { d => val er = (d - m1); val er2 = er * er; val er3 = er * er2; val er4 = er * er3; (er2, er3, er4) }
      val m2 = sxe.map(_._1).reduce(_ + _)
      val m3 = sxe.map(_._2).reduce(_ + _)
      val m4 = sxe.map(_._3).reduce(_ + _)

      new VectorStats(n, min, max, m1, m2, m3, m4)
    }

  /**
   * Initialises the stats from a single double.
   * @param value
   * @return
   */
  def fromDVector(value: Vector): VectorStats = {
    require(!value.isEmpty, "We can not initialize ")
    val zeroes = value.map(_ => 0.0)
    VectorStats(1, value, value, value, zeroes, zeroes, zeroes)
  }

  val zeroDouble: VectorStats = VectorStats(0, Seq[Double](), Seq[Double](),
    Seq[Double](), Seq[Double](), Seq[Double](), Seq[Double]())

  def append(x: VectorStats, value: Vector): VectorStats = append(x, VectorStats.fromDVector(value))

  def append(x: VectorStats, y: VectorStats): VectorStats = {
    if (x.count == 0) y
    else if (y.count == 0) x
    else {
      require(x.avg.size == y.avg.size, "The size of the vectors we are composing must be the same.")
      val n = (x.count + y.count).toDouble
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
      val m1 = (nx * x.m1 + ny * y.m1) / n // Moment 1
      val m2 = (x.m2 + y.m2) + delta2 * nx * ny / n // Moment 2
      val m3 = (x.m3 + y.m3) +
        1.0 * delta3 * nx * ny * (nx - ny) / n2 +
        3.0 * delta1 * (nx * y.m2 - ny * x.m2) / n
      val m4 = (x.m4 + y.m4) +
        1.0 * delta4 * nx * ny * (nx2 - nx * ny + ny2) / n3 +
        6.0 * delta2 * (nx2 * y.m2 + ny2 * x.m2) / n2 +
        4.0 * delta1 * (nx * y.m3 - ny * x.m3) / n

      stats.VectorStats(n.toLong, min, max, m1, m2, m3, m4)
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
  def pdf(x: Vector, mean: Vector, variance: Vector, degenerateSolution: Double = 1E-9): Vector = {
    require(x.size == mean.size && x.size == variance.size, "All input vectors must have the same size.")
    x.zip(mean).zip(variance).map { case ((x, mean), variance) => stats.pdf(x, mean, variance, degenerateSolution) }
  }

  def pdf(s: VectorStats, x: Vector, degenerateSolution: Double): Vector = pdf(x, s.avg, s.variance(), degenerateSolution)

  def probability(s: VectorStats, x: Vector, range: Vector, epsilon: Vector, degenerateSolution: Double): Vector =
    x.zip(s.mean).zip(s.stdev()).zip(range.zip(epsilon)).map {
      case (((x, mean), sigma), (range, epsilon)) =>
        stats.probability(x, mean, sigma, range, epsilon, degenerateSolution)
    }

  def probabilityNSigma(s: VectorStats, x: Vector, epsilon: Vector, nSigma: Vector, degenerateSolution: Double): Vector =
    probability(s, x, nSigma * s.stdev(), epsilon, degenerateSolution)

  implicit class StatsOps(val stats: VectorStats) {

    def append(that: VectorStats): VectorStats = VectorStats.append(stats, that)
    def append(that: Vector): VectorStats = VectorStats.append(stats, that)
    def |+|(that: VectorStats): VectorStats = this.append(that)
    def |+|(that: Vector): VectorStats = this.append(that)
    def probability(x: Vector, range: Vector, epsilon: Vector, degenerateSolution: Double): Vector =
      VectorStats.probability(stats, x, range, epsilon, degenerateSolution)
    def probabilityNSigma(x: Vector, epsilon: Vector, nSigma: Vector, degenerateSolution: Double): Vector =
      VectorStats.probabilityNSigma(stats, x, epsilon, nSigma, degenerateSolution)

  }

}
