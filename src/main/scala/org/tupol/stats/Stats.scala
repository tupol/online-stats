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
case class Stats(count: Double, min: Double, max: Double, m1: Double, m2: Double, m3: Double, m4: Double) extends IStats

/** Calculates statistics data */
object Stats {

  /**
   * Calculates statistics data of the [[Iterable]] of Double's
   *
   * @param population non-empty Iterable[Double]
   * @return [[Stats]]
   */
  def fromDoubles(population: Iterator[Double]): Stats = fromDoubles(population.toIterable)
  def fromDoubles(population: Iterable[Double]): Stats = fromDoubles(population.toParArray)
  def fromDoubles(population: ParIterable[Double]): Stats =
    if (population.isEmpty)
      Nil
    else {
      val n = population.size
      val m1 = population.sum / n
      val sxe: ParIterable[(Double, Double, Double)] =
        population.map { d => val er = (d - m1); val er2 = er * er; val er3 = er * er2; val er4 = er * er3; (er2, er3, er4) }
      val m2 = sxe.map(_._1).sum
      val m3 = sxe.map(_._2).sum
      val m4 = sxe.map(_._3).sum

      new Stats(n, population.min, population.max, m1, m2, m3, m4)
    }

  /** Initialises the stats from a single double. */
  def fromDouble(value: Double): Stats = Stats(1, value, value, value, 0.0, 0.0, 0.0)

  val Nil: Stats = Stats(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  def Nil(value: Double): Stats = Stats(0, value, value, value, 0.0, 0.0, 0.0)

  def append(x: Stats, value: Double): Stats = append(x, Stats.fromDouble(value))

  def append(x: Stats, y: Stats): Stats = {
    if (x.count == 0) y
    else if (y.count == 0) x
    else {
      def nx = x.count
      def ny = y.count
      val n = nx + ny
      val min = math.min(x.min, y.min)
      val max = math.max(x.max, y.max)
      val delta1 = y.avg - x.avg
      val delta2 = delta1 * delta1
      val delta3 = delta1 * delta2
      val delta4 = delta1 * delta3
      val n2 = n * n
      val n3 = n * n2
      val nx2 = nx * nx
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

      Stats(n.toLong, min, max, m1, m2, m3, m4)
    }
  }

  implicit class StatsOps(val stats: Stats) {

    def append(that: Stats): Stats = Stats.append(stats, that)
    def append(that: Double): Stats = Stats.append(stats, that)
    def |+|(that: Stats): Stats = this.append(that)
    def |+|(that: Double): Stats = this.append(that)
  }

}
