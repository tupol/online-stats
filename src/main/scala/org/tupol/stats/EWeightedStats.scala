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

/**
 * Exponentially Weighted Moving Stats Calculator
 *
 * `exponentially weighted average = (1 - alpha) * previous_ewma + alpha * current_value`
 *
 * @param alpha the importance of the latest readings vs the importance of history (history weight: 1 - alpha)
 * @param count the number of values in the given set
 * @param min the minimum value of the given set
 * @param max the maximum value of the given set
 * @param m1 moment 1; average
 * @param m2 moment 2; sum of squared errors
 * @param m3 moment 3; sum of cubed errors
 * @param m4 moment 4; sum of quartic errors
 */
case class EWeightedStats(alpha: Double, count: Double, min: Double, max: Double,
  m1: Double, m2: Double, m3: Double, m4: Double) extends IStats

object EWeightedStats {

  def fromDoubles(alpha: Double, population: Iterable[Double]): EWeightedStats =
    population.tail.foldLeft(fromDouble(alpha, population.head))((result, x) => result |+| x)

  def fromDouble(alpha: Double, value: Double): EWeightedStats =
    EWeightedStats(alpha, 1, value, value, value, 0.0, 0.0, 0.0)

  def Nil(alpha: Double, value: Double): EWeightedStats =
    EWeightedStats(alpha, 0, value, value, value, 0.0, 0.0, 0.0)

  def Nil(alpha: Double): EWeightedStats = Nil(alpha, 0)

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
  }
}
