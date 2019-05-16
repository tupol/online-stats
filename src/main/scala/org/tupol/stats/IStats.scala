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

import org.tupol

trait IStats {
  /** the number of values in the given set */
  def count: Double
  /** the minimum value of the given set */
  def min: Double
  /** the maximum value of the given set */
  def max: Double
  /** moment 1; average */
  def m1: Double
  /** moment 2; sum of squared errors */
  def m2: Double
  /** moment 3; sum of cubed errors */
  def m3: Double
  /** moment 4; sum of quartic errors */
  def m4: Double

  import math._

  /** Sum of all elements  */
  def sum = m1 * count
  /** The mean value in the given set */
  def avg: Double = m1
  /** The mean value in the given set */
  def average: Double = m1
  /** The mean value in the given set */
  def mean: Double = m1

  /** The total sum of all the squared errors. Essentially this is just an alias for the second moment. */
  def sse: Double = m2

  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count.toDouble / (count - 1) else 1.0
    (sse / count) * correction
  }
  /** Variance without bias correction */
  lazy val variance: Double = variance(false)
  /** Variance with bias correction */
  lazy val varianceBC: Double = variance(true)

  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))
  /** Standard deviation without bias correction */
  lazy val stdev: Double = stdev(false)
  /** Standard deviation with bias correction */
  lazy val stdevBC: Double = stdev(true)

  /** Skewness. See [[https://en.wikipedia.org/wiki/Skewness]] */
  def skewness: Double = if (variance <= 1E-20) 0.0 else sqrt(count) * m3 / pow(m2, 1.5)

  /** Kurtosis. See [[https://en.wikipedia.org/wiki/Kurtosis]] */
  def kurtosis: Double = if (variance <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

  /** Probability density function */
  def pdf(x: Double, degenerateSolution: Double = 1E-12): Double =
    tupol.stats.pdf(x, avg, stdev, variance, degenerateSolution)

  def probability(x: Double, range: Double, epsilon: Double, degenerateSolution: Double): Double =
    tupol.stats.probability(x, mean, stdev, variance, range, epsilon, degenerateSolution)

  def probability(x: Double, range: Double, splits: Int = 10, degenerateSolution: Double = 1E-12): Double =
    probability(x, range, range / splits, degenerateSolution)

  def probabilityNSigma(x: Double, epsilon: Double, nSigma: Double, degenerateSolution: Double): Double =
    probability(x, nSigma * stdev, epsilon, degenerateSolution)

  def probabilityNSigma(x: Double, splits: Int = 10, nSigma: Double = 3, degenerateSolution: Double = 1E-12): Double =
    probability(x, nSigma * stdev, nSigma * stdev / splits, degenerateSolution)

}
