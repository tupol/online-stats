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
package org.tupol

package object stats {

  /** Stats package Vector type of Doubles */
  type Vector = Seq[Double]
  /** &radic;(2 &pi;) */
  private val SQRT2PI = math.sqrt(2 * math.Pi)
  /**
   * Probability density function
   * @param x
   * @param mean
   * @param variance `sigma ^ 2`
   * @param degenerateSolution sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  private[stats] def pdf(x: Double, mean: Double, stdev: Double, variance: Double, degenerateSolution: Double = 1E-12) = {
    import math._
    if (variance == 0)
      if (x == mean) 1.0 else degenerateSolution
    else
      (1 / (SQRT2PI * stdev)) * exp(-(x - mean) * (x - mean) / (2 * variance))
  }

  /**
   * Compute the probability of a value `x` to be in the interval `[x-range, x+range]`.
   * @param x the value to compute the probability for
   * @param mean the average of the population
   * @param stdev standard deviation
   * @param range the around the x value for which we compute the probability, e.g. probability( x +- range);
   *              to get probabilities close to 1, the range should be 3 * stdev
   * @param epsilon how small should the incremental interval for computing the probability be; it should be
   *                at least an order of magnitude smaller than the `range`
   * @param degenerateSolution sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  private[stats] def probability(x: Double, mean: Double, stdev: Double, variance: Double, range: Double, epsilon: Double,
    degenerateSolution: Double = 1E-12) = {
    if (stdev == 0 && x == mean) 1.0
    else {
      require(epsilon > 0.0 && epsilon <= range / 10.0, s"epsilon ($epsilon) must be be a number greater than zero (0) " +
        s"and at least an order of magnitude smaller than the range ($range).")
      val splits = math.round(range / epsilon).toInt
      val from = x - range
      epsilon * (0 until 2 * splits).map(s => pdf(from + (0.5 + s) * epsilon, mean, stdev, variance, degenerateSolution)).sum
    }
  }

}
