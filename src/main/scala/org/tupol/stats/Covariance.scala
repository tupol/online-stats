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
 * Generic Covariance
 *
 * @param covariance
 * @param comoment
 * @param xstats
 * @param ystats
 */
case class Covariance(covariance: Double, comoment: Double, xstats: Stats, ystats: Stats)

/** Covariance companion with Covariance factories */
object Covariance {

  /**
   * Compute Covariance out of two sequences of numbers.
   * The Covariance has no bias correction
   * @param xs
   * @param ys
   * @return
   */
  def fromDoubles(xs: Seq[Double], ys: Seq[Double]): Covariance = {

    assert(xs.size == ys.size, "Can not correlate vectors of different sizes.")
    assert(xs.size > 0, "Can not correlate empty vectors.")

    val alignedInputs = xs.zip(ys)

    val n = alignedInputs.size
    val x = Stats.fromDoubles(alignedInputs.map(_._1))
    val y = Stats.fromDoubles(alignedInputs.map(_._2))

    val sumXtimesY = alignedInputs.map { case (a, b) => a * b }.sum

    val com = xs.zip(ys).map { case (a, b) => (a - x.avg) * (b - y.avg) }.sum

    val cov = sumXtimesY / n - x.avg * y.avg

    Covariance(cov, com, x, y)

  }

  def fromDoubles(x: Double, y: Double): Covariance = fromDoubles(Seq(x), Seq(y))

  val Nil: Covariance = Covariance(0, 0, Stats.Nil, Stats.Nil)

  /**
   * Append a tuple to the current Covariance to obtain a new covariance
   * See [[https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online]] for ideas
   * @param covar
   * @param x
   * @param y
   * @return
   */
  def append(covar: Covariance, x: Double, y: Double): Covariance = {

    if (covar.xstats.count == 0)
      Covariance.fromDoubles(x, y)
    else {
      val n = covar.xstats.count + 1.0

      val xAvg = covar.xstats.avg
      val yAvg = covar.ystats.avg

      val xstats = covar.xstats |+| x
      val ystats = covar.ystats |+| y

      val nc = (n - 1) / n
      val comoment = covar.comoment + nc * (x - xAvg) * (y - yAvg)
      val covariance = covar.covariance * nc + (x - xAvg) * (y - yAvg) * nc / n

      Covariance(covariance, comoment, xstats, ystats)
    }

  }
  /**
   * Compose two covariance instances to obtain a new Covariance
   * See "Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments" by Philippe Pebay
   * ([[http://prod.sandia.gov/techlib/access-control.cgi/2008/086212.pdf]])
   * @param covar1
   * @param covar2
   * @return
   */
  def append(covar1: Covariance, covar2: Covariance): Covariance = {

    val xstats = covar1.xstats |+| covar2.xstats
    val ystats = covar1.ystats |+| covar2.ystats

    val (covariance, comoment) = (covar1.xstats.count, covar2.xstats.count) match {
      case (0, _) => (covar2.covariance, covar2.comoment)
      case (_, 0) => (covar1.covariance, covar1.comoment)
      case _ =>
        val na = covar1.xstats.count.toDouble
        val nb = covar2.xstats.count.toDouble
        val n = na + nb

        val x1avg = covar1.xstats.avg
        val x2avg = covar2.xstats.avg
        val y1avg = covar1.ystats.avg
        val y2avg = covar2.ystats.avg

        val comoment = covar1.comoment + covar2.comoment + na * nb / n * (x1avg - x2avg) * (y1avg - y2avg)
        //          val covariance = (covar1.covariance + covar1.covariance) + (x1avg - x2avg)*(y1avg - y2avg) *  ((na * nb / (n - 1)))
        val covariance = comoment / n
        (covariance, comoment)
    }

    Covariance(covariance, comoment, xstats, ystats)
  }

  implicit class CovarianceOps(val covariance: Covariance) {

    def append(that: Covariance): Covariance = Covariance.append(covariance, that)
    def append(x: Double, y: Double): Covariance = Covariance.append(covariance, x, y)
    def |+|(that: Covariance): Covariance = this.append(that)
    def |+|(x: Double, y: Double): Covariance = this.append(x, y)

    /**
     * Compute the Pearsons correlation coefficient.
     * See [[https://en.wikipedia.org/wiki/Correlation_and_dependence#Pearson's_product-moment_coefficient]]
     */
    def pearsonCorrelation: Double = covariance.comoment / (math.sqrt(covariance.xstats.sse * covariance.ystats.sse))
  }

}
