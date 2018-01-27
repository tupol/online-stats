package io.tupol.stats

import io.tupol.stats.StatsOps._

/**
 * Implicit decorators for Covariance types
 */
object CovarianceOps {

  trait CovarianceOps[T] {
    /**
     * Compose two covariance instances to obtain a new Covariance
     * See "Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments" by Philippe Pebay
     * ([[http://prod.sandia.gov/techlib/access-control.cgi/2008/086212.pdf]])
     * @param covar1
     * @param covar2
     * @return
     */
    def append(covar1: Covariance[T], covar2: Covariance[T]): Covariance[T]

    /**
     * Append a tuple to the current Covariance to obtain a new covariance
     * See [[https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online]] for ideas
     * @param covar
     * @param x
     * @param y
     * @return
     */
    def append(covar: Covariance[T], x: T, y: T): Covariance[T]
  }

  /**
   * Generic Covariance decorator
   * @tparam T
   */
  trait OnlineCovarianceOps[T] {
    def append(covariance: Covariance[T])(implicit ops: CovarianceOps[T]): Covariance[T]
    def |+|(covariance: Covariance[T])(implicit ops: CovarianceOps[T]): Covariance[T] = append(covariance)
    def append(x: T, y: T)(implicit ops: CovarianceOps[T]): Covariance[T]
    def |+|(x: T, y: T)(implicit ops: CovarianceOps[T]): Covariance[T] = append(x, y)
  }

  /**
   * Decorator for Covariances of Doubles
   * @param covariance
   */
  implicit class CovarianceOnlineOps[T](val covariance: Covariance[T]) extends OnlineCovarianceOps[T] {
    override def append(covariance: Covariance[T])(implicit ops: CovarianceOps[T]): Covariance[T] = ops.append(this.covariance, covariance)
    override def append(x: T, y: T)(implicit ops: CovarianceOps[T]): Covariance[T] = ops.append(this.covariance, x, y)
  }

  implicit class DoubleCorrelationOps(val stats: Covariance[Double]) {
    /**
     * Compute the Pearsons correlation coefficient. See [[https://en.wikipedia.org/wiki/Correlation_and_dependence#Pearson's_product-moment_coefficient]]
     * @return
     */
    def pearsonCorrelation: Double = stats.comoment / (math.sqrt(stats.xstats.sse * stats.ystats.sse))
  }

  implicit object DoubleCovarianceOps extends CovarianceOps[Double] {
    /** @inheritdoc */
    def append(covar: Covariance[Double], x: Double, y: Double): Covariance[Double] = {

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

    /** @inheritdoc */
    def append(covar1: Covariance[Double], covar2: Covariance[Double]): Covariance[Double] = {

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

  }

}

