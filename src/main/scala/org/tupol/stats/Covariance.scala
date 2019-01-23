package org.tupol.stats

/**
 * Generic Covariance
 *
 * @param covariance
 * @param comoment
 * @param xstats
 * @param ystats
 * @tparam T
 */
case class Covariance[T](covariance: T, comoment: T, xstats: Stats[T], ystats: Stats[T])

/**
 * Covariance companion with Covariance factories
 */
object Covariance {

  /**
   * Compute Covariance out of two sequences of numbers.
   * The Covariance has no bias correction
   * @param xs
   * @param ys
   * @return
   */
  def fromDoubles(xs: Seq[Double], ys: Seq[Double]): Covariance[Double] = {

    assert(xs.size == ys.size, "Can not correlate vectors of different sizes.")
    assert(xs.size > 0, "Can not correlate empty vectors.")

    val alignedInputs = xs.zip(ys)

    val n = alignedInputs.size
    val x = DoubleStats.fromDoubles(alignedInputs.map(_._1))
    val y = DoubleStats.fromDoubles(alignedInputs.map(_._2))

    val sumXtimesY = alignedInputs.map { case (a, b) => a * b }.sum

    val com = xs.zip(ys).map { case (a, b) => (a - x.avg) * (b - y.avg) }.sum

    val cov = sumXtimesY / n - x.avg * y.avg

    Covariance(cov, com, x, y)

  }

  def fromDoubles(x: Double, y: Double): Covariance[Double] = fromDoubles(Seq(x), Seq(y))

  val zeroDouble: Covariance[Double] = Covariance(0, 0, DoubleStats.zeroDouble, DoubleStats.zeroDouble)

}
