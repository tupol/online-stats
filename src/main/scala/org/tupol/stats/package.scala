package org.tupol

/**
 *
 */
package object stats {

  /**
   * Stats package Vector type of Doubles
   */
  type DVector = IndexedSeq[Double]

  /**
   * Stats package Vector type of Longs
   */
  type LVector = IndexedSeq[Long]

  implicit def lVector2dVector(lv: LVector): DVector = lv.map(_.toDouble)

  implicit class DoubleOptionOps(x: Option[Double]) {
    def add(that: Option[Double]) = addDoubleOptions(x, that)
    def |+|(that: Option[Double]) = add(that)
    def sub(that: Option[Double]) = subtractDoubleOptions(x, that)
    def |-|(that: Option[Double]) = sub(that)
    def mul(that: Option[Double]) = multiplyDoubleOptions(x, that)
    def |*|(that: Option[Double]) = mul(that)
    def div(that: Option[Double]) = divideDoubleOptions(x, that)
    def |/|(that: Option[Double]) = div(that)
    def |>|(that: Option[Double]) = greaterDoubleOptions(x, that)

    /**
     * Add two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def addDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) + y.getOrElse(0.0))
    }

    /**
     * Subtract two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def subtractDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) - y.getOrElse(0.0))
    }

    /**
     * Multiply two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def multiplyDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) * y.getOrElse(0.0))
    }

    /**
     * Divide two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def divideDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(_), Some(0.0)) => None
      case (Some(x), Some(y)) => Some(x / y)
      case (_, _) => None
    }

    /**
     * Subtract two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def greaterDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(x), Some(y)) => Some(x > y)
      case _ => None
    }
  }

  implicit def double2Option(value: Double): Option[Double] = value match { case Double.NaN => None; case _ => Some(value) }

  /**
   * Probability density function
   * @param x
   * @param mean
   * @param variance `sigma ^ 2`
   * @param degenerateSolution sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  def pdf(x: Double, mean: Double, variance: Double, degenerateSolution: Double = 1E-12) = {
    import math._
    if (variance == 0)
      if (x == mean) 1.0 else degenerateSolution
    else
      (1 / sqrt(2 * Pi * variance)) * exp(-(x - mean) * (x - mean) / (2 * variance))
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
  def probability(x: Double, mean: Double, stdev: Double, range: Double, epsilon: Double,
    degenerateSolution: Double = 1E-12) = {
    if (stdev == 0 || x == mean) 1.0
    else {
      require(epsilon > 0.0 && epsilon <= range / 10.0, s"epsilon ($epsilon) must be be a number greater than zero (0) " +
        s"and at least an order of magnitude smaller than the range ($range).")
      val splits = math.round(range / epsilon).toInt
      val from = x - range
      val variance = stdev * stdev
      epsilon * (0 until 2 * splits).map(s => pdf(from + (0.5 + s) * epsilon, mean, variance, degenerateSolution)).sum
    }
  }

}
