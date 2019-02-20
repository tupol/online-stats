package org.tupol.stats

object optionops {

  implicit def doubleToOption(x: Double): Option[Double] = Some(x)

  implicit class DoubleOptionOps(x: Option[Double]) {
    def add(that: Option[Double]) = addDoubleOptions(x, that)
    def +(that: Option[Double]) = add(that)
    def sub(that: Option[Double]) = subtractDoubleOptions(x, that)
    def -(that: Option[Double]) = sub(that)
    def mul(that: Option[Double]) = multiplyDoubleOptions(x, that)
    def *(that: Option[Double]) = mul(that)
    def div(that: Option[Double]) = divideDoubleOptions(x, that)
    def /(that: Option[Double]) = div(that)
    def >(that: Option[Double]) = greaterDoubleOptions(x, that)
    def >=(that: Option[Double]) = greaterEqDoubleOptions(x, that)
    def <(that: Option[Double]) = smallerDoubleOptions(x, that)
    def <=(that: Option[Double]) = smallerEqDoubleOptions(x, that)

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
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def greaterDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(x), Some(y)) => Some(x > y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def greaterEqDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(x), Some(y)) => Some(x >= y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def smallerDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(x), Some(y)) => Some(x < y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def smallerEqDoubleOptions(x: Option[Double], y: Option[Double]) = (x, y) match {
      case (Some(x), Some(y)) => Some(x <= y)
      case _ => None
    }
  }

  implicit def double2Option(value: Double): Option[Double] = value match { case Double.NaN => None; case _ => Some(value) }

}
