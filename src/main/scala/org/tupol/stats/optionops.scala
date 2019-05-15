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

object optionops {

  implicit def double2Option(x: Double): Option[Double] = if (x.isNaN) None else Some(x)

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
    private def addDoubleOptions(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) + y.getOrElse(0.0))
    }

    /**
     * Subtract two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def subtractDoubleOptions(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) - y.getOrElse(0.0))
    }

    /**
     * Multiply two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def multiplyDoubleOptions(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
      case (None, None) => None
      case _ => Some(x.getOrElse(0.0) * y.getOrElse(0.0))
    }

    /**
     * Divide two optional doubles, returning None if both are None and the sum of the values if one is defined
     * @param x
     * @param y
     * @return
     */
    private def divideDoubleOptions(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
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
    private def greaterDoubleOptions(x: Option[Double], y: Option[Double]): Option[Boolean] = (x, y) match {
      case (Some(x), Some(y)) => Some(x > y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def greaterEqDoubleOptions(x: Option[Double], y: Option[Double]): Option[Boolean] = (x, y) match {
      case (Some(x), Some(y)) => Some(x >= y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def smallerDoubleOptions(x: Option[Double], y: Option[Double]): Option[Boolean] = (x, y) match {
      case (Some(x), Some(y)) => Some(x < y)
      case _ => None
    }

    /**
     * Compare two optional doubles, returning None if both are None and comparison result of both are defined
     * @param x
     * @param y
     * @return
     */
    private def smallerEqDoubleOptions(x: Option[Double], y: Option[Double]): Option[Boolean] = (x, y) match {
      case (Some(x), Some(y)) => Some(x <= y)
      case _ => None
    }
  }

}
