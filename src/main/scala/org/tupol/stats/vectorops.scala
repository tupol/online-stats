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

/** Additional operations for linalg.Vector */
object vectorops {

  /** Additional operations for Double with DVector */
  implicit class DoubleOps(val scalar: Double) extends AnyVal {

    def +(vector: Vector): Vector =
      opByDim(vector, (x1: Double, x2: Double) => x1 + x2)

    def *(vector: Vector): Vector =
      opByDim(vector, (x1: Double, x2: Double) => x1 * x2)

    def -(vector: Vector): Vector =
      opByDim(vector, (x1: Double, x2: Double) => x1 - x2)

    def /(vector: Vector): Vector =
      opByDim(vector, (x1: Double, x2: Double) => x1 / x2)

    private[stats] def opByDim(vector: Vector, op: (Double, Double) => Double): Vector =
      vector.map { op(scalar, _) }

  }

  /** Added operations by dimension */
  implicit class DVectorOps(val self: Vector) extends AnyVal {

    /** Squared distances by dimension */
    def sqdistByDim(that: Vector): Vector = {
      require(self.size == that.size, s"The vectors should have same size but the fist has a size of ${self.size} and the second has a size of ${that.size}.")
      self.zip(that).map {
        case (x1, x2) => math.pow(x1 - x2, 2)
      }
    }

    /** Add 2 vectors, dimension by dimension */
    def +(that: Vector): Vector = {
      require(self.size == that.size, s"The vectors should have same size but the fist has a size of ${self.size} and the second has a size of ${that.size}.")
      op(self, that, (x1: Double, x2: Double) => x1 + x2)
    }

    /** Add each value in this vector with the provided scalar value */
    def +(scalar: Double): Vector =
      op(self, scalar, (x1: Double, x2: Double) => x1 + x2)

    /** Subtract that vector from this vector, dimension by dimension */
    def -(that: Vector): Vector = {
      require(self.size == that.size, s"The vectors should have same size but the fist has a size of ${self.size} and the second has a size of ${that.size}.")
      op(self, that, (x1: Double, x2: Double) => x1 - x2)
    }

    /** Subtract each value in this vector with the provided scalar value */
    def -(scalar: Double): Vector = {
      op(self, scalar, (x1: Double, x2: Double) => x1 - x2)
    }

    /** Change the sign of each value inside the vector */
    def unary_- : Vector = self.map(-_)

    /** Chance each value from the vector to its absolute value */
    def abs: Vector = self.map(math.abs)

    /** Calculate the exponential by dimension */
    def exp: Vector = map(math.exp)

    /** Calculates the square root by dimension */
    def sqrt: Vector = map(math.sqrt)

    /** Calculate the square by dimension */
    def sqr: Vector = map(x => x * x)

    /** Calculate the power at exponent by dimension */
    def pow(exponent: Double): Vector = map(x => math.pow(x, exponent))

    /** Multiply this vector with that vector, dimension by dimension */
    def *(that: Vector): Vector = {
      require(self.size == that.size, s"The vectors should have same size but the fist has a size of ${self.size} and the second has a size of ${that.size}.")
      op(self, that, (x1: Double, x2: Double) => x1 * x2)
    }

    /** Multiply each value in this vector with the provided scalar value */
    def *(scalar: Double): Vector =
      op(self, scalar, (x1: Double, x2: Double) => x1 * x2)

    /** Divide this vector with that vector, dimension by dimension */
    def /(that: Vector): Vector = {
      require(self.size == that.size, s"The vectors should have same size but the fist has a size of ${self.size} and the second has a size of ${that.size}.")
      op(self, that, (x1: Double, x2: Double) => x1 / x2)
    }

    /** Divide each value in this vector with the provided scalar value */
    def /(scalar: Double): Vector =
      op(self, scalar, (x1: Double, x2: Double) => x1 / x2)

    def map(op: (Double) => Double): Vector = this.op(self, op)

    private[stats] def op(v1: Vector, v2: Vector, op: (Double, Double) => Double): Vector =
      v1.zip(v2).map { case (x1, x2) => op(x1, x2) }

    private[stats] def op(v1: Vector, scalar: Double, op: (Double, Double) => Double): Vector =
      v1.map { op(_, scalar) }

    private[stats] def op(v1: Vector, op: (Double) => Double): Vector =
      v1.map(op)
  }

}
