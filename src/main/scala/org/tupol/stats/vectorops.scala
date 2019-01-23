package org.tupol.stats

/** Additional operations for linalg.Vector */
object vectorops {

  /** Additional operations for Double with DVector */
  implicit class DoubleOps(val scalar: Double) extends AnyVal {

    def +(vector: DVector): DVector =
      opByDim(vector, (x1: Double, x2: Double) => x1 + x2)

    def *(vector: DVector): DVector =
      opByDim(vector, (x1: Double, x2: Double) => x1 * x2)

    def -(vector: DVector): DVector =
      opByDim(vector, (x1: Double, x2: Double) => x1 - x2)

    def /(vector: DVector): DVector =
      opByDim(vector, (x1: Double, x2: Double) => x1 / x2)

    private[stats] def opByDim(vector: DVector, op: (Double, Double) => Double): DVector =
      vector.map { op(scalar, _) }

  }

  /** Added operations by dimension */
  implicit class DVectorOps(val self: DVector) extends AnyVal {

    /** Squared distances by dimension */
    def sqdistByDim(that: DVector): DVector = {
      require(self.size == that.size, "vectors should have same size")
      self.zip(that).map {
        case (x1, x2) => math.pow(x1 - x2, 2)
      }
    }

    /** Add 2 vectors, dimension by dimension */
    def +(that: DVector): DVector = {
      require(self.size == that.size)
      op(self, that, (x1: Double, x2: Double) => x1 + x2)
    }

    /** Add each value in this vector with the provided scalar value */
    def +(scalar: Double): DVector =
      op(self, scalar, (x1: Double, x2: Double) => x1 + x2)

    /** Subtract that vector from this vector, dimension by dimension */
    def -(that: DVector): DVector = {
      require(self.size == that.size)
      op(self, that, (x1: Double, x2: Double) => x1 - x2)
    }

    /** Subtract each value in this vector with the provided scalar value */
    def -(scalar: Double): DVector = {
      op(self, scalar, (x1: Double, x2: Double) => x1 - x2)
    }

    /** Change the sign of each value inside the vector */
    def unary_- : DVector = self.map(-_)

    /** Calculate the exponential by dimension */
    def exp: DVector = map(math.exp)

    /** Calculates the square root by dimension */
    def sqrt: DVector = map(math.sqrt)

    /** Calculate the square by dimension */
    def sqr: DVector = map(x => x * x)

    /** Calculate the power at exponent by dimension */
    def pow(exponent: Double): DVector = map(x => math.pow(x, exponent))

    /** Multiply this vector with that vector, dimension by dimension */
    def *(that: DVector): DVector = {
      require(self.size == that.size)
      op(self, that, (x1: Double, x2: Double) => x1 * x2)
    }

    /** Multiply each value in this vector with the provided scalar value */
    def *(scalar: Double): DVector =
      op(self, scalar, (x1: Double, x2: Double) => x1 * x2)

    /** Divide each value in this vector with the provided scalar value */
    def /(scalar: Double): DVector =
      op(self, scalar, (x1: Double, x2: Double) => x1 / x2)

    /** Divide this vector with that vector, dimension by dimension */
    def /(that: DVector): DVector = {
      require(self.size == that.size)
      op(self, that, (x1: Double, x2: Double) => x1 / x2)
    }

    def map(op: (Double) => Double): DVector = this.op(self, op)

    private[stats] def op(v1: DVector, v2: DVector, op: (Double, Double) => Double): DVector =
      v1.zip(v2).map { case (x1, x2) => op(x1, x2) }

    private[stats] def op(v1: DVector, scalar: Double, op: (Double, Double) => Double): DVector =
      v1.map { op(_, scalar) }

    private[stats] def op(v1: DVector, op: (Double) => Double): DVector =
      v1.map(op)
  }

  /** Added operations by dimension */
  implicit class LVectorOps(val self: LVector) extends AnyVal {

    /** Squared distances by dimension */
    def sqdistByDim(that: LVector): LVector = {
      require(self.size == that.size, "vectors should have same size")
      self.zip(that).map {
        case (x1, x2) => math.pow((x1 - x2).toDouble, 2).toLong
      }
    }

    /** Add each value in this vector with the provided scalar value */
    def +(scalar: Long): LVector =
      op(self, scalar, (x1: Long, x2: Long) => x1 + x2)

    /** Add 2 vectors, dimension by dimension */
    def +(that: LVector): LVector = {
      require(self.size == that.size)
      op(self, that, (x1: Long, x2: Long) => x1 + x2)
    }

    /** Subtract each value in this vector with the provided scalar value */
    def -(scalar: Long): LVector =
      op(self, scalar, (x1: Long, x2: Long) => x1 - x2)

    /** Subtract that vector from this vector, dimension by dimension */
    def -(that: LVector): LVector = {
      require(self.size == that.size)
      op(self, that, (x1: Long, x2: Long) => x1 - x2)
    }

    /** Change the sign of each value inside the vector */
    def unary_- : LVector = self.map(-_)

    /** Calculate the exponential by dimension */
    def exp: LVector = map(x => math.exp(x.toDouble).toLong)

    /** Calculates the square root by dimension */
    def sqrt: LVector = map(x => math.sqrt(x.toDouble).toLong)

    /** Calculate the square by dimension */
    def sqr: LVector = map(x => x * x)

    /** Calculate the power at exponent by dimension */
    def pow(exponent: Double): DVector = map(x => math.pow(x.toDouble, exponent).toLong)

    /** Multiply each value in this vector with the provided scalar value */
    def *(scalar: Long): LVector =
      op(self, scalar, (x1: Long, x2: Long) => x1 * x2)

    /** Multiply this vector with that vector, dimension by dimension */
    def *(that: LVector): LVector = {
      require(self.size == that.size)
      op(self, that, (x1: Long, x2: Long) => x1 * x2)
    }

    /** Divide each value in this vector with the provided scalar value */
    def /(scalar: Long): LVector =
      op(self, scalar, (x1: Long, x2: Long) => x1 / x2)

    /** Divide this vector with that vector, dimension by dimension */
    def /(that: LVector): LVector = {
      require(self.size == that.size)
      op(self, that, (x1: Long, x2: Long) => x1 / x2)
    }

    def map(op: (Long) => Long): LVector = this.op(self, op)

    private[stats] def op(v1: LVector, v2: LVector, op: (Long, Long) => Long): LVector =
      v1.zip(v2).map { case (x1, x2) => op(x1, x2) }

    private[stats] def op(v1: LVector, scalar: Long, op: (Long, Long) => Long): LVector =
      v1.map { op(_, scalar) }

    private[stats] def op(v1: LVector, op: (Long) => Long): LVector =
      v1.map(op)
  }

}
