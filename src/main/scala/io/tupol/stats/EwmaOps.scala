package io.tupol.stats

object EwmaOps {

  trait EwmaOps[T] {
    def append(x: T): Ewma[T]
    def |+|(x: T): Ewma[T] = append(x)
  }

  implicit class DoubleEwmaOps(val ewma: Ewma[Double]) extends EwmaOps[Double] {
    override def append(x: Double) = DoubleEwma(ewma.alpha, (1 - ewma.alpha) * ewma.ewma + ewma.alpha * x)
    override def |+|(x: Double) = append(x)
  }

}
