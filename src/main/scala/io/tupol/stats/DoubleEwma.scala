package io.tupol.stats

case class DoubleEwma(alpha: Double, ewma: Double) extends Ewma[Double] {
  require(alpha > 0.0 && alpha < 1.0, "Alpha must be between 0 and 1.")
}

object DoubleEwma {

  import EwmaOps._

  def fromAlphaAndDoubles(alpha: Double, population: Iterable[Double]): Ewma[Double] =
    population.tail.foldLeft(DoubleEwma(alpha, population.head))((result, x) => result |+| x)

  def fromVolumeAndDouble(n: Long, initialValue: Double) = DoubleEwma(1.0 / n, initialValue)

}
