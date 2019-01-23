package org.tupol.stats.utils

import scala.language.implicitConversions

/**
 *
 * @note all of the ''equals'' methods defined here, which accept ''tolerance'' as an input,
 *       do '''not''' define any ''equality relation'', because the ''transitivity'' doesn't hold.
 *
 */
object Tolerance {

  implicit def tolerance2Double(t: Tolerance): Double = t.epsilon

  implicit class ProductOps(val p1: Product) extends AnyVal {
    def approx(p2: Product)(implicit tolerance: Tolerance): Boolean = productsEqual(p1, p2)
  }

  implicit class SeqOps(val ss1: Seq[Any]) extends AnyVal {
    def approx(ss2: Seq[Any])(implicit tolerance: Tolerance): Boolean = seqsEqual(ss1, ss2)
  }

  def productsEqual(p1: Product, p2: Product)(implicit eps: Tolerance): Boolean =
    p1.productArity == p2.productArity &&
      p1.productPrefix == p2.productPrefix &&
      seqsEqual(p1.productIterator.toSeq, p2.productIterator.toSeq)

  def seqsEqual(ss1: Seq[_], ss2: Seq[_])(implicit eps: Tolerance): Boolean =
    ss1.size == ss2.size && ss1.zip(ss2).foldLeft(true) {
      case (false, _) => false
      case (_, (fst: Seq[_], snd: Seq[_])) => seqsEqual(fst, snd)
      case (_, (fst: Product, snd: Product)) => productsEqual(fst, snd)
      case (_, (fst: Double, snd: Double)) => math.abs(fst - snd) <= eps
      case (_, (fst: Any, snd: Any)) => fst == snd
      case _ => false
    }

}

case class Tolerance(epsilon: Double) extends AnyVal

trait DefaultTolerance {
  implicit val tolerance = Tolerance(1E-12)
}
