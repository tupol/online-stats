package io.tupol

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

}
