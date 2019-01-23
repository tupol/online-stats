package org.tupol.stats

/**
 * Exponentially Weighted Moving Stats Calculator
 *
 * `exponentially weighted average = (1 - alpha) * previous_ewma + alpha * current_value`
 *
 */
trait EWeightedStats[T] extends Stats[T] {
  def alpha: Double
  def count: Double
  /** The total sum of all the values */
  def sum: T
  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   */
  def variance(biasCorrected: Boolean = false): T
  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   */
  def stdev(biasCorrected: Boolean = false): T
  /** Skewness. See [[https://en.wikipedia.org/wiki/Skewness]] */
  def skewness: T
  /** Kurtosis. See [[https://en.wikipedia.org/wiki/Kurtosis]] */
  def kurtosis: T
  /** Moment 1 */
  def m1: T
  /** Moment 2 */
  def m2: T
  /** Moment 3 */
  def m3: T
  /** Moment 4 */
  def m4: T
}

