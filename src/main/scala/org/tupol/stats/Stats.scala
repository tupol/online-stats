package org.tupol.stats

/** Represent statistics data interface */
trait Stats[T] {
  /** The number of values in the given set */
  def count: Double
  /** The minimum value of the given set */
  def min: T
  /** The maximum value of the given set */
  def max: T
  /** * The mean value in the given set */
  def avg: T = m1
  def average: T = m1
  def mean: T = m1
  /** The total sum of all the values */
  def sum: T
  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def variance(biasCorrected: Boolean = false): T
  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def stdev(biasCorrected: Boolean = false): T
  /** The total sum of all the squared errors. Essentially this is just an alias for the second moment. */
  def sse: T = m2
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
