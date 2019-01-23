package org.tupol.stats

/**
 * Represent statistics data interface
 */
trait Stats[T] {
  /**
   * The number of values in the given set
   * @return
   */
  def count: Double
  /**
   * The minimum value of the given set
   * @return
   */
  def min: T
  /**
   * The maximum value of the given set
   * @return
   */
  def max: T
  /**
   * The mean value in the given set
   * @return
   */
  def avg: T = m1
  def average: T = m1
  def mean: T = m1
  /**
   * The total sum of all the values
   * @return
   */
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
  /**
   * The total sum of all the squared errors. Essentially this is just an alias for the second moment.
   * @return
   */
  def sse: T = m2
  /**
   * Skewness. See [[https://en.wikipedia.org/wiki/Skewness]]
   * @return
   */
  def skewness: T
  /**
   * Kurtosis. See [[https://en.wikipedia.org/wiki/Kurtosis]]
   * @return
   */
  def kurtosis: T
  /**
   * Moment 1
   * @return
   */
  def m1: T
  /**
   * Moment 2
   * @return
   */
  def m2: T
  /**
   * Moment 3
   * @return
   */
  def m3: T
  /**
   * Moment 4
   * @return
   */
  def m4: T
}
