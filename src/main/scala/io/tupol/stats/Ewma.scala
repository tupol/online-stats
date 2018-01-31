package io.tupol.stats

/**
 * Exponentially Weighted Moving Average Calculator
 *
 * `ewma = (1 - alpha) * previous_ewma + alpha * current_value`
 * <br>
 * We can think of alpha as `alpha = 1 / number_of_previous_samples`
 *
 */
trait Ewma[T] {
  def alpha: T
  def ewma: T
  def mean: T = ewma
}

