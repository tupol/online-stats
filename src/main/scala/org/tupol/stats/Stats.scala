package org.tupol.stats

/**
 * The Stats implementation for doubles
 *
 * @param count the number of values in the given set
 * @param min the minimum value of the given set
 * @param max the maximum value of the given set
 * @param sum sum of all elements
 * @param m2 moment 2
 * @param m3 moment 3
 * @param m4 moment 4
 */
case class Stats(count: Double, min: Double, max: Double, sum: Double, m2: Double, m3: Double, m4: Double) {

  import math._

  /** moment 1 */
  def m1 = sum.toDouble / count
  /** * The mean value in the given set */
  def avg: Double = m1
  def average: Double = m1
  def mean: Double = m1

  /** The total sum of all the squared errors. Essentially this is just an alias for the second moment. */
  def sse: Double = m2

  /**
   * Variance. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def variance(biasCorrected: Boolean = false): Double = {
    val correction = if (biasCorrected && count > 1) count.toDouble / (count - 1) else 1.0
    (sse / count) * correction
  }

  /**
   * Standard deviation. See [[https://en.wikipedia.org/wiki/Variance]]
   * @param biasCorrected Should the Bessel correction be applied? See also [[https://en.wikipedia.org/wiki/Bessel%27s_correction]]
   * @return
   */
  def stdev(biasCorrected: Boolean = false): Double = math.sqrt(variance(biasCorrected))

  /** Skewness. See [[https://en.wikipedia.org/wiki/Skewness]] */
  def skewness: Double = if (variance(false) <= 1E-20) 0.0 else sqrt(count.toDouble) * m3 / pow(m2, 1.5)

  /** @inheritdoc */
  def kurtosis: Double = if (variance(false) <= 1E-20) 0.0 else count * m4 / (m2 * m2) - 3.0

}

/** Calculates statistics data */
object Stats {

  /**
   * Calculates statistics data of the [[Iterable]] of Double's
   *
   * @param population non-empty Iterable[Double]
   * @return [[Stats]]
   */
  def fromDoubles(population: Iterable[Double]): Stats =
    if (population.isEmpty)
      zeroDouble
    else {
      val n = population.size
      val total = population.sum
      val m1 = total / n.toDouble
      val sxe: Iterable[(Double, Double, Double)] =
        population.map { d => val er = (d - m1); val er2 = er * er; val er3 = er * er2; val er4 = er * er3; (er2, er3, er4) }
      val m2 = sxe.map(_._1).sum
      val m3 = sxe.map(_._2).sum
      val m4 = sxe.map(_._3).sum

      new Stats(n, population.min, population.max, total, m2, m3, m4)
    }

  /** Initialises the stats from a single double. */
  def fromDouble(value: Double): Stats = Stats(1, value, value, value, 0.0, 0.0, 0.0)

  val zeroDouble: Stats = Stats(0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  def zeroDouble(value: Double): Stats = Stats(0, value, value, value, 0.0, 0.0, 0.0)

  def append(x: Stats, value: Double): Stats = append(x, Stats.fromDouble(value))

  def append(x: Stats, y: Stats): Stats = {
    if (x.count == 0) y
    else if (y.count == 0) x
    else {
      val n = (x.count + y.count).toDouble
      val total = x.sum + y.sum
      val min = math.min(x.min, y.min)
      val max = math.max(x.max, y.max)
      val delta1 = y.avg - x.avg
      val delta2 = delta1 * delta1
      val delta3 = delta1 * delta2
      val delta4 = delta1 * delta3
      val n2 = n * n
      val n3 = n * n2
      def nx = x.count
      val nx2 = nx * nx
      def ny = y.count
      val ny2 = ny * ny
      // See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
      //        val m1 = (nx * x.m1 + ny * y.m1) / n // Moment 1
      //        val m1 = total / n.toDouble // Same as above, but less computations
      val m2 = (x.m2 + y.m2) + delta2 * nx * ny / n // Moment 2
      val m3 = (x.m3 + y.m3) +
        1.0 * delta3 * nx * ny * (nx - ny) / n2 +
        3.0 * delta1 * (nx * y.m2 - ny * x.m2) / n
      val m4 = (x.m4 + y.m4) +
        1.0 * delta4 * nx * ny * (nx2 - nx * ny + ny2) / n3 +
        6.0 * delta2 * (nx2 * y.m2 + ny2 * x.m2) / n2 +
        4.0 * delta1 * (nx * y.m3 - ny * x.m3) / n

      Stats(n.toLong, min, max, total, m2, m3, m4)
    }
  }

  /**
   * Probability density function
   * @param x
   * @param mean
   * @param variance `sigma ^ 2`
   * @param degenerateSolution sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  def pdf(x: Double, mean: Double, variance: Double, degenerateSolution: Double) = {
    import math._
    if (variance == 0)
      if (x == mean) 1.0 else degenerateSolution
    else
      (1 / sqrt(2 * Pi * variance)) * exp(-(x - mean) * (x - mean) / (2 * variance))
  }

  /**
   * Compute the probability of a value `x` to be in the interval `[x-range, x+range]`.
   * @param x the value to compute the probability for
   * @param mean the average of the population
   * @param stdev standard deviation
   * @param range the around the x value for which we compute the probability, e.g. probability( x +- range);
   *              to get probabilities close to 1, the range should be 3 * stdev
   * @param epsilon how small should the incremental interval for computing the probability be; it should be
   *                at least an order of magnitude smaller than the `range`
   * @param degenerateSolution sometimes so it happens that the distribution is flat... what then?
   * @return
   */
  def probability(x: Double, mean: Double, stdev: Double, range: Double, epsilon: Double,
    degenerateSolution: Double) = {
    if (stdev == 0 || x == mean) 1.0
    else {
      require(epsilon > 0.0 && epsilon <= range / 10.0, s"epsilon ($epsilon) must be be a number greater than zero (0) " +
        s"and at least an order of magnitude smaller than the range ($range).")
      val splits = math.round(range / epsilon).toInt
      val from = x - range
      val variance = stdev * stdev
      epsilon * (0 until 2 * splits).map(s => pdf(from + (0.5 + s) * epsilon, mean, variance, degenerateSolution)).sum
    }
  }

  def pdf(s: Stats, x: Double, degenerateSolution: Double = 1E-12): Double =
    pdf(x, s.avg, s.variance(), degenerateSolution)

  def probability(s: Stats, x: Double, range: Double, epsilon: Double, degenerateSolution: Double = 1E-12): Double =
    probability(x, s.mean, s.stdev(), range, epsilon, degenerateSolution)

  def probabilityNSigma(s: Stats, x: Double, epsilon: Double, nSigma: Double = 3, degenerateSolution: Double = 1E-12): Double =
    probability(s, x, nSigma * s.stdev(), epsilon, degenerateSolution)

  implicit class StatsOps(val stats: Stats) {

    def append(that: Stats): Stats = Stats.append(stats, that)
    def append(that: Double): Stats = Stats.append(stats, that)
    def |+|(that: Stats): Stats = this.append(that)
    def |+|(that: Double): Stats = this.append(that)
    def probability(x: Double, range: Double, epsilon: Double, degenerateSolution: Double = 1E-12): Double =
      Stats.probability(stats, x, range, epsilon, degenerateSolution)
    def probabilityNSigma(x: Double, epsilon: Double, nSigma: Double, degenerateSolution: Double = 1E-12): Double =
      Stats.probabilityNSigma(stats, x, epsilon, nSigma, degenerateSolution)

  }

}
