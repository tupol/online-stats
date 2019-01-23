package org.tupol.stats

object StatsOps {

  implicit def double2Stats(value: Double): Stats[Double] = DoubleStats.fromDouble(value)
  implicit def doubles2Stats(value: Iterable[Double]): Stats[Double] = DoubleStats.fromDoubles(value)
  implicit def dVector2Stats(value: DVector): Stats[DVector] = DVectorStats.fromDVector(value)
  implicit def dVectors2Stats(value: Iterable[DVector]): Stats[DVector] = DVectorStats.fromDVectors(value)
  implicit val doubleStatsOps = DoubleStatsOps
  implicit val dVectorStatsOps = DVectorStatsOps

  trait StatsOps[T] {
    def append(x: Stats[T], y: Stats[T]): Stats[T]
    def append(x: Stats[T], value: T): Stats[T]
    def pdf(s: Stats[T], x: T, degenerateSolution: Double): T
    def probability(s: Stats[T], x: T, range: T, epsilon: T, degenerateSolution: Double): T
    def probabilityNSigma(s: Stats[T], x: T, epsilon: T, nSigma: T, degenerateSolution: Double): T
  }

  trait OnlineStats[T] {
    def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T]
    def append(value: T)(implicit ops: StatsOps[T]): Stats[T]
    def |+|(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = append(stats)
    def |+|(value: T)(implicit ops: StatsOps[T]): Stats[T] = append(value)
    def pdf(x: T, degenerateSolution: Double)(implicit ops: StatsOps[T]): T
    def probability(x: T, range: T, epsilon: T, degenerateSolution: Double)(implicit ops: StatsOps[T]): T
    def probabilityNSigma(x: T, epsilon: T, nSigma: T, degenerateSolution: Double)(implicit ops: StatsOps[T]): T
  }

  implicit class OnlineStatsOps[T](val stats: Stats[T]) extends OnlineStats[T] {
    override def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = ops.append(this.stats, stats)
    override def append(value: T)(implicit ops: StatsOps[T]): Stats[T] = ops.append(stats, value)
    override def pdf(x: T, degenerateSolution: Double = 1E-12)(implicit ops: StatsOps[T]) =
      ops.pdf(stats, x, degenerateSolution)
    override def probability(x: T, range: T, epsilon: T, degenerateSolution: Double = 1E-12)(implicit ops: StatsOps[T]): T =
      ops.probability(stats, x, range, epsilon, degenerateSolution)
    override def probabilityNSigma(x: T, epsilon: T, nSigma: T, degenerateSolution: Double = 1E-12)(implicit ops: StatsOps[T]): T =
      ops.probabilityNSigma(stats, x, epsilon, nSigma, degenerateSolution)
  }

}
