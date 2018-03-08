package io.tupol.stats

object StatsOps {

  implicit def double2Stats(value: Double): Stats[Double] = DoubleStats.fromDoubles(value)
  implicit def doubles2Stats(value: Iterable[Double]): Stats[Double] = DoubleStats.fromDoubles(value)
  implicit def dVector2Stats(value: DVector): Stats[DVector] = DVectorStats.fromDVector(value)
  implicit def dVectors2Stats(value: Iterable[DVector]): Stats[DVector] = DVectorStats.fromDVectors(value)
  implicit val doubleStatsOps = DoubleStatsOps
  implicit val dVectorStatsOps = DVectorStatsOps

  trait StatsOps[T] {
    def append(x: Stats[T], y: Stats[T]): Stats[T]
    def pdf(s: Stats[T], x: T, degenerateSolution: Double): T
  }

  trait OnlineStats[T] {
    def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T]
    def |+|(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = append(stats)
    def pdf(x: T, degenerateSolution: Double)(implicit ops: StatsOps[T]): T
  }

  implicit class OnlineStatsOps[T](val stats: Stats[T]) extends OnlineStats[T] {
    override def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = ops.append(this.stats, stats)
    override def pdf(x: T, degenerateSolution: Double)(implicit ops: StatsOps[T]) = ops.pdf(stats, x, degenerateSolution)
  }

}
