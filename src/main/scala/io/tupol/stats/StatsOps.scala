package io.tupol.stats

object StatsOps {

  implicit def double2Stats(value: Double): Stats[Double] = DoubleStats.fromDoubles(value)
  implicit def doubleIterable2Stats(value: Iterable[Double]): Stats[Double] = DoubleStats.fromDoubles(value)
  implicit val doubleStatsOps = DoubleStatsOps

  trait StatsOps[T] {
    def append(x: Stats[T], y: Stats[T]): Stats[T]
    def pdf(s: Stats[T], x: T, degenerateSolution: T): T
  }

  trait OnlineStats[T] {
    def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T]
    def |+|(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = append(stats)
    def pdf(x: T, degenerateSolution: T)(implicit ops: StatsOps[T]): T
  }

  implicit class OnlineStatsOps[T](val stats: Stats[T]) extends OnlineStats[T] {
    override def append(stats: Stats[T])(implicit ops: StatsOps[T]): Stats[T] = ops.append(this.stats, stats)
    override def pdf(x: T, degenerateSolution: T)(implicit ops: StatsOps[T]) = ops.pdf(stats, x, degenerateSolution)
  }

}
