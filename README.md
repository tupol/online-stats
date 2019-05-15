# online-stats #

[![Maven Central](https://img.shields.io/maven-central/v/org.tupol/online-stats_2.11.svg)](https://mvnrepository.com/artifact/org.tupol/online-stats) &nbsp;
[![GitHub](https://img.shields.io/github/license/tupol/online-stats.svg)](https://github.com/tupol/online-stats/blob/master/LICENSE) &nbsp; 
[![Travis (.org)](https://img.shields.io/travis/tupol/online-stats.svg)](https://travis-ci.com/tupol/online-stats) &nbsp; 
[![Codecov](https://img.shields.io/codecov/c/github/tupol/online-stats.svg)](https://codecov.io/gh/tupol/online-stats) &nbsp;
[![Javadocs](https://www.javadoc.io/badge/org.tupol/online-stats_2.11.svg)](https://www.javadoc.io/doc/org.tupol/online-stats_2.11) &nbsp;
[![Gitter](https://badges.gitter.im/online-stats/community.svg)](https://gitter.im/online-stats/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) &nbsp; 
[![Twitter](https://img.shields.io/twitter/url/https/_tupol.svg?color=%2317A2F2)](https://twitter.com/_tupol) &nbsp; 

## Scope ##

Naive implementation of a few online statistical algorithms.

The idea behind this implementation is to be used as a tool for stateful streaming computations.

Algos covered so far:
- 4 statistical moments and the derived features:
  - average
  - variance and standard deviation
  - skewness
  - kurtosis
- covariance
- exponentially weighted moving averages and variance

Algos to be researched:
- exponentially weighted moving skewness
- exponentially weighted moving kurtosis

Using a more formal and mature library like **[Apache Commons Math](http://commons.apache.org/proper/commons-math/)** 
is probably a better idea for production applications, but this is also tested against it. 

## Description ##

The main concepts introduced in this library are the `Stats`, `EWeightedStats` (exponentially
weighted stats), `VectorStats` and `Covariance`. Each of them can be composed using either the
`append` or the `|+|` functions. 

For example, if we have a sequence of numbers, we can compute the statistics like this:

```scala
  val xs1 = Seq(1.0, 3.0)
  val stats1: Stats = xs1.foldLeft(Stats.zeroDouble)((s, x) => s |+| x)
  val xs2 = Seq(5.0, 7.0)
  val stats2: Stats = xs2.foldLeft(Stats.zeroDouble)((s, x) => s |+| x)
  val totalStats = stats1 |+| stats2
  val newStats = totalStats |+| 4.0
```

The `Stats` type with the `|+|` operation also form a *monoid*, since `|+|` has an *identity* 
(unit) element, `Stats.zeroDouble`, and it is *associative*. 
 
Also the `|+|` operation is also *commutative*, which makes appealing for distributed computing 
as well.
 
Same goes for `VectorStats` and `Covariance`.

`EWeightedStats` is an exception for now, as two `EWeightedStats` instances can not be composed.
However, the `|+|` works between an `EWeightedStats` instance and a double. 


## Complexity ##

| Feature                         | Space Complexity (*O*) | Time Complexity (*O*) |
| ------------------------------- | :--------------------: | :-------------------: |
| Count, Sum, Min, Max            | ***O***(1)  (1 * MU)   | ***O***(1)            |
| Average                         | ***O***(1)  (2 * MU)   | ***O***(1)            |
| Variance, Standard deviation    | ***O***(1)  (3 * MU)   | ***O***(1)            |
| Skewness                        | ***O***(1)  (4 * MU)   | ***O***(1)            |
| Kurtosis                        | ***O***(1)  (5 * MU)   | ***O***(1)            |
| Exponentially weighted average  | ***O***(1)  (2 * MU)   | ***O***(1)            |
| Exponentially weighted variance | ***O***(1)  (2 * MU)   | ***O***(1)            |

*MU*: Memory Unit, e.g. Int: 4 bytes, Double 8: bytes

## Demos and Examples ##

The [`streaming-anomalies-demos`](https://github.com/tupol/streaming-anomalies-demos) project was created to explore and demonstrate some basic use cases for the `online-stats` library.

## References ##

- [*"Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments"* by Philippe Pebay](http://prod.sandia.gov/techlib/access-control.cgi/2008/086212.pdf)
- [*"The Exponentially Weighted Moving Variance"* by J. F. Macgregor and T. J. Harris](https://www.tandfonline.com/doi/abs/10.1080/00224065.1993.11979433)
- [*"Incremental calculation of weighted mean and variance"* by Tony Finch, February 2009](http://people.ds.cam.ac.uk/fanf2/hermes/doc/antiforgery/stats.pdf)
- [Bessel Correction](https://en.wikipedia.org/wiki/Bessel%27s_correction)
- [Skewness](https://en.wikipedia.org/wiki/Skewness)
- [Kurtosis](https://en.wikipedia.org/wiki/Kurtosis)
- [Pearson's_correlation_coefficient](https://en.wikipedia.org/wiki/Correlation_and_dependence#Pearson's_product-moment_coefficient)
