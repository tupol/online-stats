# online-stats #


## Scope ##

Naive implementation of a few online statistical algorithms.

The idea behind this implementation is to be used as a tool for stateful streaming computations.

Algos covered so far:
- 4 statistical moments and the derived features:
  - average
  - variance and standard deviation
  - skewness
  - kurtosis
- exponentially weighted moving averages and variance
- covariance

Algos to be researched:
- exponentially weighted moving skewness
- exponentially weighted moving kurtosis


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
