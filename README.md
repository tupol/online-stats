# online-stats


## Scope

Naive implementation of a few online statistical algorithms.

The idea behind this implementation is to be used as a tool for stateful streaming computations.

Algos covered so far:
- 4 statistical moments and the derived features:
  - average
  - variance and standard deviation
  - skewness
  - kurtosis
- covariance


## Complexity

| Feature                      | State Memory Requirements | Complexity |
| ---------------------------- | :-----------------------: | :--------: |
| Count, Sum, Min, Max         | 1 * MU                    | 1          |
| Average                      | 2 * MU                    | 1          |
| Variance, Standard deviation | 3 * MU                    | 1          |
| Skewness                     | 4 * MU                    | 1          |
| Kurtosis                     | 5 * MU                    | 1          |
| Exponentially moving average | 2 * MU                    | 1          |

*MU*: Memory Unit, e.g. Int: 4 bytes, Double 8: bytes


## References

- [Bessel Correction](https://en.wikipedia.org/wiki/Bessel%27s_correction)
- [Skewness](https://en.wikipedia.org/wiki/Skewness)
- [Kurtosis](https://en.wikipedia.org/wiki/Kurtosis)
- [Pearson's_correlation_coefficient](https://en.wikipedia.org/wiki/Correlation_and_dependence#Pearson's_product-moment_coefficient)
- ["Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments" by Philippe Pebay](http://prod.sandia.gov/techlib/access-control.cgi/2008/086212.pdf)

