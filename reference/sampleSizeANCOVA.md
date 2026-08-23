# Sample size for a baseline-adjusted (ANCOVA) continuous endpoint

The recommended pre-post rehabilitation analysis: adjusting the
follow-up outcome for its baseline reduces the residual variance by a
factor \\(1-\rho^2)\\, where \\\rho\\ is the baseline-outcome
correlation - shrinking the required sample versus an unadjusted
comparison.

## Usage

``` r
sampleSizeANCOVA(
  delta,
  sd,
  baseline_cor,
  power = 0.8,
  sig_level = 0.05,
  ratio = 1,
  sides = 2L,
  dropout = 0,
  cluster_size = NULL,
  icc = NULL
)
```

## Arguments

- delta, sd, power, sig_level, ratio, sides, dropout, cluster_size, icc:

  As in
  [`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md).
  `sd` is the raw (unadjusted) outcome SD.

- baseline_cor:

  Correlation \\\rho\\ between baseline and follow-up outcome (0-1); the
  variance is scaled by \\(1-\rho^2)\\.

## Value

A `trial_power` object.

## References

Frison & Pocock (1992), Stat Med; Borm et al. (2007), J Clin Epi.

## See also

[`sampleSizeChangeScore()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeChangeScore.md),
[`estimateBaselineCorrelation()`](https://x-biosignal.github.io/PhysioTrial/reference/estimateBaselineCorrelation.md)

## Examples

``` r
sampleSizeANCOVA(delta = 5, sd = 10, baseline_cor = 0.6)   # < 64/arm
#> <trial_power> ANCOVA (baseline-adjusted)
#>   effect: delta=5, sd=10, baseline_cor=0.6, d_adjusted=0.625
#>   power 0.80 | alpha 0.05 (2-sided) | allocation 1:1
#>   n per arm: control 42, treatment 42  (total 84)
```
