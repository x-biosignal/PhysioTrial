# Sample size for a change-from-baseline continuous endpoint

Analysis of the change score (follow-up minus baseline). The change SD
is \\\sigma\sqrt{2(1-\rho)}\\; change-score analysis beats an unadjusted
follow-up comparison only when \\\rho \> 0.5\\, and is never better than
ANCOVA.

## Usage

``` r
sampleSizeChangeScore(
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

- delta, sd, baseline_cor, power, sig_level, ratio, sides, dropout,
  cluster_size, icc:

  As in
  [`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md).

## Value

A `trial_power` object.

## See also

[`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md)
