# Sample size for a two-arm binary / responder endpoint

Comparison of two proportions (e.g. responder rates), via the normal
approximation with unequal allocation; matches
[`stats::power.prop.test`](https://rdrr.io/r/stats/power.prop.test.html)
in the balanced case.

## Usage

``` r
sampleSizeBinary(
  p1,
  p2,
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

- p1:

  Control-arm proportion.

- p2:

  Treatment-arm proportion.

- power, sig_level, ratio, sides, dropout, cluster_size, icc:

  As in
  [`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md).

## Value

A `trial_power` object.

## See also

[`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md),
[`powerBinary()`](https://x-biosignal.github.io/PhysioTrial/reference/powerBinary.md)

## Examples

``` r
sampleSizeBinary(p1 = 0.30, p2 = 0.50)      # responder-rate 30% vs 50%
#> <trial_power> two-proportion
#>   effect: p1=0.3, p2=0.5
#>   power 0.80 | alpha 0.05 (2-sided) | allocation 1:1
#>   n per arm: control 93, treatment 93  (total 186)
```
