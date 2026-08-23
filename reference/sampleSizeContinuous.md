# Sample size for a two-arm continuous endpoint

Two-sample comparison of means. Delegates the balanced case's core to
the noncentral-t (matching
[`stats::power.t.test`](https://rdrr.io/r/stats/power.t.test.html)) and
adds unequal allocation, cluster design effect, and dropout inflation.

## Usage

``` r
sampleSizeContinuous(
  delta,
  sd,
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

- delta:

  Target difference in means (treatment - control).

- sd:

  Common (residual) standard deviation of the endpoint.

- power:

  Target power (default 0.8).

- sig_level:

  Two-sided (or one-sided, see `sides`) alpha (default 0.05).

- ratio:

  Allocation ratio n2/n1 (treatment/control; default 1).

- sides:

  1 or 2 (default 2).

- dropout:

  Expected proportion lost to follow-up; inflates enrolment (default 0).

- cluster_size:

  Average subjects per cluster for a cluster-randomized design (`NULL` =
  individually randomized).

- icc:

  Intra-cluster correlation (required with `cluster_size`).

## Value

A `trial_power` object (`n1`, `n2`, `n_total`, `n_enrolled`, design
effect, clusters, ...).

## See also

[`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md),
[`sampleSizeBinary()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeBinary.md),
[`powerContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/powerContinuous.md)

## Examples

``` r
sampleSizeContinuous(delta = 5, sd = 10)          # d = 0.5 -> 64/arm
#> <trial_power> two-sample continuous
#>   effect: delta=5, sd=10, d=0.5
#>   power 0.80 | alpha 0.05 (2-sided) | allocation 1:1
#>   n per arm: control 64, treatment 64  (total 128)
sampleSizeContinuous(delta = 5, sd = 10, dropout = 0.2)
#> <trial_power> two-sample continuous
#>   effect: delta=5, sd=10, d=0.5
#>   power 0.80 | alpha 0.05 (2-sided) | allocation 1:1
#>   n per arm: control 64, treatment 64  (total 128)
#>   enrol 160 to allow 20% dropout
```
