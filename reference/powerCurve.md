# Power curve over a range of sample sizes or effect sizes

Power curve over a range of sample sizes or effect sizes

## Usage

``` r
powerCurve(
  delta,
  sd,
  n1 = NULL,
  deltas = NULL,
  sig_level = 0.05,
  ratio = 1,
  sides = 2L
)
```

## Arguments

- delta, sd, sig_level, ratio, sides:

  As in
  [`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md).

- n1:

  A vector of control-arm sizes to evaluate (vary n at fixed effect), or
  `NULL` to vary the effect instead.

- deltas:

  A vector of effect differences to evaluate at fixed `n1` (used when
  `n1` is a single value).

## Value

A `data.frame` with columns `n1`, `delta`, `power`.

## See also

[`powerContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/powerContinuous.md)

## Examples

``` r
powerCurve(delta = 5, sd = 10, n1 = seq(20, 120, 10))
#>     n1 delta     power
#> 1   20     5 0.3379390
#> 2   30     5 0.4778965
#> 3   40     5 0.5981469
#> 4   50     5 0.6968934
#> 5   60     5 0.7752659
#> 6   70     5 0.8358223
#> 7   80     5 0.8816025
#> 8   90     5 0.9155872
#> 9  100     5 0.9404272
#> 10 110     5 0.9583410
#> 11 120     5 0.9711088
```
