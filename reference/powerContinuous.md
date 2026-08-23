# Power of a two-arm continuous endpoint at a given sample size

Power of a two-arm continuous endpoint at a given sample size

## Usage

``` r
powerContinuous(n1, delta, sd, sig_level = 0.05, ratio = 1, sides = 2L)
```

## Arguments

- n1:

  Control-arm sample size.

- delta, sd, sig_level, ratio, sides:

  As in
  [`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md).

## Value

The achieved power (numeric in 0-1).

## See also

[`sampleSizeContinuous()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeContinuous.md),
[`powerCurve()`](https://x-biosignal.github.io/PhysioTrial/reference/powerCurve.md)

## Examples

``` r
powerContinuous(64, delta = 5, sd = 10)     # ~0.80
#> [1] 0.8014596
```
