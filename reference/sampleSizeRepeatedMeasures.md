# Sample size for a repeated-measures continuous endpoint

Comparison of the mean of `n_measurements` post-baseline measurements
under a compound-symmetry correlation `correlation` between them. The
effective variance is \\\sigma^2 \[1 + (r-1)\rho\]/r\\, so more visits
help most when the within-subject correlation is low.

## Usage

``` r
sampleSizeRepeatedMeasures(
  delta,
  sd,
  n_measurements,
  correlation,
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

- n_measurements:

  Number of post-baseline measurements per subject.

- correlation:

  Compound-symmetry correlation between measurements (0-1).

## Value

A `trial_power` object.

## References

Frison & Pocock (1992), Stat Med.

## See also

[`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md)
