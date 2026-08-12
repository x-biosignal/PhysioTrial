# Score Pocock-Simon Imbalance

Internal RNG-free scorer used by minimization and its numeric
validation.

## Usage

``` r
.ps_imbalance(
  state,
  levels,
  arm,
  arms,
  ratio,
  weights,
  measure = c("range", "variance", "sd")
)
```

## Arguments

- state:

  Nested count list indexed by factor, level, and arm.

- levels:

  Named factor-level vector for one participant.

- arm:

  Candidate arm.

- arms:

  Ordered arm labels.

- ratio:

  Named allocation weights.

- weights:

  Named factor weights.

- measure:

  One of `"range"`, `"variance"`, or `"sd"`.

## Value

Numeric imbalance score for the candidate arm.
