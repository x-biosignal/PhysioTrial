# Estimate the baseline-outcome correlation (for ANCOVA planning)

From pilot data - a pair of numeric vectors, a `data.frame` with a
baseline and follow-up column, or a `PhysioCohort` `cohortDesign()` plus
column names - estimate \\\rho\\ to feed
[`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md).

## Usage

``` r
estimateBaselineCorrelation(
  baseline,
  followup = NULL,
  cols = NULL,
  method = "pearson"
)
```

## Arguments

- baseline:

  A numeric vector of baseline values, a `data.frame`, or a
  `PhysioCohort`.

- followup:

  A numeric vector of follow-up values (when `baseline` is a vector).

- cols:

  Length-2 character vector naming the baseline and follow-up columns
  (when `baseline` is a `data.frame` / cohort design).

- method:

  Correlation method (default `"pearson"`).

## Value

The estimated correlation (numeric).

## See also

[`sampleSizeANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/sampleSizeANCOVA.md)
