# Apply an Analysis Set and Missing-Data Method

Filters outcome rows to analysis-set members. LOCF is included only for
legacy and sensitivity workflows; it is generally unsuitable for primary
confirmatory inference. Multiple imputation requires an explicit
callback and never silently substitutes a single imputation.

## Usage

``` r
analysisData(
  set,
  outcomes,
  id_col = "id",
  time_col = NULL,
  value_cols = NULL,
  missing = c("none", "locf", "multiple"),
  imputer = NULL,
  ...
)
```

## Arguments

- set:

  An `analysis_set`.

- outcomes:

  Outcome data frame.

- id_col:

  Participant-ID column in `outcomes`.

- time_col:

  Optional time column, required for LOCF.

- value_cols:

  Columns subject to missing-data processing. Defaults to every
  non-ID/non-time column.

- missing:

  One of `"none"`, `"locf"`, or `"multiple"`.

- imputer:

  Explicit multiple-imputation callback.

- ...:

  Arguments forwarded to `imputer`.

## Value

A filtered data frame, or a non-empty list of completed data frames for
a multiple-imputation callback.

## Examples

``` r
trial <- Trial("T1", c("A", "B"))
p <- data.frame(
  id = c("p1", "p2"), arm = c("A", "B"), randomized = TRUE
)
set <- intentionToTreat(trial, p)
outcomes <- data.frame(id = c("p1", "p1", "p2"), time = c(0, 1, 0),
                       value = c(NA, 2, 9))
analysisData(set, outcomes, time_col = "time", missing = "locf")
#>   id time value
#> 1 p1    0    NA
#> 2 p1    1     2
#> 3 p2    0     9
```
