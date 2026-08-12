# Export Trial Analysis Sets to ADaM-Shaped Data

Builds an ADSL population table and a generic BDS analysis table from
explicit analysis sets. This structural export is not a claim of formal
ADaM conformance.

## Usage

``` r
toADaM(
  set,
  subject_data,
  bds = NULL,
  study_id = set$trial_id,
  id_col = "id",
  treatment_col = "arm",
  population_flags = NULL
)
```

## Arguments

- set:

  The authoritative primary `analysis_set`.

- subject_data:

  One row per source participant.

- bds:

  Optional generic BDS source data frame. A named list may contain an
  `ADBDS` source plus already constructed analysis datasets such as
  `ADQS`.

- study_id:

  Non-empty study identifier.

- id_col, treatment_col:

  Subject ID and originally assigned treatment columns.

- population_flags:

  Optional named list of other `analysis_set` objects from the same
  source population.

## Value

An `adam_export` containing datasets, manifest, and population metadata.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
subjects <- data.frame(
  id = c("p1", "p2", "screen"),
  arm = c("active", "control", NA),
  randomized = c(TRUE, TRUE, FALSE)
)
itt <- intentionToTreat(trial, subjects)
export <- toADaM(itt, subjects)
export$datasets$ADSL
#>   STUDYID   USUBJID SUBJID  TRT01P TRT01PN ITTFL PPROTFL randomized
#> 1      T1     T1-p1     p1  active       1     Y    <NA>       TRUE
#> 2      T1     T1-p2     p2 control       2     Y    <NA>       TRUE
#> 3      T1 T1-screen screen    <NA>      NA     N    <NA>      FALSE
```
