# Capture an Adverse Event

Creates one typed adverse-event row. CTCAE severity and regulatory
seriousness are stored independently; a serious event is not inferred
from a grade threshold. MedDRA fields preserve externally assigned
coding metadata and are not checked against a bundled terminology.

## Usage

``` r
adverseEvent(
  participant_id,
  term,
  onset_date,
  end_date = as.Date(NA),
  ctcae_grade,
  serious = FALSE,
  causality = c("not_assessed", "unrelated", "unlikely", "possible", "probable",
    "definite"),
  action = NA_character_,
  outcome = c("unknown", "not_recovered", "recovering", "recovered",
    "recovered_with_sequelae", "fatal"),
  meddra_pt = NA_character_,
  meddra_code = NA_character_,
  meddra_version = NA_character_,
  metadata = list()
)
```

## Arguments

- participant_id:

  Non-empty participant identifier.

- term:

  Verbatim adverse-event term.

- onset_date:

  Event onset date.

- end_date:

  Optional event end date.

- ctcae_grade:

  Integer CTCAE grade from 1 through 5.

- serious:

  Whether the event meets a seriousness criterion.

- causality:

  Investigator causality assessment.

- action:

  Optional action taken.

- outcome:

  Controlled event outcome.

- meddra_pt:

  Optional MedDRA preferred term.

- meddra_code:

  Optional MedDRA code.

- meddra_version:

  Optional MedDRA version.

- metadata:

  Free-form list stored as a list-column.

## Value

A one-row `adverse_event` data frame.

## References

National Cancer Institute. CTCAE version 5.0.

## Examples

``` r
adverseEvent(
  "P001", "Nausea", as.Date("2026-01-02"),
  ctcae_grade = 2, causality = "possible"
)
#>   participant_id   term onset_date end_date ctcae_grade serious causality
#> 1           P001 Nausea 2026-01-02     <NA>           2   FALSE  possible
#>   action outcome meddra_pt meddra_code meddra_version metadata
#> 1   <NA> unknown      <NA>        <NA>           <NA>         
```
