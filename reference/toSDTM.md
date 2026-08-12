# Export Trial Data to CDISC SDTM-Shaped Domains

Builds deterministic, linked trial domains with explicit sponsor
mappings. The output is structurally SDTM-shaped; it is not a claim of
CDISC, regulator, or third-party validator conformance.

## Usage

``` r
toSDTM(
  trial,
  participants,
  adverse_events = NULL,
  findings = list(),
  study_id = trial@id,
  id_col = "id",
  arm_col = "arm",
  sex_col = NULL,
  birth_date_col = NULL,
  reference_start_col = NULL,
  reference_end_col = NULL,
  country_col = NULL,
  site_col = NULL,
  extra_domains = list()
)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- participants:

  One row per screened or enrolled participant.

- adverse_events:

  Optional adverse events accepted by
  [`aeSummary()`](https://x-biosignal.github.io/PhysioTrial/reference/aeSummary.md).

- findings:

  Named list containing normalized `VS`, `EG`, or `XP` findings data
  frames.

- study_id:

  Non-empty study identifier.

- id_col, arm_col:

  Participant ID and assigned-arm columns.

- sex_col, birth_date_col, reference_start_col, reference_end_col:

  Optional participant columns.

- country_col, site_col:

  Optional country and site columns.

- extra_domains:

  Named list of already constructed two-letter domains, such as output
  from
  [`PhysioClinical::toCDISC_QS()`](https://x-biosignal.github.io/PhysioClinical/reference/toCDISC_QS.html).

## Value

An `sdtm_export` containing datasets, manifest, and mapping metadata.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
participants <- data.frame(
  id = c("p1", "p2"),
  arm = c("active", "control")
)
export <- toSDTM(trial, participants)
export$datasets$DM
#>   STUDYID DOMAIN USUBJID SUBJID SITEID RFSTDTC RFENDTC BRTHDTC  SEX COUNTRY
#> 1      T1     DM   T1-p1     p1   <NA>    <NA>    <NA>    <NA> <NA>    <NA>
#> 2      T1     DM   T1-p2     p2   <NA>    <NA>    <NA>    <NA> <NA>    <NA>
#>   ARMCD     ARM
#> 1  ARM1  active
#> 2  ARM2 control
```
