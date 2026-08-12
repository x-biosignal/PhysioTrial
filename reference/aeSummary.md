# Summarize Adverse Events by Trial Arm

Reports event counts separately from participant incidence, and counts
SAE status from the explicit `serious` field rather than a CTCAE grade
cutoff.

## Usage

``` r
aeSummary(events, allocation, arms = NULL)
```

## Arguments

- events:

  List of
  [`adverseEvent()`](https://x-biosignal.github.io/PhysioTrial/reference/adverseEvent.md)
  rows or a data frame using the same schema.

- allocation:

  Data frame with unique `participant_id` and `arm`.

- arms:

  Optional complete arm order, including zero-event arms.

## Value

An `ae_summary` with arm, severity, and term tables.

## Examples

``` r
allocation <- data.frame(
  participant_id = c("p1", "p2"),
  arm = c("active", "control")
)
events <- list(adverseEvent(
  "p1", "Nausea", as.Date("2026-01-02"),
  ctcae_grade = 1
))
aeSummary(events, allocation, arms = c("active", "control"))
#> <ae_summary>
#>      arm n_randomized n_ae n_participants_ae n_sae n_participants_sae ae_risk
#>   active            1    1                 1     0                  0       1
#>  control            1    0                 0     0                  0       0
#>  sae_risk
#>         0
#>         0
```
