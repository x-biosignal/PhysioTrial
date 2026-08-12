# Per-Protocol Analysis Set

Starts from ITT and excludes participants with a major protocol
deviation or caller-selected nonadherence/incompletion flags. Multiple
exclusion reasons are retained in a deterministic order.

## Usage

``` r
perProtocol(
  trial,
  participants,
  deviations = NULL,
  id_col = "id",
  arm_col = "arm",
  randomized_col = "randomized",
  deviation_id_col = "participant_id",
  major_col = "major",
  code_col = "code",
  adherent_col = NULL,
  completed_col = NULL
)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- participants:

  One row per source participant.

- deviations:

  Optional protocol-deviation data frame.

- id_col, arm_col, randomized_col:

  Column names in `participants`.

- deviation_id_col, major_col, code_col:

  Columns in `deviations`.

- adherent_col, completed_col:

  Optional logical columns in `participants`.

## Value

A `PP` `analysis_set`.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
participants <- data.frame(
  id = c("p1", "p2", "screen"),
  arm = c("active", "control", NA),
  randomized = c(TRUE, TRUE, FALSE),
  adherent = c(TRUE, FALSE, NA)
)
perProtocol(trial, participants, adherent_col = "adherent")
#> <analysis_set> T1 PP: 1 included, 2 excluded
```
