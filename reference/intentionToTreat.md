# Intention-to-Treat Analysis Set

Includes every randomized participant in the originally assigned arm.
Missing outcomes, adherence, and post-randomization events do not alter
membership.

## Usage

``` r
intentionToTreat(
  trial,
  participants,
  id_col = "id",
  arm_col = "arm",
  randomized_col = "randomized"
)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- participants:

  One row per source participant.

- id_col, arm_col, randomized_col:

  Column names in `participants`.

## Value

An `analysis_set` with included rows, member IDs, and explicit
exclusions.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
participants <- data.frame(
  id = c("p1", "p2", "screen"),
  arm = c("active", "control", NA),
  randomized = c(TRUE, TRUE, FALSE)
)
intentionToTreat(trial, participants)
#> <analysis_set> T1 ITT: 2 included, 1 excluded
```
