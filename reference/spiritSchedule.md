# SPIRIT Schedule of Events

Converts a validated long schedule into a deterministic wide
enrolment/intervention/assessment table.

## Usage

``` r
spiritSchedule(trial, events, marker = "X")
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- events:

  Long schedule data frame using the documented event schema.

- marker:

  Default marker for scheduled cells.

## Value

A `spirit_schedule` object with validated long events, a wide table, and
ordered timepoint metadata.

## References

Chan AW et al. (2013). SPIRIT 2013 Statement.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
events <- data.frame(
  timepoint_id = c("screen", "week12"),
  timepoint_label = c("Screening", "Week 12"),
  timepoint_order = c(-1L, 1L),
  category = c("enrolment", "assessment"),
  activity = c("Eligibility", "Primary endpoint"),
  arm = NA_character_,
  scheduled = TRUE
)
spiritSchedule(trial, events)
#> <spirit_schedule> T1: 2 activities x 2 timepoints
#>    category         activity arm screen week12
#>   enrolment      Eligibility all      X       
#>  assessment Primary endpoint all             X
```
