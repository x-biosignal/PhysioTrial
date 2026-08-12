# CONSORT Participant-Flow Diagram

Builds reconciled CONSORT participant counts and Graphviz DOT.
DiagrammeR rendering is optional; the inspectable counts and DOT are
always returned.

## Usage

``` r
consortDiagram(
  trial,
  flow,
  render = requireNamespace("DiagrammeR", quietly = TRUE),
  show_reasons = TRUE
)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- flow:

  One row per assessed participant using the documented flow schema.

- render:

  Whether to create a
  [`DiagrammeR::grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
  widget.

- show_reasons:

  Whether exclusion/disposition reasons appear in labels.

## Value

A `consort_diagram` object containing nodes, edges, reason counts, DOT,
and an optional widget.

## References

Schulz KF, Altman DG, Moher D (2010). CONSORT 2010 Statement.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
flow <- data.frame(
  participant_id = c("p1", "p2", "p3"),
  eligible = c(TRUE, TRUE, FALSE),
  randomized = c(TRUE, TRUE, FALSE),
  arm = c("active", "control", NA),
  received_allocated = c(TRUE, TRUE, NA),
  followup_complete = c(TRUE, TRUE, NA),
  analysed = c(TRUE, TRUE, NA),
  pre_exclusion_reason = c(NA, NA, "ineligible"),
  not_received_reason = c(NA, NA, NA),
  followup_reason = c(NA, NA, NA),
  analysis_exclusion_reason = c(NA, NA, NA)
)
consortDiagram(trial, flow, render = FALSE)
#> <consort_diagram> T1: 3 assessed, 2 randomized
#>                   node_id                         stage     arm
#>                  assessed                      assessed    <NA>
#>                  excluded excluded_before_randomization    <NA>
#>                randomized                    randomized    <NA>
#>            arm1_allocated                     allocated  active
#>             arm1_received                      received  active
#>         arm1_not_received               did_not_receive  active
#>             arm1_followup             followup_complete  active
#>  arm1_followup_incomplete           followup_incomplete  active
#>             arm1_analysed                      analysed  active
#>    arm1_excluded_analysis             excluded_analysis  active
#>            arm2_allocated                     allocated control
#>             arm2_received                      received control
#>         arm2_not_received               did_not_receive control
#>             arm2_followup             followup_complete control
#>  arm2_followup_incomplete           followup_incomplete control
#>             arm2_analysed                      analysed control
#>    arm2_excluded_analysis             excluded_analysis control
#>                                   label n
#>                Assessed for eligibility 3
#>           Excluded before randomization 1
#>                              Randomized 2
#>                     Allocated to active 1
#>         Received allocated intervention 1
#>  Did not receive allocated intervention 0
#>                      Follow-up complete 1
#>                    Follow-up incomplete 0
#>                                Analysed 1
#>                  Excluded from analysis 0
#>                    Allocated to control 1
#>         Received allocated intervention 1
#>  Did not receive allocated intervention 0
#>                      Follow-up complete 1
#>                    Follow-up incomplete 0
#>                                Analysed 1
#>                  Excluded from analysis 0
```
