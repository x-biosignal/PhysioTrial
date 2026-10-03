# Designing and running a randomized trial with PhysioTrial

PhysioTrial provides reproducible trial-design infrastructure:
randomization with allocation concealment, CONSORT/SPIRIT operations,
explicit analysis populations, power and sample-size helpers, and
deterministic CDISC-shaped exports. This vignette walks one small trial
end to end on synthetic data.

These exports are auditable and structurally checked; they are **not** a
claim of formal CDISC, third-party validator, or regulatory acceptance.

``` r

library(PhysioTrial)
```

## 1. Define the trial

``` r

trial <- Trial(
  "rehab-01",
  arms = c("active", "control"),
  strata = list(site = c("north", "south"))
)
arms(trial)
#> [1] "active"  "control"
```

## 2. Plan the sample size

``` r

# Two-arm comparison of a continuous endpoint (e.g. change in gait speed).
sampleSizeContinuous(delta = 0.12, sd = 0.20, power = 0.8)
#> <trial_power> two-sample continuous
#>   effect: delta=0.12, sd=0.2, d=0.6
#>   power 0.80 | alpha 0.05 (2-sided) | allocation 1:1
#>   n per arm: control 45, treatment 45  (total 90)
```

## 3. Randomize with allocation concealment

Stratified permuted-block randomization is reproducible from a seed.
Future treatment assignments stay masked in the sealed sequence.

``` r

participants <- data.frame(
  id = sprintf("P%03d", 1:8),
  site = rep(c("north", "south"), each = 4)
)
sequence <- randomize(
  trial,
  method = "stratified_block",
  participants = participants,
  seed = 2026
)
assignments(sequence)
#>   order participant_id    stratum block_id  arm
#> 1     1           P001 site=north        1 <NA>
#> 2     2           P002 site=north        1 <NA>
#> 3     3           P003 site=north        1 <NA>
#> 4     4           P004 site=north        1 <NA>
#> 5     5           P005 site=south        2 <NA>
#> 6     6           P006 site=south        2 <NA>
#> 7     7           P007 site=south        2 <NA>
#> 8     8           P008 site=south        2 <NA>
```

Allocations are revealed one at a time, through an auditable request:

``` r

allocation <- nextAllocation(sequence, requester = "site")
allocation$arm
#> [1] "active"
sequence <- allocation$sequence
```

## 4. CONSORT flow

CONSORT counts and the flow diagram are inspectable without a rendering
dependency (`render = FALSE`).

``` r

flow <- data.frame(
  participant_id = c("P001", "P002"),
  eligible = c(TRUE, FALSE),
  randomized = c(TRUE, FALSE),
  arm = c("active", NA),
  received_allocated = c(TRUE, NA),
  followup_complete = c(TRUE, NA),
  analysed = c(TRUE, NA),
  pre_exclusion_reason = c(NA, "ineligible"),
  not_received_reason = NA_character_,
  followup_reason = NA_character_,
  analysis_exclusion_reason = NA_character_
)
diagram <- consortDiagram(trial, flow, render = FALSE)
consortCounts(diagram)
#>                     node_id                         stage     arm
#> 1                  assessed                      assessed    <NA>
#> 2                  excluded excluded_before_randomization    <NA>
#> 3                randomized                    randomized    <NA>
#> 4            arm1_allocated                     allocated  active
#> 5             arm1_received                      received  active
#> 6         arm1_not_received               did_not_receive  active
#> 7             arm1_followup             followup_complete  active
#> 8  arm1_followup_incomplete           followup_incomplete  active
#> 9             arm1_analysed                      analysed  active
#> 10   arm1_excluded_analysis             excluded_analysis  active
#> 11           arm2_allocated                     allocated control
#> 12            arm2_received                      received control
#> 13        arm2_not_received               did_not_receive control
#> 14            arm2_followup             followup_complete control
#> 15 arm2_followup_incomplete           followup_incomplete control
#> 16            arm2_analysed                      analysed control
#> 17   arm2_excluded_analysis             excluded_analysis control
#>                                     label n
#> 1                Assessed for eligibility 2
#> 2           Excluded before randomization 1
#> 3                              Randomized 1
#> 4                     Allocated to active 1
#> 5         Received allocated intervention 1
#> 6  Did not receive allocated intervention 0
#> 7                      Follow-up complete 1
#> 8                    Follow-up incomplete 0
#> 9                                Analysed 1
#> 10                 Excluded from analysis 0
#> 11                   Allocated to control 0
#> 12        Received allocated intervention 0
#> 13 Did not receive allocated intervention 0
#> 14                     Follow-up complete 0
#> 15                   Follow-up incomplete 0
#> 16                               Analysed 0
#> 17                 Excluded from analysis 0
```

## 5. Analysis populations

Intention-to-treat and per-protocol membership consume explicit
finalized assignments.

``` r

participant_state <- data.frame(
  id = c("P001", "P002", "screen-01"),
  arm = c("active", "control", NA),
  randomized = c(TRUE, TRUE, FALSE),
  adherent = c(TRUE, FALSE, NA)
)
itt <- intentionToTreat(trial, participant_state)
pp  <- perProtocol(trial, participant_state, adherent_col = "adherent")
analysisExclusions(pp)
#>   participant_id     arm         reason
#> 1           P002 control    nonadherent
#> 2      screen-01    <NA> not_randomized
```

## 6. CDISC-shaped export

Exports keep the source-to-`USUBJID` map explicit and are structurally
validated.

``` r

sdtm <- toSDTM(trial, participant_state)
validateCDISC(sdtm)
#> <cdisc_validation> valid: 0 error(s), 0 warning(s)
sdtm$metadata$id_map
#>   source_id            USUBJID
#> 1      P001      rehab-01-P001
#> 2      P002      rehab-01-P002
#> 3 screen-01 rehab-01-screen-01
```

## Where to go next

[`?PhysioTrial`](https://x-biosignal.github.io/PhysioTrial/reference/PhysioTrial-package.md)
lists every entry point: blinding and unblinding audits
([`blindingManager()`](https://x-biosignal.github.io/PhysioTrial/reference/blindingManager.md),
[`unblind()`](https://x-biosignal.github.io/PhysioTrial/reference/unblind.md)),
SPIRIT schedules
([`spiritSchedule()`](https://x-biosignal.github.io/PhysioTrial/reference/spiritSchedule.md)),
adverse-event summaries
([`aeSummary()`](https://x-biosignal.github.io/PhysioTrial/reference/aeSummary.md)),
endpoint wrappers
([`endpointANCOVA()`](https://x-biosignal.github.io/PhysioTrial/reference/endpointANCOVA.md),
[`endpointMMRM()`](https://x-biosignal.github.io/PhysioTrial/reference/endpointMMRM.md)),
and the ADaM export
([`toADaM()`](https://x-biosignal.github.io/PhysioTrial/reference/toADaM.md)).
