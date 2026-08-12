# PhysioTrial

Reproducible randomization, allocation concealment, treatment-code
management, unblinding audits, CONSORT/SPIRIT study operations,
adverse-event summaries, blinding assessment, explicit analysis
populations, and endpoint helpers for clinical trials. It also creates
linked SDTM-shaped and ADaM-shaped datasets with deterministic
structural validation and define.xml metadata stubs.

``` r

library(PhysioTrial)

trial <- Trial(
  "rehab-01",
  arms = c("active", "control"),
  strata = list(site = c("north", "south"))
)
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

# Future treatment assignments remain masked.
assignments(sequence)

allocation <- nextAllocation(sequence, requester = "site")
allocation$arm
sequence <- allocation$sequence
```

CONSORT counts and DOT remain inspectable without a rendering
dependency:

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
```

ITT and per-protocol membership consume explicit finalized assignments:

``` r

participant_state <- data.frame(
  id = c("P001", "P002", "screen-01"),
  arm = c("active", "control", NA),
  randomized = c(TRUE, TRUE, FALSE),
  adherent = c(TRUE, FALSE, NA)
)
itt <- intentionToTreat(trial, participant_state)
pp <- perProtocol(
  trial,
  participant_state,
  adherent_col = "adherent"
)
analysisExclusions(pp)
```

Trial exports keep the source-to-`USUBJID` map and sponsor mappings
explicit:

``` r

sdtm <- toSDTM(trial, participant_state)
validateCDISC(sdtm)
sdtm$metadata$id_map
```

These are auditable, structurally checked exports. They are not a claim
of formal CDISC, third-party validator, or regulatory acceptance.

Randomization, Allocation Concealment and Blinding for Clinical Trials

## Installation

``` r

install.packages("PhysioTrial",
  repos = c("https://x-biosignal.r-universe.dev", "https://cloud.r-project.org"))
```

## Part of the x-biosignal ecosystem

See the [x-biosignal](https://github.com/x-biosignal) organization and
[x-biosignal.r-universe.dev](https://x-biosignal.r-universe.dev) for the
full package suite.
