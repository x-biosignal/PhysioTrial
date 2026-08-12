# MDC/MCID Responder Analysis

Delegates each subject's dual-threshold classification to
[`PhysioClinical::classifyResponder()`](https://x-biosignal.github.io/PhysioClinical/reference/classifyResponder.html)
and summarizes true responders with Wilson score intervals. Missing
outcomes remain explicit and are excluded from the rate denominator.

## Usage

``` r
responderAnalysis(
  data,
  baseline,
  followup,
  treatment,
  mdc,
  mcid,
  direction = c("increase", "decrease"),
  control = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  Participant-level data.

- baseline, followup, treatment:

  Scalar column names.

- mdc, mcid:

  Positive MDC and MCID thresholds.

- direction:

  Whether improvement is an increase or decrease.

- control:

  Optional control arm placed first.

- conf_level:

  Wilson interval confidence level.

## Value

A `responder_analysis` with subject classifications and arm summaries.
