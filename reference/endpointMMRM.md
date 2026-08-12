# MMRM Endpoint Analysis

Delegates longitudinal modelling and estimated marginal means to
`PhysioClinStats`. No alternate MMRM or complete-case ANCOVA fallback is
implemented here.

## Usage

``` r
endpointMMRM(
  data,
  response,
  treatment,
  time,
  subject,
  baseline = NULL,
  covariates = NULL,
  control = NULL,
  target_time = NULL,
  covariance = c("unstructured", "ar1", "compound-symmetry", "toeplitz"),
  df = c("kenward-roger", "satterthwaite"),
  conf_level = 0.95
)
```

## Arguments

- data:

  Longitudinal endpoint data.

- response, treatment, time, subject:

  Scalar column names.

- baseline:

  Optional repeated subject-level baseline column.

- covariates:

  Optional additional fixed-effect columns.

- control:

  Control-arm label.

- target_time:

  Optional one requested time level.

- covariance:

  MMRM covariance structure.

- df:

  Denominator degrees-of-freedom method.

- conf_level:

  Confidence level.

## Value

An `endpoint_analysis` carrying standardized treatment contrasts and the
underlying `PhysioClinStats` result objects.
