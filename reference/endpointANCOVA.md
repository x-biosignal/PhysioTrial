# ANCOVA Endpoint Analysis

Fits change-from-baseline or follow-up ANCOVA with treatment referenced
to an explicit control. Reported Cohen d values are unadjusted
change-score effects delegated to
[`PhysioCore::cohensD()`](https://x-biosignal.r-universe.dev/PhysioCore/reference/cohensD.html)
and are separate from the adjusted ANCOVA estimate.

## Usage

``` r
endpointANCOVA(
  data,
  followup,
  baseline,
  treatment,
  covariates = NULL,
  control = NULL,
  response_scale = c("change", "followup"),
  conf_level = 0.95
)
```

## Arguments

- data:

  Participant-level endpoint data.

- followup, baseline, treatment:

  Scalar column names.

- covariates:

  Optional additional model columns.

- control:

  Control-arm label; defaults to the first observed arm.

- response_scale:

  `"change"` or `"followup"`.

- conf_level:

  Confidence level for adjusted treatment estimates.

## Value

An `endpoint_analysis` with standardized estimates, the `lm` fit,
complete analysis data, and missingness counts.

## Examples

``` r
d <- data.frame(
  arm = rep(c("control", "active"), each = 4),
  baseline = rep(0:3, 2),
  followup = c(1, 2, 4, 5, 3, 4, 6, 7)
)
endpointANCOVA(d, "followup", "baseline", "arm")
#> <endpoint_analysis> ancova_change: 1 contrast(s), 8/8 observations used
#>     arm control estimate std_error df statistic      p_value ci_lower ci_upper
#>  active control        2       0.2  5        10 0.0001709476 1.485884 2.514116
#>  cohens_d d_ci_lower d_ci_upper n_arm n_control
#>  3.464102   0.728371   6.199832     4         4
```
