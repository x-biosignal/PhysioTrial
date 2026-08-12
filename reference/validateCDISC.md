# Validate a CDISC-Shaped Export

Checks the documented structural contract for exports created by
[`toSDTM()`](https://x-biosignal.github.io/PhysioTrial/reference/toSDTM.md)
or
[`toADaM()`](https://x-biosignal.github.io/PhysioTrial/reference/toADaM.md).
A clean report does not mean an external conformance product or
regulator has accepted the data.

## Usage

``` r
validateCDISC(x, controlled_terms = NULL)
```

## Arguments

- x:

  An `sdtm_export` or `adam_export`.

- controlled_terms:

  Optional named list, or a data frame with `variable` and `value`,
  adding caller-controlled terminology checks.

## Value

A `cdisc_validation` object with deterministic issues and summary.

## Examples

``` r
trial <- Trial("T1", c("A", "B"))
participants <- data.frame(id = c("p1", "p2"), arm = c("A", "B"))
validateCDISC(toSDTM(trial, participants))
#> <cdisc_validation> valid: 0 error(s), 0 warning(s)
```
