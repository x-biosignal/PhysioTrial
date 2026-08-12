# Trial Strata

Trial Strata

## Usage

``` r
strata(x)

# S4 method for class 'Trial'
strata(x)
```

## Arguments

- x:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

## Value

Named list of prognostic factors and admissible levels.

## Examples

``` r
strata(Trial(
  "T1", c("active", "control"),
  strata = list(site = c("north", "south"))
))
#> $site
#> [1] "north" "south"
#> 
```
