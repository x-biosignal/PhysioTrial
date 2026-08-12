# Construct a Randomized Trial

Construct a Randomized Trial

## Usage

``` r
Trial(
  id,
  arms,
  allocation_ratio = NULL,
  strata = list(),
  endpoints = list(),
  metadata = list()
)
```

## Arguments

- id:

  Non-empty trial identifier.

- arms:

  Character arm labels or a list of
  [Arm](https://x-biosignal.github.io/PhysioTrial/reference/Arm.md)
  objects.

- allocation_ratio:

  Optional named or positional allocation weights.

- strata:

  Named list mapping prognostic factors to admissible levels.

- endpoints:

  Named endpoint definitions.

- metadata:

  Free-form trial metadata.

## Value

A Trial object.

## Examples

``` r
Trial(
  "rehab-01",
  arms = c("active", "control"),
  strata = list(site = c("north", "south"))
)
#> An object of class "Trial"
#> Slot "id":
#> [1] "rehab-01"
#> 
#> Slot "arms":
#> [1] "active"  "control"
#> 
#> Slot "arm_specs":
#> [[1]]
#> An object of class "Arm"
#> Slot "label":
#> [1] "active"
#> 
#> Slot "ratio":
#> [1] 1
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "is_control":
#> [1] FALSE
#> 
#> Slot "treatment_code":
#> [1] NA
#> 
#> 
#> [[2]]
#> An object of class "Arm"
#> Slot "label":
#> [1] "control"
#> 
#> Slot "ratio":
#> [1] 1
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "is_control":
#> [1] FALSE
#> 
#> Slot "treatment_code":
#> [1] NA
#> 
#> 
#> 
#> Slot "allocation_ratio":
#>  active control 
#>       1       1 
#> 
#> Slot "strata":
#> $site
#> [1] "north" "south"
#> 
#> 
#> Slot "endpoints":
#> list()
#> 
#> Slot "metadata":
#> list()
#> 
```
