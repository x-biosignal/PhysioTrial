# Construct a Trial Participant

Construct a Trial Participant

## Usage

``` r
Participant(
  id,
  strata = character(0),
  arm = NA_character_,
  enrolled_at = NA_character_
)
```

## Arguments

- id:

  Non-empty participant identifier.

- strata:

  Named character vector mapping prognostic factors to levels.

- arm:

  Allocated arm, or `NA` before allocation.

- enrolled_at:

  Optional enrollment time representation.

## Value

A Participant object.

## Examples

``` r
Participant("P001", strata = c(site = "north"))
#> An object of class "Participant"
#> Slot "id":
#> [1] "P001"
#> 
#> Slot "strata":
#>    site 
#> "north" 
#> 
#> Slot "arm":
#> [1] NA
#> 
#> Slot "enrolled_at":
#> [1] NA
#> 
```
