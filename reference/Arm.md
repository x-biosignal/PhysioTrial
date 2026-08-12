# Construct a Trial Arm

Construct a Trial Arm

## Usage

``` r
Arm(
  label,
  ratio = 1L,
  description = NA_character_,
  is_control = FALSE,
  treatment_code = NA_character_
)
```

## Arguments

- label:

  Non-empty arm label.

- ratio:

  Positive integer allocation weight.

- description:

  Optional description.

- is_control:

  Whether this is a control arm.

- treatment_code:

  Optional masked treatment code.

## Value

An Arm object.

## Examples

``` r
Arm("active", ratio = 2L, is_control = FALSE)
#> An object of class "Arm"
#> Slot "label":
#> [1] "active"
#> 
#> Slot "ratio":
#> [1] 2
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
```
