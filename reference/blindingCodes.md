# Masked Treatment Codes

Masked Treatment Codes

## Usage

``` r
blindingCodes(manager)
```

## Arguments

- manager:

  A
  [BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
  object.

## Value

Named character vector mapping arms to masked codes.

## Examples

``` r
manager <- blindingManager(Trial("T1", c("A", "B")), seed = 1)
blindingCodes(manager)
#>         A         B 
#> "Kit-001" "Kit-002" 
```
