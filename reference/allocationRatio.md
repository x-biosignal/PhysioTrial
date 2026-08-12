# Allocation Ratio

Allocation Ratio

## Usage

``` r
allocationRatio(x)

# S4 method for class 'Trial'
allocationRatio(x)
```

## Arguments

- x:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

## Value

Named integer vector of allocation weights.

## Examples

``` r
allocationRatio(Trial("T1", c("active", "control")))
#>  active control 
#>       1       1 
```
