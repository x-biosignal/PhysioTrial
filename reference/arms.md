# Arm Labels

Arm Labels

## Usage

``` r
arms(x)

# S4 method for class 'Trial'
arms(x)
```

## Arguments

- x:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

## Value

Character vector of arm labels in allocation order.

## Examples

``` r
arms(Trial("T1", c("active", "control")))
#> [1] "active"  "control"
```
