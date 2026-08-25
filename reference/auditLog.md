# Randomization Audit Log

Randomization Audit Log

## Usage

``` r
auditLog(x)

# S4 method for class 'RandomizationSequence'
auditLog(x)
```

## Arguments

- x:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object.

## Value

Append-only audit data frame.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
auditLog(sequence)
#>   event                time seed n_revealed agent
#> 1  seal 2026-08-25 16:16:49    1          0  <NA>
```
