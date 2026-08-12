# Allocation Counts

Counts use revealed rows only. The `stratum` value `"__overall__"`
denotes totals across all revealed strata.

## Usage

``` r
allocationTable(seq)

# S4 method for class 'RandomizationSequence'
allocationTable(seq)
```

## Arguments

- seq:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object.

## Value

Data frame with `stratum`, `arm`, and `n` columns.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
allocationTable(sequence)
#>       stratum arm n
#> 1 __overall__   A 0
#> 2 __overall__   B 0
```
