# Revealed Assignments

Revealed Assignments

## Usage

``` r
assignments(seq)

# S4 method for class 'RandomizationSequence'
assignments(seq)
```

## Arguments

- seq:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object.

## Value

Assignment table with every unrevealed arm replaced by `NA`.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
assignments(sequence)
#>   order participant_id stratum block_id  arm
#> 1     1         slot_1               NA <NA>
#> 2     2         slot_2               NA <NA>
#> 3     3         slot_3               NA <NA>
#> 4     4         slot_4               NA <NA>
```
