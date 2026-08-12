# Captured Randomization Seed

Captured Randomization Seed

## Usage

``` r
randomSeed(seq)

# S4 method for class 'RandomizationSequence'
randomSeed(seq)
```

## Arguments

- seq:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object.

## Value

Positive integer seed.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 19)
randomSeed(sequence)
#> [1] 19
```
