# Construct a Blinding Manager

Construct a Blinding Manager

## Usage

``` r
blindingManager(trial, sequence = NULL, codes = NULL, seed = NULL)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- sequence:

  Optional
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object from the same trial.

- codes:

  Optional named character vector mapping arms to masked treatment
  codes.

- seed:

  Optional positive integer used to scramble generated codes.

## Value

A
[BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
object.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
blindingManager(trial, seed = 1)
#> BlindingManager:T1
#>   arms:2
#>   coded participants:0
#>   unblinding events:0
```
