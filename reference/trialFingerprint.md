# Trial Object Fingerprint

Trial Object Fingerprint

## Usage

``` r
trialFingerprint(x)

# S4 method for class 'RandomizationSequence'
trialFingerprint(x)

# S4 method for class 'BlindingManager'
trialFingerprint(x)
```

## Arguments

- x:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  or
  [BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
  object.

## Value

Deterministic `xxhash64` semantic-content fingerprint.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
trialFingerprint(sequence)
#> [1] "9c7354379b4ff2af"
```
