# Unblinding Log

Unblinding Log

## Usage

``` r
unblindingLog(manager)
```

## Arguments

- manager:

  A
  [BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
  object.

## Value

Append-only unblinding data frame.

## Examples

``` r
manager <- blindingManager(Trial("T1", c("A", "B")), seed = 1)
unblindingLog(manager)
#> [1] participant_id arm            requester      reason         time          
#> <0 rows> (or 0-length row.names)
```
