# Record an Unblinding Event

Record an Unblinding Event

## Usage

``` r
unblind(
  manager,
  participant_id,
  requester = NA_character_,
  reason = NA_character_
)
```

## Arguments

- manager:

  A
  [BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
  object.

- participant_id:

  Participant to unblind.

- requester:

  Optional identity requesting unblinding.

- reason:

  Optional reason for unblinding.

## Value

The modified
[BlindingManager](https://x-biosignal.github.io/PhysioTrial/reference/BlindingManager-class.md)
object.

## Examples

``` r
trial <- Trial("T1", c("A", "B"))
sequence <- randomize(trial, n = 4, seed = 1)
manager <- blindingManager(trial, sequence, seed = 2)
unblind(manager, "slot_1", requester = "PI", reason = "safety")
#> BlindingManager:T1
#>   arms:2
#>   coded participants:4
#>   unblinding events:1
```
