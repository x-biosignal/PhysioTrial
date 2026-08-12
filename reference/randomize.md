# Generate a Concealed Randomization Sequence

Generate a Concealed Randomization Sequence

## Usage

``` r
randomize(
  trial,
  method = c("simple", "permuted_block", "stratified_block", "minimization"),
  n = NULL,
  participants = NULL,
  block_sizes = NULL,
  seed = NULL,
  p_bias = 0.8,
  weights = NULL,
  imbalance = c("range", "variance", "sd"),
  requester = NA_character_
)
```

## Arguments

- trial:

  A
  [Trial](https://x-biosignal.github.io/PhysioTrial/reference/Trial.md)
  object.

- method:

  Allocation method. Simple randomization matches the allocation ratio
  only in expectation. Block methods enforce the ratio in every
  completed block.

- n:

  Number of anonymous slots. Valid only for an unstratified trial and
  the simple or permuted-block methods.

- participants:

  A data frame with `id` and trial-factor columns, or a list of
  [Participant](https://x-biosignal.github.io/PhysioTrial/reference/Participant.md)
  objects.

- block_sizes:

  Positive block lengths, each a multiple of the allocation weight sum.

- seed:

  Positive integer seed. If `NULL`, a seed is drawn and captured.

- p_bias:

  High-probability branch of minimization's biased coin.

- weights:

  Optional named prognostic-factor weights.

- imbalance:

  Pocock-Simon discrepancy measure.

- requester:

  Optional audit identity recorded at sealing.

## Value

A concealed
[RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
object.

## Examples

``` r
trial <- Trial("T1", c("active", "control"))
randomize(trial, "permuted_block", n = 8, seed = 2026)
#> RandomizationSequence:T1
#>   method:permuted_block
#>   revealed:0 of 8
#>  order participant_id stratum block_id  arm
#>      1         slot_1                1 <NA>
#>      2         slot_2                1 <NA>
#>      3         slot_3                1 <NA>
#>      4         slot_4                1 <NA>
#>      5         slot_5                2 <NA>
#>      6         slot_6                2 <NA>
#>      7         slot_7                2 <NA>
#>      8         slot_8                2 <NA>
```
