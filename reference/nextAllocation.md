# Reveal the Next Allocation

Reveal the Next Allocation

## Usage

``` r
nextAllocation(seq, requester = NA_character_)

# S4 method for class 'RandomizationSequence'
nextAllocation(seq, requester = NA_character_)
```

## Arguments

- seq:

  A
  [RandomizationSequence](https://x-biosignal.github.io/PhysioTrial/reference/RandomizationSequence-class.md)
  object.

- requester:

  Optional audit identity.

## Value

A list containing `participant_id`, `arm`, and the modified `sequence`.

## Examples

``` r
sequence <- randomize(Trial("T1", c("A", "B")), n = 4, seed = 1)
nextAllocation(sequence, requester = "site")
#> $participant_id
#> [1] "slot_1"
#> 
#> $arm
#> [1] "B"
#> 
#> $sequence
#> RandomizationSequence:T1
#>   method:simple
#>   revealed:1 of 4
#>  order participant_id stratum block_id  arm
#>      1         slot_1               NA    B
#>      2         slot_2               NA <NA>
#>      3         slot_3               NA <NA>
#>      4         slot_4               NA <NA>
#> 
```
