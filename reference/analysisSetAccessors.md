# Extract Analysis-Set Rows and Exclusions

Extract Analysis-Set Rows and Exclusions

## Usage

``` r
analysisParticipants(x)

analysisExclusions(x)
```

## Arguments

- x:

  An `analysis_set`.

## Value

`analysisParticipants()` returns the included original rows;
`analysisExclusions()` returns one explicit exclusion row per excluded
source participant.

## Examples

``` r
trial <- Trial("T1", c("A", "B"))
p <- data.frame(
  id = c("p1", "screen"),
  arm = c("A", NA),
  randomized = c(TRUE, FALSE)
)
x <- intentionToTreat(trial, p)
analysisParticipants(x)
#>   id arm randomized
#> 1 p1   A       TRUE
analysisExclusions(x)
#>   participant_id  arm         reason
#> 1         screen <NA> not_randomized
```
