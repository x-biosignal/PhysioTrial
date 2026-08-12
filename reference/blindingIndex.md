# Bang Blinding Index

Computes Bang's arm-specific blinding index and normal-approximation
confidence interval. A guess matching the actual arm is correct;
`dont_know` is neutral; any other value, including an unknown arm label
or missing guess, is incorrect.

## Usage

``` r
blindingIndex(
  guesses,
  arm_col = "arm",
  guess_col = "guess",
  dont_know = "dont_know",
  conf_level = 0.95
)
```

## Arguments

- guesses:

  Data frame containing actual assignments and guesses.

- arm_col:

  Name of the actual-assignment column.

- guess_col:

  Name of the guess column.

- dont_know:

  Sentinel used for a "don't know" response.

- conf_level:

  Confidence level strictly between zero and one.

## Value

A `blinding_index` object.

## References

Bang H, Ni L, Davis CE (2004). Assessment of blinding in clinical
trials. *Controlled Clinical Trials*, 25, 143-156.

## Examples

``` r
guesses <- data.frame(
  arm = c("active", "active", "control", "control"),
  guess = c("active", "dont_know", "active", "control")
)
blindingIndex(guesses)
#> Bang blinding index (95% confidence interval)
#>      arm n n_correct n_incorrect n_dontknow  BI        SE   ci_lower ci_upper
#>   active 2         1           0          1 0.5 0.3535534 -0.1929519 1.192952
#>  control 2         1           1          0 0.0 0.7071068 -1.3859038 1.385904
```
