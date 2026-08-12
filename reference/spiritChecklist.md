# SPIRIT 2013 Protocol Checklist

Marks protocol evidence against the bundled SPIRIT 2013 checklist. This
is a completeness aid and does not certify protocol compliance.

## Usage

``` r
spiritChecklist(protocol, checklist = NULL)
```

## Arguments

- protocol:

  Named list (`item_id` to evidence) or data frame with `item_id` and
  `evidence`.

- checklist:

  Optional replacement checklist using the bundled schema.

## Value

A `spirit_checklist` data frame with completion flags and evidence.

## References

Chan AW et al. (2013). SPIRIT 2013 Statement.

## Examples

``` r
spiritChecklist(list(`1` = "Descriptive title", `2` = "Registry ID"))
#> <spirit_checklist> 2/33 entries have evidence; completeness aid only
#>  item_id top_level_item                                           section
#>        1              1                        Administrative information
#>        2              2                        Administrative information
#>        3              3                        Administrative information
#>        4              4                        Administrative information
#>        5              5                        Administrative information
#>        6              6                                      Introduction
#>        7              7                                      Introduction
#>        8              8                                      Introduction
#>        9              9 Methods - participants interventions and outcomes
#>       10             10 Methods - participants interventions and outcomes
#>       11             11 Methods - participants interventions and outcomes
#>       12             12 Methods - participants interventions and outcomes
#>       13             13 Methods - participants interventions and outcomes
#>       14             14 Methods - participants interventions and outcomes
#>       15             15 Methods - participants interventions and outcomes
#>       16             16             Methods - assignment of interventions
#>       17             17             Methods - assignment of interventions
#>       18             18             Methods - assignment of interventions
#>       19             19             Methods - assignment of interventions
#>       20             20 Methods - data collection management and analysis
#>       21             21 Methods - data collection management and analysis
#>       22             22 Methods - data collection management and analysis
#>       23             23                              Methods - monitoring
#>       24             24                              Methods - monitoring
#>       25             25                              Methods - monitoring
#>       26             26                          Ethics and dissemination
#>       27             27                          Ethics and dissemination
#>       28             28                          Ethics and dissemination
#>       29             29                          Ethics and dissemination
#>       30             30                          Ethics and dissemination
#>       31             31                          Ethics and dissemination
#>       32             32                          Ethics and dissemination
#>       33             33                                        Appendices
#>                                 short_label          evidence complete
#>                           Descriptive title Descriptive title     TRUE
#>                          Trial registration       Registry ID     TRUE
#>                            Protocol version                      FALSE
#>                                     Funding                      FALSE
#>                  Roles and responsibilities                      FALSE
#>                    Background and rationale                      FALSE
#>                                  Objectives                      FALSE
#>                                Trial design                      FALSE
#>                               Study setting                      FALSE
#>                        Eligibility criteria                      FALSE
#>                               Interventions                      FALSE
#>                                    Outcomes                      FALSE
#>                        Participant timeline                      FALSE
#>                                 Sample size                      FALSE
#>                                 Recruitment                      FALSE
#>              Allocation sequence generation                      FALSE
#>                      Allocation concealment                      FALSE
#>                   Allocation implementation                      FALSE
#>                                    Blinding                      FALSE
#>                     Data collection methods                      FALSE
#>                             Data management                      FALSE
#>                         Statistical methods                      FALSE
#>                             Data monitoring                      FALSE
#>                                       Harms                      FALSE
#>                                    Auditing                      FALSE
#>                    Research ethics approval                      FALSE
#>                         Protocol amendments                      FALSE
#>                           Consent or assent                      FALSE
#>                             Confidentiality                      FALSE
#>                    Declaration of interests                      FALSE
#>                              Access to data                      FALSE
#>                        Dissemination policy                      FALSE
#>  Consent materials and biological specimens                      FALSE
```
