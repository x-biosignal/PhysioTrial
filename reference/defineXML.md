# Create a Deterministic define.xml Metadata Stub

Serializes metadata for the exact datasets in an SDTM-shaped or
ADaM-shaped export. The result is a deterministic ODM metadata stub, not
a submission-ready define.xml document.

## Usage

``` r
defineXML(
  x,
  file = NULL,
  study_name = NULL,
  protocol_name = NULL,
  standard_version = NULL
)
```

## Arguments

- x:

  An `sdtm_export` or `adam_export`.

- file:

  Optional output path. Writing uses a temporary file followed by an
  atomic rename.

- study_name, protocol_name:

  Optional display metadata.

- standard_version:

  Optional standard-version label.

## Value

Invisibly, the XML string when `file` is `NULL`; otherwise the
normalized written path.

## Examples

``` r
trial <- Trial("T1", c("A", "B"))
participants <- data.frame(id = c("p1", "p2"), arm = c("A", "B"))
xml <- defineXML(toSDTM(trial, participants))
grepl("ItemGroupDef", xml, fixed = TRUE)
#> [1] TRUE
```
