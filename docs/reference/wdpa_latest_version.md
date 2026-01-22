# Query latest version

Find the latest version of the combined World Database on Protected
Areas (WDPA) and World Database on Other Effective Area-Based
Conservation Measures (WDOECM) dataset. This is a character identifier
representing the month and year (e.g. `Sep2020`) the data were released.

## Usage

``` r
wdpa_latest_version()
```

## Value

A `character` value with the dataset version.

## Details

The version number is determined using a web address where the global
dataset is available. For specific details, please refer to the source
code for this function.

## Examples

``` r
# \dontrun{
# find the latest version
wdpa_latest_version()
#> [1] "Jan2026"
# }
```
