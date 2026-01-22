# Download URL

Obtain a URL to download data from [Protected
Planet](https://www.protectedplanet.net/en). Specifically, the URL
provides access to data available through the World Database on
Protected Areas (WDPA) and the World Database on Other Effective
Area-Based Conservation Measures (WDOECM). **Note that data are accessed
assuming non-commercial use.**

## Usage

``` r
wdpa_url(x, wait = FALSE, page_wait = 2, datatype = "gdb")
```

## Arguments

- x:

  `character` country for desired data. This argument can be the name of
  the country (e.g. `"Liechtenstein"`) or the ISO-3 code for the country
  (e.g. `"LIE"`). This argument can also be set to `"global"` to obtain
  the URL for the global dataset.

- wait:

  `logical` if data is not immediately available for download should the
  session be paused until it is ready for download? If argument to
  `wait` is `FALSE` and the data is not ready then an error will be
  thrown. Defaults to `FALSE`.

- page_wait:

  `numeric` number of seconds to wait for web pages to load when finding
  the download URL on [Protected
  Planet](https://www.protectedplanet.net/en). Defaults to 2. Since the
  process of finding a download URL requires navigating through multiple
  web pages, the default argument means that the function will take at
  least 8 seconds to complete. Users on slow internet connections may
  experience issues with the default argument (e.g. resulting in an
  error containing the message `Error: Summary: NoSuchElement`). To
  avoid this, users can try specifying a greater value (e.g. 5 seconds).

- datatype:

  `character` denoting the file format for which to download protected
  area data. Available options include: (`"shp"`) shapefile format and
  (`"gdb"`) file geodatabase format. Defaults to \`"gdb". Note that
  global data are only available in file geodatabase format.

## Value

A `character` value with the URL to download the data.

## See also

[`wdpa_fetch()`](wdpa_fetch.md),
[`countrycode::countrycode()`](https://vincentarelbundock.github.io/countrycode/reference/countrycode.html).

## Examples

``` r
# \dontrun{
# obtain url for New Zealand data
nzl_url <- wdpa_url("New Zealand", wait = TRUE)
print(nzl_url)
#> [1] "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Jan2026_Public_NZL.zip"

# obtain url for New Zealand data using its ISO3 code
nzl_url <- wdpa_url("NZL", wait = TRUE)
print(nzl_url)
#> [1] "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Jan2026_Public_NZL.zip"

# obtain url for global data
global_url <- wdpa_url("global")
print(global_url)
#> [1] "http://wcmc.io/wdpa_current_release"
# }
```
