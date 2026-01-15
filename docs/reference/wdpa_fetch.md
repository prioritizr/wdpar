# Fetch data

Fetch data from [Protected Planet](https://www.protectedplanet.net/en).
Specifically, data are downloaded from the World Database on Protected
Areas (WDPA) and the World Database on Other Effective Area-Based
Conservation Measures (WDOECM). **Note that data are downloaded assuming
non-commercial use.**

## Usage

``` r
wdpa_fetch(
  x,
  wait = FALSE,
  download_dir = tempdir(),
  force_download = FALSE,
  check_version = TRUE,
  n = NULL,
  page_wait = 2,
  datatype = "gdb",
  verbose = interactive()
)
```

## Arguments

- x:

  `character` country for which to download data. This argument can be
  the name of the country (e.g. `"Liechtenstein"`) or the ISO-3 code for
  the country (e.g. `"LIE"`). This argument can also be set to
  `"global"` to download all of the protected areas available in the
  database (approximately 1.1 GB).

- wait:

  `logical` if data is not immediately available for download should the
  session be paused until it is ready for download? If argument to
  `wait` is `FALSE` and the data is not ready then `NA` will be
  returned. Defaults to `FALSE`.

- download_dir:

  `character` folder path to download the data. Defaults to a temporary
  directory. To avoid downloading the same dataset multiple times, it is
  recommended to use a persistent directory (e.g.
  `rappdirs::user_data_dir("wdpar")`; see Examples below).

- force_download:

  `logical` if the data has previously been downloaded and is available
  at argument to `download_dir`, should a fresh copy be downloaded?
  Defaults to `FALSE`.

- check_version:

  `logical` if the data are being imported from from the argument to
  `download_dir`, should the data be checked to see if the version
  number matches the latest version available online? Defaults to
  `TRUE`.

- n:

  `integer` number of records to import per data source. Defaults to
  `NULL` such that all data are imported.

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

- verbose:

  `logical` should a progress on downloading data be reported? Defaults
  to `TRUE` in an interactive session, otherwise `FALSE`.

## Value

A [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Details

This function obtains and imports data from Protected Planet. By default
(per `force_download = FALSE`), it will check to see if the data have
already been downloaded and, if so, simply import the previously
downloaded data. It will also check to see if a newer version of the
dataset is available on Protected Planet (per `check_version = TRUE`)
and, if so, provide an alert. If the latest version is not required,
this alert can be safely ignored. However, if the latest version of the
data is required, then using `force_download = TRUE` will ensure that
the latest version is always obtained. After importing the data, it is
strongly recommended to clean the data prior to analysis (see
[`wdpa_clean()`](wdpa_clean.md)).

## Data source

The `PA_DEF` column indicates the data source for individual areas and
sites that comprise the imported dataset. Specifically, data obtained
through the World Database on Protected Areas (WDPA) are indicated with
a value of `1` in the `PA_DEF` column. Additionally, data obtained
through the World Database on Other Effective Area-Based Conservation
Measures (WDOECM) are indicated with a value of `0` in the `PA_DEF`
column. For more details on data conventions, please consult the
official manual (UNEP-WCMC 2019).

## Troubleshooting

The function requires a Chromium-based browser (e.g., Google Chrome,
Chromium, or Brave) to be installed. This is because it uses the
chromote to find the URL for downloading data from Protected Planet. If
you don't have one of these browsers installed, then please try
installing Google Chrome. If you do have one of these browsers installed
and this function throws an error indicating that it can't find the
browser, try setting the `CHROMOTE_CHROME` environment variable to the
file path of the executable. For example, you could do this with:

    Sys.setenv(CHROMOTE_CHROME = "INSERT_FILE_PATH_HERE.exe")

Also, the function will sometimes produce messages that complain about
`handle_read_frame` or `unpromised promise` errors. Please understand
that these messages are, in fact, not errors and can be safely ignored
(see <https://github.com/rstudio/chromote/pull/111>). As such, if you
see these messages when running the function, you can assume that the
function has still worked correctly. For reference, the misleading
messages will look something like the following:

    [error] handle_read_frame error: websocketpp.transport:7 (End of File)
    Unhandled promise error: Chromote: timed out waiting for response to command Browser.close

For further help with troubleshooting, please refer to the documentation
for the chromote package (https://rstudio.github.io/chromote/).

## References

UNEP-WCMC (2019). User Manual for the World Database on Protected Areas
and world database on other effective area-based conservation measures:
1.6. UNEP-WCMC: Cambridge, UK. Available at:
<https://wcmc.io/WDPA_Manual>.

## See also

[`wdpa_clean()`](wdpa_clean.md), [`wdpa_read()`](wdpa_read.md),
[`wdpa_url()`](wdpa_url.md),
[`countrycode::countrycode()`](https://vincentarelbundock.github.io/countrycode/reference/countrycode.html).

## Examples

``` r
# \dontrun{
# fetch data for Liechtenstein
lie_raw_data <- wdpa_fetch("Liechtenstein", wait = TRUE)

# print data
print(lie_raw_data)
#> Simple feature collection with 47 features and 33 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 9.475186 ymin: 47.04974 xmax: 9.636976 ymax: 47.26538
#> Geodetic CRS:  WGS 84
#> # A tibble: 47 × 34
#>    SITE_ID SITE_PID SITE_TYPE NAME_ENG NAME  DESIG DESIG_ENG DESIG_TYPE IUCN_CAT
#>      <int> <chr>    <chr>     <chr>    <chr> <chr> <chr>     <chr>      <chr>   
#>  1   18107 18107    PA        Ruggell… Rugg… Natu… Nature R… National   Ia      
#>  2   18109 18109    PA        Schwabb… Schw… Natu… Nature R… National   Ia      
#>  3   30747 30747    PA        Gamprin… Gamp… Natu… Nature R… National   Ia      
#>  4   30750 30750    PA        Äulehäg  Äule… Natu… Nature R… National   Ia      
#>  5   30752 30752    PA        Triesne… Trie… Natu… Nature R… National   Ia      
#>  6   30753 30753    PA        Wisanels Wisa… Natu… Nature R… National   Ia      
#>  7   30754 30754    PA        Birka    Birka Natu… Nature R… National   Ia      
#>  8   30755 30755    PA        Schneck… Schn… Natu… Nature R… National   Ia      
#>  9   30756 30756    PA        Au       Au    Natu… Nature R… National   Ia      
#> 10   30757 30757    PA        Pflanze… Pfla… Gesc… Protecte… National   V       
#> # ℹ 37 more rows
#> # ℹ 25 more variables: INT_CRIT <chr>, REALM <chr>, REP_M_AREA <dbl>,
#> #   GIS_M_AREA <dbl>, REP_AREA <dbl>, GIS_AREA <dbl>, NO_TAKE <chr>,
#> #   NO_TK_AREA <dbl>, STATUS <chr>, STATUS_YR <int>, GOV_TYPE <chr>,
#> #   GOVSUBTYPE <chr>, OWN_TYPE <chr>, OWNSUBTYPE <chr>, MANG_AUTH <chr>,
#> #   MANG_PLAN <chr>, VERIF <chr>, METADATAID <int>, PRNT_ISO3 <chr>,
#> #   ISO3 <chr>, SUPP_INFO <chr>, CONS_OBJ <chr>, INLND_WTRS <chr>, …

# plot data
plot(lie_raw_data)
#> Warning: plotting the first 10 out of 33 attributes; use max.plot = 33 to plot all


# fetch data for Liechtenstein using the ISO3 code
lie_raw_data <- wdpa_fetch("LIE", wait = TRUE)

# since data are saved in a temporary directory by default,
# a persistent directory can be specified to avoid having to download the
# same dataset every time the R session is restarted
lie_raw_data <- wdpa_fetch("LIE", wait = TRUE,
                           download_dir = rappdirs::user_data_dir("wdpar"))
#> ! importing local data (version Dec 2021); use "force=TRUE" if you need latest version.

# data for multiple countries can be downloaded separately and combined,
# this is useful to avoid having to download the global dataset
## load packages to easily merge datasets
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tibble)

## define country names to download
country_codes <- c("LIE", "MHL")

## download data for each country
mult_data <- lapply(country_codes, wdpa_fetch, wait = TRUE)

## merge datasets together
mult_dat <- st_as_sf(as_tibble(bind_rows(mult_data)))

## print data
print(mult_dat)
#> Simple feature collection with 63 features and 33 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 9.475186 ymin: 5.586051 xmax: 171.9985 ymax: 47.26538
#> Geodetic CRS:  WGS 84
#> # A tibble: 63 × 34
#>    SITE_ID SITE_PID SITE_TYPE NAME_ENG NAME  DESIG DESIG_ENG DESIG_TYPE IUCN_CAT
#>      <int> <chr>    <chr>     <chr>    <chr> <chr> <chr>     <chr>      <chr>   
#>  1   18107 18107    PA        Ruggell… Rugg… Natu… Nature R… National   Ia      
#>  2   18109 18109    PA        Schwabb… Schw… Natu… Nature R… National   Ia      
#>  3   30747 30747    PA        Gamprin… Gamp… Natu… Nature R… National   Ia      
#>  4   30750 30750    PA        Äulehäg  Äule… Natu… Nature R… National   Ia      
#>  5   30752 30752    PA        Triesne… Trie… Natu… Nature R… National   Ia      
#>  6   30753 30753    PA        Wisanels Wisa… Natu… Nature R… National   Ia      
#>  7   30754 30754    PA        Birka    Birka Natu… Nature R… National   Ia      
#>  8   30755 30755    PA        Schneck… Schn… Natu… Nature R… National   Ia      
#>  9   30756 30756    PA        Au       Au    Natu… Nature R… National   Ia      
#> 10   30757 30757    PA        Pflanze… Pfla… Gesc… Protecte… National   V       
#> # ℹ 53 more rows
#> # ℹ 25 more variables: INT_CRIT <chr>, REALM <chr>, REP_M_AREA <dbl>,
#> #   GIS_M_AREA <dbl>, REP_AREA <dbl>, GIS_AREA <dbl>, NO_TAKE <chr>,
#> #   NO_TK_AREA <dbl>, STATUS <chr>, STATUS_YR <int>, GOV_TYPE <chr>,
#> #   GOVSUBTYPE <chr>, OWN_TYPE <chr>, OWNSUBTYPE <chr>, MANG_AUTH <chr>,
#> #   MANG_PLAN <chr>, VERIF <chr>, METADATAID <int>, PRNT_ISO3 <chr>,
#> #   ISO3 <chr>, SUPP_INFO <chr>, CONS_OBJ <chr>, INLND_WTRS <chr>, …
# }
```
