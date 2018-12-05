
<!--- README.md is generated from README.Rmd. Please edit that file -->
wdpar: Interface to the World Database on Protected Areas
---------------------------------------------------------

[![lifecycle](https://img.shields.io/badge/Lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![Travis Build Status](https://img.shields.io/travis/prioritizr/wdpar/master.svg?label=Linux%20%26%20Mac%20OSX)](https://travis-ci.org/prioritizr/wdpar) [![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/wdpar/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/wdpar) [![Coverage Status](https://codecov.io/github/prioritizr/wdpar/coverage.svg?branch=master)](https://codecov.io/github/prioritizr/wdpar?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/wdpar)](https://CRAN.R-project.org/package=wdpar)

### Overview

The *wdpar R* package provides an interface to the World Database on Protected Areas (WDPA). It provides functions for automatically downloading data (from [Protected Planet](http://protectedplanet.net)) and cleaning data following best practices (outlined in [Butchart *et al.* 2015](https://dx.doi.org/10.1111/conl.12158); [Runge *et al.* 2015](https://dx.doi.org/10.1126/science.aac9180); and [Protected Planet](https://www.protectedplanet.net/c/calculating-protected-area-coverage)).

### Installation

To install the developmental version of *wdpar*, use the following *R* code:

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("prioritizr/wdpar")
```

### Usage

Here we will provide a short introduction to the *wdpar R* package. First, we will load the *wdpar R* package. We will also load the *sf* and *dplyr R* packages to help explore the data.

``` r
# load packages
library(wdpar)
library(dplyr)
library(ggmap)
```

Now we will download protected area data for Malta. Note that we could have alternatively downloaded the data using Malta's [ISO3 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) (MLT).

``` r
# download protected area data for Malta
mlt_raw_pa_data <- wdpa_fetch("Malta", wait = TRUE)
```

Next, we will clean the data set. See `?wdpa_clean` for a detailed description of the data cleaning process.

``` r
# clean Malta data
mlt_pa_data <- wdpa_clean(mlt_raw_pa_data)
```

Print preview of the data associated with each protected area.

``` r
# print preview
head(mlt_pa_data)
```

    ## Simple feature collection with 6 features and 28 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 1382455 ymin: 4280784 xmax: 1399726 ymax: 4299684
    ## epsg (SRID):    NA
    ## proj4string:    +proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
    ## precision:      1000 
    ## # A tibble: 6 x 29
    ##   WDPAID WDPA_PID PA_DEF NAME  ORIG_NAME DESIG DESIG_ENG DESIG_TYPE
    ##    <dbl> <chr>    <chr>  <chr> <chr>     <chr> <chr>     <chr>     
    ## 1 1.94e5 194420   1      Filf… Filfla    Rise… Nature R… National  
    ## 2 1.94e5 194425   1      Il-G… Il-Gżejj… Rise… Nature R… National  
    ## 3 5.56e8 5555886… 1      Il-M… Il-Majji… Park… National… National  
    ## 4 1.75e5 174757   1      Il-Ġ… Il-Ġonna… List… List of … National  
    ## 5 1.75e5 174758   1      Bidn… Bidnija,… List… List of … National  
    ## 6 1.94e5 194415   1      Il-Ġ… Il-Ġonna… List… List of … National  
    ## # ... with 21 more variables: IUCN_CAT <chr>, INT_CRIT <chr>,
    ## #   MARINE <chr>, REP_M_AREA <dbl>, REP_AREA <dbl>, NO_TAKE <chr>,
    ## #   NO_TK_AREA <dbl>, STATUS <chr>, STATUS_YR <dbl>, GOV_TYPE <chr>,
    ## #   OWN_TYPE <chr>, MANG_AUTH <chr>, MANG_PLAN <chr>, VERIF <chr>,
    ## #   METADATAID <dbl>, SUB_LOC <chr>, PARENT_ISO <chr>, ISO3 <chr>,
    ## #   GEOMETRY_TYPE <chr>, AREA_KM2 <dbl>, geometry <MULTIPOLYGON [m]>

Finally, after cleaning the data, let's plot a map showing Malta's protected areas and color each area according to its management category ([as defined by the The International Union for Conservation of Nature](https://www.iucn.org/theme/protected-areas/about/protected-area-categories)).

``` r
# reproject data to longitude/latitude for plotting
mlt_pa_data <- st_transform(mlt_pa_data, 4326)

# download basemap imagery
bg <- get_stamenmap(unname(st_bbox(mlt_pa_data)), zoom = 8,
                    maptype = "watercolor", force = TRUE)

# make map
ggmap(bg) +
geom_sf(aes(fill = IUCN_CAT), data = mlt_pa_data, inherit.aes = FALSE) +
theme(axis.title = element_blank(), legend.position = "bottom")
```

<img src="man/figures/README-readme-map-1.png" width="80%" style="display: block; margin: auto;" />

For more examples using the *wdpar R* package, please refer to the [package vignette](https://github.com/prioritizr/wdpar/articles/wdpar.html).

### Citation

Please cite the *wdpar R* package and the World Database on Protected Areas (WDPA) in publications.


    To cite the wdpar package in publications, use:

      Hanson JO (2018) wdpar: Interface to the World Database on
      Protected Areas. R package version 0.0.0.1. Available at:
      https://github.com/jeffreyhanson/wdpar

      UNEP-WCMC and IUCN (2018) Protected Planet: The World Database
      on Protected Areas (WDPA), [insert month/year of the version
      downloaded], Cambridge, UK: UNEP-WCMC and IUCN. Available at:
      www.protectedplanet.net.

    Please cite both the World Database on Protected Areas data set
    and this package.
    To see these entries in BibTeX format, use 'print(<citation>,
    bibtex=TRUE)', 'toBibtex(.)', or set
    'options(citation.bibtex.max=999)'.
