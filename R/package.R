#' wdpar: Interface to the World Database on Protected Areas
#'
#' The \pkg{wdpar} R package provides an interface to data provided by
#' [Protected Planet](https://www.protectedplanet.net/en).
#' Specifically, it can be used to automatically obtain data from
#' the World Database on Protected Areas
#' (WDPA) and the World Database on Other Effective Area-Based Conservation
#' Measures (WDOECM).
#' It also provides methods for cleaning data from these databases following
#' best practices
#' (outlined in Butchart *et al.* 2015; Protected Planet 2021; Runge *et al.*
#' 2015). The main functions are [wdpa_fetch()]
#' for downloading data and [wdpa_clean()] for cleaning data. For
#' more information, please see the package vignette.
#' To cite this package, please see `citation("wdpar")`.
#'
#' @references
#' Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP,
#' Harfoot M, ... & Brooks TM (2015) Shortfalls and solutions for
#' meeting national and global conservation area targets.
#' *Conservation Letters*, **8**: 329--337.
#'
#' Protected Planet (2021) Calculating protected and OECM area coverage.
#' Available at:
#' <https://www.protectedplanet.net/en/resources/calculating-protected-area-coverage>.
#'
#' Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
#' (2015) Protected areas and global conservation of migratory birds.
#' *Science*, **350**: 1255--1258.
#'
#' @name wdpar
#' @aliases wdpar-package
#' @docType package
NULL

#' @import sf
NULL

# avoid false positive NOTEs during CRAN checks
#' @importFrom rappdirs user_data_dir
NULL
