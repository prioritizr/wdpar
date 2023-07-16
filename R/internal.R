#' Country code
#'
#' Find ISO-3 country code for a country.
#'
#' @param x `character` name of country or its ISO-3 code.
#'
#' @return `character` ISO-3 country code.
#'
#' @seealso [countrycode::countrycode()].
#'
#' @noRd
country_code <- function(x) {
  # validate argument
  assertthat::assert_that(assertthat::is.string(x))
  # processing
  if (nchar(x) == 3) {
    # check that x is valid ISO-3 code
    name <- suppressWarnings(countrycode::countrycode(x, "iso3c",
                                                      "country.name.en"))
    if (is.na(name[[1]]))
      stop("argument to x is not a valid iso3 code")
    country_code <- toupper(x)
  } else {
    # check that x is valid country name
    country_code <- suppressWarnings(countrycode::countrycode(x,
                      "country.name.en", "iso3c"))
    if (is.na(country_code[[1]]))
      stop("argument to x is not a valid country name")
    country_code <- toupper(country_code)
  }
  return(country_code)
}

#' Find most recent version of WDPA dataset in folder
#'
#' Find the file in a folder which has the most recent version of the WDPA data
#' set in it.
#'
#' @inheritParams wdpa_fetch
#'
#' @return `character` file path.
#'
#' @noRd
wdpa_file <- function(x, download_dir = tempfile()) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.dir(download_dir))
  # convert x to country ISO3 code
  if (x != "global") {
    x <- country_code(x)
  }
  # set file and date search patterns
  if (x == "global") {
    file_pattern <- "WDPA\\_.*\\_Public.zip"
  } else {
    file_pattern <- paste0("WDPA\\_.*\\_", x, "\\-shapefile.zip")
  }
  ## search for file
  file_paths <- dir(download_dir, file_pattern, full.names = TRUE)
  if (length(file_paths) == 0)
    stop("data not found in \"download_dir\" folder")
  # parse date-times
  file_versions <- vapply(file_paths, wdpa_version, character(1))
  file_dates <- convert_wdpa_version_to_POSIXct(file_versions)
  # set file_path as latest version
  file_path <- file_paths[which.max(file_dates)]
  # return file path
  return(file_path)
}

#' Extract polygons and points
#'
#' Extract polygons and points from a [sf::sf()] object.
#'
#' @param x [sf::sf()] object.
#'
#' @return [sf::sf()] object.
#'
#' @noRd
extract_polygons_and_points <- function(x) {
  # find point indices
  ind <- vapply(sf::st_geometry(x), inherits, logical(1),
                c("POINT", "MULTIPOINT"))
  # extract polygons from geometries
  if (inherits(sf::st_geometry(x),
               c("sfc_GEOMETRY", "sfc_GEOMETRYCOLLECTION"))) {
    o <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  } else {
    o <- x[!ind, ]
  }
  # extract points
  rbind(o, x[ind, , drop = FALSE])
}

# Empty WDPA data set
#'
#' This function creates an empty WDPA dataset.
#'
#' @return [sf::sf()] object.
#'
#' @examples
#' empty_wdpa_dataset(st_crs("+init=epsg:4326"))
#'
#' @noRd
empty_wdpa_dataset <- function(crs) {
  sf::st_sf(tibble::tibble(
    WDPAID = numeric(0),
    WDPA_PID = character(0),
    PA_DEF = character(0),
    NAME = character(0),
    ORIG_NAME = character(0),
    DESIG = character(0),
    DESIG_ENG = character(0),
    DESIG_TYPE = character(0),
    IUCN_CAT = character(0),
    INT_CRIT = character(0),
    MARINE = character(0),
    REP_M_AREA = numeric(0),
    REP_AREA = numeric(0),
    NO_TAKE = character(0),
    NO_TK_AREA = numeric(0),
    STATUS = character(0),
    STATUS_YR = numeric(0),
    GOV_TYPE = character(0),
    OWN_TYPE = character(0),
    MANG_AUTH = character(0),
    MANG_PLAN = character(0),
    VERIF =  character(0),
    METADATAID = integer(0),
    SUB_LOC = character(0),
    PARENT_ISO = character(0),
    ISO3 = character(0),
    SUPP_INFO = character(0),
    CONS_OBJ = character(0),
    GEOMETRY_TYPE = character(0),
    AREA_KM2 = numeric(0)),
    geometry = sf::st_sfc(crs = crs))
}

#' Dataset version
#'
#' Determine the version of protected area data.
#'
#' @param x `character` file name.
#'
#' @return `character` version.
#'
#' @noRd
wdpa_version <- function(x) {
  # verify argument is valid
  assertthat::assert_that(assertthat::is.string(x), assertthat::noNA(x))
  x <- gsub("WDOECM_", "", x, fixed = TRUE)
  strsplit(basename(x), "_", fixed = TRUE)[[1]][[2]]
}

#' Covert dataset version to POSIXct
#'
#' Coerce data version to `POSIXct` format.
#'
#' @param x `character` file name.
#'
#' @return `character` version.
#'
#' @noRd
convert_wdpa_version_to_POSIXct <- function(x) {
  # verify argument is valid
  assertthat::assert_that(is.character(x), assertthat::noNA(x))
  # parse date-times
  # note that date-times easily cannot be parsed using base::strptime due to
  # bug where date-times with month before year (e.g. Nov2011) will return NA
  # see this post on Stack Overflow
  # https://stackoverflow.com/questions/26997864/strptime-not-recognizing-b-b
  # therefore we must---unsatisfyingly---extract the month and years separately
  # reorder them, and then parse using base::strptime
  month <- gsub("[[:digit:]]", "", x)
  year <- gsub("[[:alpha:]]", "", x)
  out <- try(as.POSIXct(strptime(paste0("01/", month, "/", year), "%d/%b/%Y")),
             silent = TRUE)
  # verify valid date
  if (inherits(out, "try-error"))
    stop("version not recognized") #nocov
  assertthat::assert_that(
    all(!is.na(out)), all(nchar(year) == 4), all(nchar(month) == 3),
    msg = "version not recognized")
  # return result
  out
}

#' Download file
#'
#' @param url `character` URL for downloading file.
#'
#' @param path `character` path to save data.
#'
#' @param quiet `logical` should downloading information be suppressed?
#'
#' @return Invisible `logical` indicating success.
#'
#' @noRd
download_file <- function(url, path, quiet = TRUE) {
  res <- curl::curl_download(url, path, quiet = quiet)
  invisible(TRUE)
}

#' Is online?
#'
#' Check if an online internet connection is active.
#'
#' @details
#' This function uses [pingr::is_online()] and [curl::has_internet()]
#' to check for an internet connection. There are some issues where
#' one of these functions returns a false-negative.
#'
#' @return `logical` value indicating success.
#'
#' @examples
#' # check if online
#' print(is_online())
#'
#' @noRd
is_online <- function() {
  isTRUE(try(curl::has_internet(), silent = TRUE)) ||
  isTRUE(try(pingr::is_online(), silent = TRUE))
}

assertthat::on_failure(is_online) <- function(call, env) {
  "could not establish an active internet connection"
}
