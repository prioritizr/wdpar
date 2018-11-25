#' Country code
#'
#' Find ISO-3 country code for a country.
#'
#' @param x \code{character} name of country or its ISO-3 code.
#'
#' @return \code{character} ISO-3 country code.
#'
#' @seealso \code{\link[countrycode]{countrycode}}.
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

#' Find most recent version of WDPA data set in folder
#'
#' Find the file in a folder which has the most recent version of the WDPA data #' set in it.
#'
#'
#' @param x \code{character} Country for desired data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to obtain file path for the global data set.
#'
#' @param download_dir \code{character} directory to which the data will be
#'  downloaded. Defaults to a persistent data directory
#'  (\code{rappdirs::user_data_dir("wdpar")}).
#'
#' @return \code{character} file path.
#'
#' @noRd
wdpa_file <- function(x, download_dir) {
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
  # note that date-times easily cannot be parsed using base::strptime due to
  # bug where date-times with month before year (e.g. Nov2011) will return NA
  # see this post on Stack Overflow
  # https://stackoverflow.com/questions/26997864/strptime-not-recognizing-b-b
  # therefore we must---unsatisfyingly---extract the month and years separately
  # reorder them, and then parse using base::strptime
  month_year <- vapply(strsplit(basename(file_paths), "_", fixed = TRUE), `[[`,
                       character(1), 2)
  months <- gsub("[[:digit:]]", "", month_year)
  years <- gsub("[[:alpha:]]", "", month_year)
  file_dates <- as.POSIXct(strptime(paste0("01/", months, "/", years),
                           "%d/%b/%Y"))
  # set file_path as latest version
  file_path <- file_paths[which.max(file_dates)]
  # return file path
  return(file_path)
}
