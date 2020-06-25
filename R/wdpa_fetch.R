#' @include internal.R wdpa_url.R
NULL

#' Fetch data from the World Database on Protected Areas
#'
#' Download data from the World Database on Protected Areas (WDPA)
#' (available at <http://protectedplanet.net>) and import it.
#'
#' @param x `character` country for which to download data. This argument
#'   can be the name of the country (e.g. `"Liechtenstein"`) or the
#'   ISO-3 code for the country (e.g. `"LIE"`). This argument can also
#'   be set to `"global"` to download all of the protected areas available
#'   in the database (approximately 1.1 GB).
#'
#' @param wait `logical` if data is not immediately available for download
#'   should the session be paused until it is ready for download? If argument
#'   to `wait` is `FALSE` and the data is not ready then `NA`
#'   will be returned. Defaults to `FALSE`.
#'
#' @param download_dir `character` folder path to download the data.
#'  Defaults to a persistent data directory
#'  (`rappdirs::user_data_dir("wdpar")`).
#'
#' @param force_download `logical` if the data has previously been
#'   downloaded and is available at argument to `download_dir`, should a
#'   fresh copy be downloaded? Defaults to `FALSE`.
#'
#' @param verbose `logical` should a progress on downloading data be
#'   reported? Defaults to `TRUE` in an interactive session, otherwise
#'   `FALSE`.
#'
#' @details This function will download the specified protected area
#'   data and return it. **It is strongly recommended that the data be
#'   cleaned prior to analysis**. Check out the
#'   [wdpa_clean()] function to clean the data according to standard
#'   practices. For information on this database,
#'   prefer refer to the official manual
#'   (<https://www.protectedplanet.net/c/wdpa-manual>).
#'
#' @return [sf::sf()] object.
#'
#' @seealso [wdpa_clean()], [wdpa_read()],
#'   [wdpa_url()], [countrycode::countrycode()],
#'   <http://protectedplanet.net>,
#'   <https://www.protectedplanet.net/c/wdpa-manual>.
#'
#' @examples
#' \donttest{
#' # fetch data for Liechtenstein
#' lie_raw_data <- wdpa_fetch("Liechtenstein", wait = TRUE)
#'
#' # fetch data for Liechtenstein using the ISO3 code
#' lie_raw_data <- wdpa_fetch("LIE")
#'
#' # plot data
#' plot(lie_raw_data)
#' }
#' @export
wdpa_fetch <- function(x, wait = FALSE,
                       download_dir = rappdirs::user_data_dir("wdpar"),
                       force_download = FALSE, verbose = interactive()) {
  # check that arguments are valid
  ## check that classes are correct
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  assertthat::assert_that(assertthat::is.string(x),
    assertthat::is.dir(download_dir),
    assertthat::is.flag(force_download),
    assertthat::is.flag(verbose),
    identical(x, "global") || assertthat::is.string(country_code(x)))
  # try to find locally on system
  file_path <- try(wdpa_file(x, download_dir = download_dir), silent = TRUE)
  # fetch data
  if (force_download || inherits(file_path, "try-error")) {
    ## check for internet connection
   if (!curl::has_internet())
      stop(paste0("data not found in download_dir, and no internet connection",
                  "to download it."))
    ## find the download link and set file path to save the data
    download_url <- wdpa_url(x, wait = wait)
    file_name <- basename(httr::HEAD(download_url)$url)
    file_path <- file.path(download_dir, file_name)
    ## download the data
    if (!file.exists(file_path) || force_download) {
      curl::curl_download(download_url, file_path, quiet = !verbose)
      if (verbose)
        message("\n")
    }
    ## verify that the file exists
    if (!file.exists(file_path))
      stop("downloading data failed")
  } else {
    # parse month-year from file
    month_year <- vapply(strsplit(basename(file_path), "_", fixed = TRUE), `[[`,
                         character(1), 2)
    month <- gsub("[[:digit:]]", "", month_year)
    year <- gsub("[[:alpha:]]", "", month_year)
    file_date <- as.POSIXct(strptime(paste0("01/", month, "/", year),
                            "%d/%b/%Y"))
    current_date <- as.POSIXct(strptime(paste(replace(
                      strsplit(as.character(Sys.Date()), "-")[[1]],
                      3, "01"), collapse = "/"), "%Y/%m/%d"))
    if (file_date < current_date)
      warning(paste0("local data is out of date: ", format(file_date, "%b %Y")))
  }
  # import the data
  wdpa_read(file_path)
}
