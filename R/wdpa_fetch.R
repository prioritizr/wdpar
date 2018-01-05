#' @include internal.R wdpa_url.R
NULL

#' Fetch data from the World Database on Protected Areas
#'
#' Download data from the World Database on Protected Areas (WDPA)
#' (available at \url{http://protectedplanet.net}) and import it. Note that
#' if the data has previously been downloaded and is available
#' in the specified download folder, then the data will simply be imported.
#'
#' @param x \code{character} country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to download all of the protected areas available
#'   in the database (approx. 1.1 GB).
#'
#' @param download_dir \code{character} folder path to download the data.
#'  Defaults to a persistent data directory
#'  (\code{rappdirs::user_data_dir("wdpar")}).
#'
#' @param force_download \code{logical} if the data has previously been
#'   downloaded and is available at argument to \code{x}, should the data be
#'   redownloaded anyway? Defaults to \code{FALSE}.
#'
#' @param verbose \code{logical} should a progress on downloading data be
#'   reported? Defaults to \code{TRUE} if the session is interactive.
#'
#' @details This function will simply download the specified protected area
#'   data and return it for subsequent use. It is strongly recomended that users
#'   clean the data prior to analysis. It is worth noting that downloading
#'   data for a specific country will not include protected areas that are
#'   represented as a point locality. To obtain data for protected areas
#'   that are represented as point localities, the global data set will need
#'   to be downloaded---even if data for a single country is required.
#'   Check out the \code{\link{wdpa_clean}} function to clean the data
#'   according to standard practices.
#'
#' @return \code{\link[sf]{sf}} simple features object.
#'
#' @seealso \code{\link{wdpa_clean}}, \code{\link{wdpa_read}},
#'   \code{\link{wdpa_url}}, \code{\link[countrycode]{countrycode}},
#'   \url{http://protectedplanet.net}.
#'
#' @examples
#' \donttest{
#' # fetch data for Liechtenstein
#' lie_raw_data <- wdpa_fetch("Liechtenstein")
#'
#' # fetch data for Liechtenstein using the ISO3 code
#' lie_raw_data <- wdpa_fetch("LIE")
#'
#' # plot data
#' plot(lie_raw_data)
#'
#' \dontrun{
#' # fetch data for all protected areas on the planet
#' # note that this might take some time given that the global data set is
#' # over 1 GB in size
#' global_raw_data <- wdpa_fetch("global")
#' }}
#' @export
wdpa_fetch <- function(x, download_dir = rappdirs::user_data_dir("wdpar"),
                       force_download = FALSE, verbose = interactive()) {
  # check that arguments are valid
  ## check that classes are correct
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.dir(download_dir),
                          assertthat::is.flag(force_download),
                          assertthat::is.flag(verbose))
  if (force_download && !pingr::is_online())
    stop("not connected to the internet")
  # fetch data
  if (pingr::is_online() || force_download) {
    ## find the download link and set file path to save the data
    download_url <- wdpa_url(x)
    file_name <- basename(httr::HEAD(download_url)$url)
    file_path <- file.path(download_dir, file_name)
    ## download the data
    if (!file.exists(file_path) || force_download) {
      if (verbose) {
        result <- httr::GET(download_url,
                            httr::write_disk(file_path, overwrite = TRUE),
                            httr::progress())
        message("\n")
      } else {
        result <- httr::GET(download_url,
                            httr::write_disk(file_path, overwrite = TRUE))
      }
    }
    ## verify that the file exists
    if (!file.exists(file_path))
      stop("downloading data failed")
  } else {
    file_path <- wdpa_file(x, download_dir = download_dir)
  }
  # import the data
  return(wdpa_read(file_path))
}
