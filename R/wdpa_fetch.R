#' @include internal.R
NULL

#' Fetch data from the World Database on Protected Areas
#'
#' Download data from the World Database on Protected Areas (WDPA)
#' (available at \url{http://protectedplanet.net}) and import it. Note that
#' if the data has previously been downloaded and is available
#' in the specified download folder, then the data will simply be imported.
#'
#' @param x \code{character} Country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to download all of the protected areas available
#'   in the database (approx. 1.1 GB).
#'
#' @param download_dir \code{character} folder path to download the data.
#'  Defaults to the temporary directory (\code{\link{tempdir}}).
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
#'   clean the data prior to analysis. It is worth noting that the returned data
#'   for protected areas represented using both polygon and point geometries.
#'   Check out the \code{\link{clean_wdpa}} function to clean the data
#'   according to standard practices.
#'
#' @seealso \code{\link{wdpa_clean}}, \code{\link[countrycode]{countrycode}},
#'   \url{http://protectedplanet.net}
#'
#' @return \code{\link[sf]{sf}} simple features object.
#'
#' @examples
#' \donttest{
#' # fetch data for Liechtenstein
#' lie_data <- wdpa_fetch("Liechtenstein")
#'
#' # fetch data for Liechtenstein using the ISO3 code
#' lie_data <- wdpa_fetch("LIE", force_download = TRUE)
#' }
#' \dontrun{
#' # fetch data for the globe
#' # note that this might take some time given that the global data set is
#' # approx. 1.1 GB in size
#' global_data <- wdpa_fetch("global")
#' }
#' @export
wdpa_fetch <- function(x, download_dir = tempdir(), force_download = FALSE,
                       verbose = interactive()) {
  # check that arguments are valid
  ## check that classes are correct
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.dir(download_dir),
                          assertthat::is.flag(force_download),
                          assertthat::is.flag(verbose))
  ## check that country codes/names are correct
  if (x != "global") {
    if (length(x) == 3) {
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
  } else {
    country_code <- "global"
  }
  # download the data
  ## determine download links
  if (country_code == "global") {
    download_url <- "http://wcmc.io/wdpa_current_release"
  } else {
    ### generate potential urls since http://protectplanet.net can sometimes be
    ### a few months behind the current date
    last_few_months <- format(Sys.Date() - (seq(0, 10) * 30), "%b%Y")
    potential_urls <- paste0("https://www.protectedplanet.net/downloads/WDPA_",
                             last_few_months, "_", country_code,
                             "?type=shapefile")
    ## find working url
    found_url <- FALSE
    for (i in seq_along(potential_urls)) {
      if (httr::http_error(potential_urls[i])) {
        download_url <- potential_urls[i]
        found_url <- TRUE
        break()
      }
    }
    ### check that at least one working url was found
    if (!found_url)
      stop("no valid download links available at http://protectedplanet.net - ",
           "check your internet connection?")
  }
  ## find correct filename with which to save data
  file_name <- basename(httr::HEAD(download_url)$url)
  file_path <- file.path(download_dir, file_name)
  ## download the file if required
  if (!file.exists(file_path) || force_download) {
    if (verbose) {
      result <- httr::GET(download_url, httr::write_disk(file_path),
                         httr::progress(), overwrite = TRUE)
    } else {
      result <- httr::GET(download_url, httr::write_disk(file_path),
                          overwrite = TRUE)
    }
  }
  ## verify that the file exists
  if (!file.exists(file_path))
    stop("downloading data failed")
  # load the data
  ## unzip the folder
  tdir <- file.path(tempdir(), tempfile())
  unzip(file_path, tdir)
  ## load data
  month_year <- strsplit(file_name, "_", fixed = TRUE)[[1]][[2]]
  if (country_code == "global") {
    gdb_path <- dir(tdir, "^.*\\.gdb$", recursive = TRUE)[[1]]
    wdpa_polygon_data <- sf::st_read(gdb_path, paste0("WDPA_poly_", month_year))
    wdpa_point_data <- sf::st_read(gdb_path, paste0("WDPA_point_", month_year))
    wdpa_data <- sf::rbind(wdpa_polygon_data, wdpa_point_data)
  } else {
    shapefile_path <- dir(tdir, "^.*\\.shp$", recursive = TRUE)[[1]]
    wdpa_data <- sf::st_read(shapefile_path)
  }
  ## cleanup
  unlink(tdir)
  # return file path
  return(wdpa_data)
}
