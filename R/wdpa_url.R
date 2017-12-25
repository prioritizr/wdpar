#' @include internal.R
NULL

#' Download URL for the World Database on Protected Areas
#'
#' Obtain the URL for downloading data from the World Database on Protected
#' Areas (WDPA).
#'
#' @param x \code{character} Country for desired data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to obtain the URL for the global data set.
#'
#' @return \code{character} URL.
#'
#' @seealso \code{\link{wdpa_fetch}}, \code{\link[countrycode]{countrycode}}.
#'
#' @examples
#' # obtain url for USA data
#' usa_url <- wdpa_url("United States of America")
#' print(usa_url)
#'
#' # obtain url for USA data (using the ISO3 code)
#' usa_url <- wdpa_url("USA")
#' print(usa_url)
#'
#' # download link for global data
#' global_url <- wdpa_url("global")
#' print(global_url)
#' @export
wdpa_url <- function(x) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(x), pingr::is_online())
  # find url
  if (x == "global") {
    download_url <- "http://wcmc.io/wdpa_current_release"
  } else {
    ## convert x to country ISO3 code
    x <- country_code(x)
    ## generate potential urls since http://protectplanet.net can sometimes
    ## be a few months behind the current date, and also oddly enough
    ## ahead of the current date
    possible_months <- format(Sys.Date() - (seq(-2, 10) * 30), "%b%Y")
    potential_urls <- paste0("https://www.protectedplanet.net/",
                             "downloads/WDPA_", possible_months, "_", x,
                             "?type=shapefile")
    ## find working url
    found_url <- FALSE
    for (i in seq_along(potential_urls)) {
      if (!httr::http_error(potential_urls[i])) {
        download_url <- potential_urls[i]
        found_url <- TRUE
        break()
      }
    }
    ## check that at least one working url was found
    if (!found_url)
      stop("data is not yet available for download at ",
           "http://protectedplanet.net")
  }
  # return url
  return(download_url)
}
