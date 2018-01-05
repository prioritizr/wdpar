#' @include internal.R
NULL

#' Stage data for download from the World Database on Protected Areas
#'
#' The World Database on Protected Areas updates rather frequently (at least
#' once a month). After the database is updated, the country-level data is
#' not immediately available for download. This function directs
#' the \href{http://protectedplanet.net}{ProtectedPlanet} website to
#' start preparing the data for later download. Once the data for a country
#' has been staged for downloading, this step does not need to be repated
#' until the database is updated again.
#'
#' @param \code{character} country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}).
#'
#' @param \code{logical} wait until the data is finished staging? Defaults to
#'  \code{TRUE}.
#'
#' @return \code{logical} indicating success.
#'
#' @details Note that the global data set does not need to be staged and is
#'   immediately available for download after the database has been updated.
#'
#' @seealso \code{\link{wdpa_url}}.
#'
#' @examples
#' \donttest{
#' wdpa_stage_data("LIE")
#' }
#' @export
wdpa_stage_data <- function(x, wait = TRUE) {
  # validate arguments
  assertthat::assert_that(is.character(x),
                          all(!is.na(x)),
                          pingr::is_online(),
                          assertthat::is.flag(wait))
  # check that argument to x does not contain "global"
  if ("global" %in% x)
    stop("\"global\" data set does not need to be staged")
  # find ISO3 country ids
  iso3_id <- unique(vapply(x, country_code, character(1)))
  # run of initial staging
  data_available <- vapply(iso3_id, is_wdpa_data_available, logical(1))
  # if wait until staging then check if the data has been staged
  if (wait & !all(staged)) {
    while(!data_available) {
      # wait for 5 minutes
      Sys.sleep(60 * 5)
      data_available <- vapply(iso3_id, is_wdpa_data_available, logical(1))
    }
  }
  # return success
  return(TRUE)
}
