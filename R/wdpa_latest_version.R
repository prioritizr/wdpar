#' @include internal.R wdpa_url.R
NULL

#' Query latest version of the World Database on Protected Areas
#'
#' Find the latest version of the World Database on Protected Areas dataset.
#' This is a character identifier representing the month and year (e.g.
#' `Sep2020`) the data were released.
#'
#' @details The version number can be determined based on the file name for
#'  the global dataset (currently available at:
#'  <https://wcmc.io/wdpa_current_release>)
#'
#' @return `character` version of the dataset.
#'
#' @examples
#' \dontrun{
#' # find the latest version
#' wdpa_latest_version()
#' }
#'
#' @export
wdpa_latest_version <- function() {
  assertthat::assert_that(curl::has_internet())
  download_url <- wdpa_url("global")
  file_name <- basename(httr::HEAD(download_url)$url)[[1]]
  wdpa_version(file_name)
}
