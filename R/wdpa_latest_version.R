#' @include internal.R wdpa_url.R
NULL

#' Query latest version
#'
#' Find the latest version of the combined
#' World Database on Protected Areas
#' (WDPA) and World Database on Other Effective Area-Based Conservation Measures
#' (WDOECM) dataset.
#' This is a character identifier representing the month and year (e.g.
#' `Sep2020`) the data were released.
#'
#' @details The version number is determined using a web address where the
#'   global dataset is available. For specific details, please refer to
#'   the source code for this function.
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
  download_url <- "http://wcmc.io/wdpa_current_release"
  file_name <- basename(httr::HEAD(download_url)$url)[[1]]
  wdpa_version(file_name)
}
