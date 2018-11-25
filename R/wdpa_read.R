#' @include internal.R
NULL

#' Read data from the World Database on Protected Areas
#'
#' Read data from the World Database on Protected Areas from a local file.
#' This function assumes that the data has already been downloaded to
#' your computer, see the \code{\link{wdpa_fetch}} function for automatically
#' downloading and importing the data into the current session.
#'
#' @param x \code{character} file name for a zip archive file downloaded from
#'   \url{http://protectedplanet.net}.
#'
#' @return \code{\link[sf]{sf}} object.
#'
#' @seealso \code{\link{wdpa_fetch}}, \code{\link{wdpa_clean}},
#'   \url{http://protectedplanet.net}.
#'
#' @examples
#' \donttest{
#' # find url for Liechtenstein dataset
#' download_url <- wdpa_url("LIE", wait = TRUE)
#'
#' # path to save file zipfile with data
#' path <- tempfile(pattern = "WDPA_", fileext = ".zip")
#'
#' # download zipfile
#' result <- httr::GET(download_url, httr::write_disk(path))
#'
#' # load data
#' lie_raw_data <- wdpa_read(path)
#'
#' # plot data
#' plot(lie_raw_data)
#' }
#' @export
wdpa_read <- function(x) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.readable(x),
                          assertthat::has_extension(x, "zip"),
                          startsWith(basename(x), "WDPA_"),
                          file.exists(x))
  # unzip the folder
  tdir <- file.path(tempdir(), basename(tempfile()))
  dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(x, exdir = tdir)
  # load data
  month_year <- strsplit(basename(x), "_", fixed = TRUE)[[1]][[2]]
  if (grepl("Public", basename(x))) {
    gdb_path <- dir(tdir, "^.*\\.gdb$", recursive = TRUE,
                    full.names = TRUE, include.dirs = TRUE)[[1]]
    wdpa_polygon_data <- sf::read_sf(gdb_path, paste0("WDPA_poly_", month_year))
    wdpa_point_data <- sf::read_sf(gdb_path, paste0("WDPA_point_", month_year))
    polygon_matching_cols <- which(names(wdpa_polygon_data) %in%
                                   names(wdpa_point_data))
    point_matching_cols <- which(names(wdpa_point_data) %in%
                                 names(wdpa_polygon_data))
    wdpa_polygon_data <- wdpa_polygon_data[, polygon_matching_cols]
    wdpa_point_data <- wdpa_point_data[, point_matching_cols]
    wdpa_data <- rbind(wdpa_polygon_data, wdpa_point_data)
  } else {
    shapefile_path <- dir(tdir, "^.*\\.shp$", recursive = TRUE,
                          full.names = TRUE)
    wdpa_data <- lapply(shapefile_path, sf::read_sf)
    if (length(wdpa_data) > 1) {
      col_names <- Reduce(base::intersect, lapply(wdpa_data, names))
      wdpa_data <- lapply(wdpa_data, function(x) x[, col_names])
      wdpa_data <- do.call(rbind, wdpa_data)
    } else {
      wdpa_data <- wdpa_data[[1]]
    }
  }
  # cleanup
  unlink(tdir)
  # return data
  return(wdpa_data)
}
