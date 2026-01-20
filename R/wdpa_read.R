#' @include internal.R
NULL

#' Read data
#'
#' Read data obtained from
#' [Protected Planet](https://www.protectedplanet.net/en).
#' Specifically, this function is designed to import data obtained from
#' the World Database on Protected Areas
#' (WDPA) and the World Database on Other Effective Area-Based Conservation
#' Measures (WDOECM).
#'
#' @param x `character` file name for a zip archive file downloaded from
#'   <https://www.protectedplanet.net/en>.
#'
#' @param n `integer` number of records to import per data source.
#'   Defaults to `NULL` such that all data are imported.
#'
#' @details
#' This function assumes that data have previously been downloaded to
#' your computer, and need to import the data.
#' After importing the data, it is strongly recommended to clean the data
#' prior to analysis (see [wdpa_clean()]).
#'
#' @inheritSection wdpa_fetch Data source
#'
#' @return A [sf::sf()] object.
#'
#' @seealso [wdpa_fetch()], [wdpa_clean()].
#'
#' @inherit wdpa_fetch references
#'
#' @examples
#' \dontrun{
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
wdpa_read <- function(x, n = NULL) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.readable(x),
                          assertthat::has_extension(x, "zip"),
                          startsWith(basename(x), "WDPA_"),
                          file.exists(x),
                          inherits(n, c("numeric", "NULL")))
  if (!is.null(n)) {
   assertthat::assert_that(assertthat::is.count(n),
                           assertthat::noNA(n))
  }

  # unzip the folder
  tdir <- file.path(tempdir(), basename(tempfile()))
  dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
  archive::archive_extract(x, dir = tdir)
  # determine version
  month_year <- strsplit(basename(x), "_", fixed = TRUE)[[1]][[2]]

  # load data
  ## if zip file contains sub-zip files, then extract them
  zip_path <- dir(tdir, "^.*\\.zip$", recursive = TRUE, full.names = TRUE)
  if (length(zip_path) > 0) {
    result <- Map(
      archive::archive_extract, zip_path,
      dir = gsub(".zip", "", zip_path, fixed = TRUE)
    )
  }
  ## try and find shapefiles and gdb in unzipped files
  shp_path <- dir(tdir, "^.*\\.shp$", recursive = TRUE, full.names = TRUE)
  gdb_path <- dir(tdir, "^.*\\.gdb$", recursive = TRUE,
                   full.names = TRUE, include.dirs = TRUE)
  if (
    (length(shp_path) > 0) &&
    (identical(length(gdb_path), 0L))
  ) {
    ## if shapefiles present, then import them
    wdpa_data <- lapply(shp_path, read_sf_n, n = n)
    if (length(wdpa_data) > 1) {
      wdpa_data <- wdpa_data[vapply(wdpa_data, nrow, integer(1)) > 0]
    }
  } else if (
    (length(gdb_path) > 0) &&
    (identical(length(shp_path), 0L))
  ) {
    ### if has file geodatabase, then import each of the spatial layers in them
    wdpa_data <- lapply(gdb_path, function(x) {
      lyrs <- sf::st_layers(x)
      is_lyrs_spatial <- !vapply(lyrs$crs, is.na, logical(1))
      if (!any(is_lyrs_spatial)) return(NULL) # nocov
      lapply(lyrs$name[is_lyrs_spatial], read_sf_n, dsn = x)
    })
    wdpa_data <- wdpa_data[!vapply(wdpa_data, is.null, logical(1))]
    wdpa_data <- unlist(wdpa_data, recursive = FALSE, use.names = FALSE)
  } else if (
    # nocov start
    (identical(length(gdb_path), 0L)) &&
    (identical(length(shp_path), 0L))
    # nocov end
  ) {
    # nocov start
    stop(
      "Couldn't find shapefile or file geodatabase inside zip file.",
      call. = FALSE
    )
    # nocov end
  } else {
    # nocov start
    stop(
      paste(
        "Couldn't import data because both shapefile and",
        "file geodatabase; inside zip file."
      ),
      call. = FALSE
    )
    # nocov end
  }

  # if needed, merge layers togeather
  if (length(wdpa_data) > 1) {
    col_names <- Reduce(base::intersect, lapply(wdpa_data, names))
    wdpa_data <- lapply(wdpa_data, function(x) x[, col_names])
    wdpa_data <- do.call(rbind, wdpa_data)
  } else {
    wdpa_data <- wdpa_data[[1]]
  }

  # cleanup
  unlink(tdir, force = TRUE)

  # return data
  return(wdpa_data)
}
