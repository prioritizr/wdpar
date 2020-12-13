#' @include internal.R
NULL

#' Read data from the World Database on Protected Areas
#'
#' Read data from the World Database on Protected Areas from a local file.
#' This function assumes that the data has already been downloaded to
#' your computer, see the [wdpa_fetch()] function for automatically
#' downloading and importing the data into the current session.
#'
#' @param x `character` file name for a zip archive file downloaded from
#'   <https://www.protectedplanet.net/en>.
#'
#' @param n `integer` number of records to import per data source.
#'   Defaults to `NULL` such that all data are imported.
#'
#' @return [sf::sf()] object.
#'
#' @seealso [wdpa_fetch()], [wdpa_clean()],
#'   <https://www.protectedplanet.net/en>.
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
  utils::unzip(x, exdir = tdir)
  # determine version
  month_year <- strsplit(basename(x), "_", fixed = TRUE)[[1]][[2]]
  # load data
  if (grepl("Public", basename(x))) {
    ## load global data
    ### find geodatabase(s)
    gdb_paths <-  dir(tdir, "^.*\\.gdb$", recursive = TRUE,
                      full.names = TRUE, include.dirs = TRUE)
    ## import data from geodatabase(s)
    if (length(gdb_paths) == 1) {
      ### WDPA < Dec2020
      wdpa_point_data <-
        read_sf_n(gdb_paths, paste0("WDPA_point_", month_year), n)
      wdpa_polygon_data <-
        read_sf_n(gdb_paths, paste0("WDPA_poly_", month_year), n)
    } else if (length(gdb_paths) == 2) {
      ### WDPA >= Dec2020
      point_path <-
        grep("point", gdb_paths, value = TRUE, ignore.case = TRUE)
      polygon_path <-
        grep("polygon", gdb_paths,  value = TRUE, ignore.case = TRUE)
      wdpa_point_data <-
        read_sf_n(point_path, "WDPA_WDOECM_wdpa_gdb_points", n)
      wdpa_polygon_data <-
        read_sf_n(polygon_path, "WDPA_WDOECM_wdpa_gdb_polygons", n)
    } else {
      stop("Global data format not recognized.")
    }
    ## extract point and polygon data
    ## merge data together
    polygon_matching_cols <- which(names(wdpa_polygon_data) %in%
                                   names(wdpa_point_data))
    point_matching_cols <- which(names(wdpa_point_data) %in%
                                 names(wdpa_polygon_data))
    wdpa_polygon_data <- wdpa_polygon_data[, polygon_matching_cols]
    wdpa_point_data <- wdpa_point_data[, point_matching_cols]
    wdpa_data <- rbind(wdpa_polygon_data, wdpa_point_data)
  } else {
    ## extract any data stored in zip files
    zip_path <- dir(tdir, "^.*\\.zip$", recursive = TRUE, full.names = TRUE)
    if (length(zip_path) > 0)
      result <- Map(utils::unzip, zip_path,
                    exdir = gsub(".zip", "", zip_path, fixed = TRUE))
    ## import shapefile data
    shapefile_path <- dir(tdir, "^.*\\.shp$", recursive = TRUE,
                          full.names = TRUE)
    wdpa_data <- lapply(shapefile_path, sf::read_sf, n = n)
    ## merge shapefile data together
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
