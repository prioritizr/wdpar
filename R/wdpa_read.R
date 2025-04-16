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
  is_global_dataset <-
    grepl("Public", basename(x)) &&
    (
      endsWith(gsub(".zip", "", basename(x), fixed = TRUE), "Public")  ||
      endsWith(gsub(".gdb.zip", "", basename(x), fixed = TRUE), "Public")
    )
  if (isTRUE(is_global_dataset)) {
    ## load global data
    ### find geodatabase(s)
    gdb_paths <-  dir(tdir, "^.*\\.gdb$", recursive = TRUE,
                      full.names = TRUE, include.dirs = TRUE)
    ## import data from geodatabase(s)
    if (length(gdb_paths) == 1) {
      wdpa_lyrs <- sf::st_layers(gdb_paths)
      point_path <-
        grep("point", wdpa_lyrs$name, value = TRUE, ignore.case = TRUE)
      polygon_path <-
        grep("poly", wdpa_lyrs$name, value = TRUE, ignore.case = TRUE)
      assertthat::assert_that(
        length(point_path) == 1,
        length(polygon_path) == 1,
        !identical(polygon_path, point_path),
        msg = "global data format not recognized.")
      wdpa_point_data <- read_sf_n(gdb_paths, point_path, n)
      wdpa_polygon_data <- read_sf_n(gdb_paths, polygon_path, n)
    } else if (length(gdb_paths) == 2) {
      ### WDPA <= Dec2020
      #nocov start
      point_path <-
        grep("point", gdb_paths, value = TRUE, ignore.case = TRUE)
      polygon_path <-
        grep("poly", gdb_paths,  value = TRUE, ignore.case = TRUE)
      assertthat::assert_that(
        length(point_path) == 1,
        length(polygon_path) == 1,
        !identical(polygon_path, point_path),
        msg = "global data format not recognized.")
      wdpa_point_data <-
        read_sf_n(point_path, "WDPA_WDOECM_wdpa_gdb_points", n)
      wdpa_polygon_data <-
        read_sf_n(polygon_path, "WDPA_WDOECM_wdpa_gdb_polygons", n)
      #nocov end
    } else {
      stop("global data format not recognized.") #nocov
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
    ## load country-level data
    ### extract data stored in zip files
    zip_path <- dir(tdir, "^.*\\.zip$", recursive = TRUE, full.names = TRUE)
    if (length(zip_path) > 0) {
      result <- Map(utils::unzip, zip_path,
                    exdir = gsub(".zip", "", zip_path, fixed = TRUE))
    }
    ### try and find shapefiles and gdb in unzipped files
    shapefile_path <- dir(tdir, "^.*\\.shp$", recursive = TRUE,
                          full.names = TRUE)
    gdb_path <- dir(tdir, "^.*\\.gdb$", recursive = TRUE,
                    full.names = TRUE, include.dirs = TRUE)
    if (length(shapefile_path) > 0) {
      ### if has shapefiles, then...
      ### import shapefile data
      wdpa_data <- lapply(shapefile_path, read_sf_n, n = n)
      ### exclude any shapefiles that are empty and don't contain any data
      if (length(wdpa_data) > 1) {
        wdpa_data <- wdpa_data[vapply(wdpa_data, nrow, integer(1)) > 0]
      }
    } else if (length(gdb_path) > 0) {
      ### if has file geodatabase, then...
      ### determine which layers to import
      d <- sf::st_layers(gdb_path)
      is_d_spatial <- !vapply(d$crs, is.na, logical(1))
      wdpa_data <- lapply(d$name[is_d_spatial], sf::read_sf, dsn = gdb_path)
    } else {
      stop(
        "Couldn't find shapefile or file geodatabase inside zip file.",
        call. = FALSE
      )
    }
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
