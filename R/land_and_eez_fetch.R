#' @include internal.R
NULL

#' Fetch land and exclusive economic zone data
#'
#' Fetch and assemble land and exclusive economic zone (EEZ) data for the
#' world's nations.
#'
#' @param x \code{character} Country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}).
#'
#' @param crs \code{character} coordinate reference system in PROJ4 format.
#'   Defaults to \code{3395} (Mercator).
#'
#' @param tolerance \code{numeric} tolerance for simplifying geometry. Defaults
#'  to zero.
#'
#' @param download_dir \code{character} folder path to download the data.
#'  Defaults to a persistent data directory
#'  (\code{rappdirs::user_data_dir("wdpar")}).
#'
#' @param force_download \code{logical} if the data has previously been
#'   downloaded and is available at argument to \code{x}, should the data be
#'   redownloaded anyway? Defaults to \code{FALSE}.
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
#'
#' @param verbose \code{logical} should download progress be reported? Defaults
#'  to \code{TRUE} if executed in an interactive session.
#'
#' @details Data are assembled using the following steps.
#' \enumerate{
#' \item Spatial data showing the spatial extent of each country's terrestrial
#'   administrative area is downloaded (from the Database of Global
#'   Administrative Areas; \url{http://www.gadm.org/})
#' \item Spatial data showing the spatial extent of each country's exclusive
#'   economic zone are downloaded (from \url{http://marineregions.org}; approx.
#'   113 MB in size; Claus \emph{at al.} 2017))
#' \item Both data sets are scanned for invalid geometries, and invalid
#'   geometries are fixed (using \code{\link{st_parallel_make_valid}}).
#' \item Both data sets are reprojected to argument to \code{crs}.
#' \item Scan both data sets again for invalid geometries, and fix any
#'   invalid geometries that have manifested (using
#'   \code{\link{st_parallel_make_valid}}).
#' \item Geometry in both data sets are simplified using argument to
#'   \code{tolerance} (using \code{\link{st_parallel_simplify}}).
#' \item A field denoting the country code is created in the two data sets
#'   is created (\code{"ISO3"}).
#' \item Both data sets are dissolved by the country code field (\code{"ISO3"})
#'   to remove overlapping geometries.
#' \item Scan both data sets again for invalid geometries, and fix any
#'   invalid geometries that have manifested (using
#'   \code{\link{st_parallel_make_valid}}).
#' \item Areas in the exclusive economic zone datset that overlap with the
#'   terrestrial data set are removed (using \code{\link[sf]{st_difference}}).
#' \item The terrestrial data is assigned a new field \code{"TYPE"} containing
#'   \code{"LAND"}.
#' \item The exclusive economic zone data is assigned a new field
#'   \code{"TYPE"} containing \code{"EEZ"}.
#' \item The terrestrial and exclusive economic zone datsets are merged
#'   (using \code{\link[base]{rbind}}).
#' \item The merged data set is sorted by country code (\code{"ISO3"}) and
#'   (\code{"TYPE"}; using \code{\link[sf]{filter.sf}}).
#' }
#'
#' @return \code{\link[sf]{sf}} spatial data object. This object contains the
#'  following two fields: \code{"ISO3"} which shows the country associated
#'  with each geometry, \code{"TYPE"} which indicates if a geometry corresponds
#'  to the country's terrestrial land mass (\code{"LAND"}) or its
#'  exclusive economic zone (\code{"EEZ"}).
#'
#' @references
#' Claus S, De Hauwere N, Vanhoorne B, Souza Dias F, Oset Garc\'{i}a P,
#' Hernandez F & Mees J (2017) MarineRegions.org. Flanders Marine Institute.
#' Available at \url{http://www.marineregions.org}.
#'
#' @examples
#' \donttest{
#' # fetch land and eez data for Saint Kitts and Nevis
#' kna_data <- land_and_eez_fetch("KNA")
#'
#' # plot data
#' plot(kna_data)
#'
#' \dontrun{
#' # fetch land and eez data for the planet
#' global_data <- land_and_eez_fetch("globe")
#' }}
#' @export
land_and_eez_fetch <- function(x, crs = 3395, tolerance = 0,
                               download_dir = rappdirs::user_data_dir("wdpar"),
                               force_download = FALSE,
                               threads = 1, verbose = interactive()) {
  # validate arguments
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  assertthat::assert_that(is.character(x),
                          all(!is.na(x)),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.scalar(tolerance),
                          isTRUE(tolerance >= 0),
                          assertthat::is.dir(download_dir),
                          assertthat::is.flag(force_download),
                          assertthat::is.count(threads),
                          assertthat::is.flag(verbose),
                          pingr::is_online())
  # create dirs to save data
  gadm_dir <- file.path(download_dir, "gadm")
  eez_dir <- file.path(download_dir, "eez")
  dir.create(gadm_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(eez_dir, recursive = TRUE, showWarnings = FALSE)
  # convert x to country code
  if (!"global" %in% x) {
    iso3_id <- unique(vapply(x, country_code, character(1)))
  } else {
    iso3_id <- unique(countrycode::countrycode_data$iso3c)
  }
  # fetch data
  ## gadm
  if (verbose) message("fetching GADM data...")
  gadm_data <- plyr::llply(iso3_id, .progress = ifelse(verbose, "text", "none"),
                           function(x) {
    version <- "2.8"
    dl_url <- paste0("http://biogeo.ucdavis.edu/data/gadm", version, "/rds/",
                     x, "_adm0.rds")
    dl_path <- file.path(gadm_dir, basename(dl_url))
    if (!file.exists(dl_path)) {
      result <- httr::GET(dl_url, httr::write_disk(dl_path, overwrite = TRUE))
    }
    suppressMessages({out <- as(readRDS(dl_path), "sf")})
    out <- st_parallel_make_valid(out, threads = threads)
    out <- sf::st_union(out)
    out <- st_parallel_make_valid(out, threads = threads)
    return(sf::st_sf(ISO3 = x, geometry = out))
  })
  if (length(gadm_data) > 1) {
    gadm_data <- do.call(rbind, gadm_data)
  } else {
    gadm_data <- gadm_data[[1]]
  }
  ## eez
  if (verbose) message("fetching eez data...")
  if ("global" %in% x) {
    ### global eez data set
    eez_url <- paste0("http://geo.vliz.be/geoserver/MarineRegions/",
                     "wfs?service=wfs&version=1.0.0&request=GetFeature&",
                     "typeNames=MarineRegions:eez&outputFormat=SHAPE-ZIP")
    eez_path <- file.path(eez_dir, "eez.zip")
    eez_shp <- file.path(eez_dir, "eez.shp")
    if (!file.exists(eez_shp) || force_download) {
      if (verbose) {
        result <- httr::GET(eez_url, httr::write_disk(eez_path,
                                                      overwrite = TRUE),
                             httr::progress())
        message("\n")
      } else {
        result <- httr::GET(eez_url, httr::write_disk(eez_path,
                                                      overwrite = TRUE))
      }
      utils::unzip(eez_path, exdir = eez_dir)
    }
    eez_data <- sf::read_sf(file.path(eez_dir, "eez.shp"))
  } else {
    ### fetch data for one or more iso3 codes
    eez_data <- plyr::llply(iso3_id, .progress = ifelse(verbose, "text",
                                                        "none"),
                            function(x) {
      eez_url <- paste0("http://geo.vliz.be/geoserver/MarineRegions/",
                        "wfs?service=wfs&version=1.0.0&request=GetFeature&",
                        "typeNames=MarineRegions:eez&cql_filter=iso_ter1='",
                        x, "'&outputFormat=SHAPE-ZIP")
      curr_dir <- file.path(eez_dir, x)
      curr_path <- file.path(curr_dir, "eez.zip")
      curr_shp <- file.path(curr_dir, "eez.shp")
      if (!file.exists(curr_shp) || force_download) {
        dir.create(curr_dir, recursive = FALSE, showWarnings = FALSE)
        result <- httr::GET(eez_url, httr::write_disk(curr_path,
                                                      overwrite = TRUE))
        utils::unzip(curr_path, exdir = curr_dir)
      }
      return(sf::read_sf(curr_shp))
    })
    ## merge data
    if (length(eez_data) > 1) {
      eez_data <- do.call(rbind, eez_data)
    } else {
      eez_data <- eez_data[[1]]
    }
    ## set as null if no spatial data
    if (nrow(eez_data) == 0)
      eez_data <- NULL
  }
  ## repair eez data
  if (!is.null(eez_data))
    eez_data <- st_parallel_make_valid(eez_data, threads = threads)
  # processing
  ## reproject data
  gadm_data <- st_parallel_transform(gadm_data, crs = crs, threads = threads)
  gadm_data <- st_parallel_make_valid(gadm_data, threads = threads)
  if (!is.null(eez_data)) {
    eez_data <- st_parallel_transform(eez_data, crs = crs, threads = threads)
    eez_data <- st_parallel_make_valid(eez_data, threads = threads)
  }
  ## simplify geometry
  if (tolerance > 0) {
    gadm_data <- st_parallel_simplify(gadm_data,
                                      preserveTopology = TRUE,
                                      dTolerance = tolerance,
                                      threads = threads)
    if (!is.null(eez_data))
      eez_data <- st_parallel_simplify(eez_data,
                                       preserveTopology = TRUE,
                                       dTolerance = tolerance,
                                       threads = threads)
  }
  ## modify fields
  if (!is.null(eez_data)) {
    eez_data <- eez_data[, c("iso_ter1", "geometry")]
    eez_data <- summarize(group_by(eez_data, iso_ter1))
    eez_data <- rename(eez_data, ISO3 = iso_ter1)
    eez_data <- st_parallel_make_valid(eez_data, threads = threads)
  }
  ## remove holes from eez
  if (!is.null(eez_data))
    eez_data <- st_remove_holes(eez_data)
  # erase gadm from eez
  if (!is.null(eez_data)) {
    eez_data <- suppressWarnings(st_parallel_difference(eez_data,
                                   sf::st_union(gadm_data), threads = threads))
    eez_data <- st_parallel_make_valid(eez_data, threads = threads)
  }
  ## add fields indicating type
  gadm_data$TYPE <- "LAND"
  if (!is.null(eez_data))
    eez_data$TYPE <- "EEZ"
  ## merge gadm and eez
  if (!is.null(eez_data)) {
    result <- rbind(gadm_data, eez_data)
  } else {
    result <- gadm_data
  }
  ## sort data
  result <- arrange(result, ISO3, TYPE)
  # return output
  return(result)
}
