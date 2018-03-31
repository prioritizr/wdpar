#' @include internal.R geo.R
NULL

#' Fetch land and exclusive economic zone data
#'
#' Fetch and assemble land and exclusive economic zone (EEZ) data for the
#' world's nations.
#'
#' @param x \code{character} country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to download data for the planet (approx. 1 GB).
#'
#' @param crs \code{character} or code{integer} coordinate reference system.
#'   Defaults to \code{3395} (Mercator).
#'
#' @param snap_tolerance \code{numeric} tolerance for snapping geometry to a
#'   grid for resolving invalid geometries. Defaults to 1 meter.
#'
#' @param simplify_tolerance \code{numeric} simplification tolerance. Defaults
#'  to 0 metres.
#'
#' @param download_dir \code{character} folder path to download the data.
#'  Defaults to a persistent data directory
#'  (\code{rappdirs::user_data_dir("wdpar")}).
#'
#' @param force_download \code{logical} if the data has previously been
#'   downloaded and is available at argument to \code{x}, should the data be
#'   redownloaded anyway? Defaults to \code{FALSE}.
#'
#' @param verbose \code{logical} should download progress be reported? Defaults
#'  to \code{FALSE}.
#'
#' @details Data are assembled using the following steps.
#'
#' \enumerate{
#'
#' \item Data delineating the spatial extent of each country's terrestrial
#'   administrative area is downloaded (from the Database of Global
#'   Administrative Areas; \url{http://www.gadm.org/}). Note that the global
#'   data set is approximately 800 MB in size.
#'
#' \item Data delineating the spatial extent of each country's exclusive
#'   economic zone are downloaded (from \url{http://marineregions.org}; Claus
#'   \emph{at al.} 2017)). Note that the global data set is approximately 200
#'   MB in size.
#'
#' \item Both data sets are scanned for invalid geometries, and invalid
#'   geometries are fixed (using \code{\link[lwgeom]{st_make_valid}}).
#'
#' \item Both data sets are reprojected to argument to \code{crs} (using
#'   \code{\link[sf]{st_transform}}).
#'
#' \item Fix any invalid geometries that have manifested (using
#'   (using \code{\link[lwgeom]{st_make_valid}}).
#'
#' \item The geometries are snapped to a grid to fix any remaining unresolved
#'   geometry issues (using argument to \code{snap_tolerance} and
#'   \code{link[lwgeom]{st_snap_to_grid}}).
#'
#' \item The geometries are simplified to reduce computational burden (using
#'   argument to \code{simplify_tolerance} and \code{\link[sf]{st_simplify}}).
#'
#' \item Fix any invalid geometries that have manifested (using
#'   (using \code{\link[lwgeom]{st_make_valid}}).
#'
#' \item A field denoting the country code is created (\code{"ISO3"}).
#'
#' \item Both data sets are dissolved by the country code field (\code{"ISO3"})
#'   to remove overlapping geometries.
#'
#' \item Fix any invalid geometries that have manifested (using
#'   (using \code{\link[lwgeom]{st_make_valid}}).
#'
#' \item Areas in the exclusive economic zone data that overlap with the
#'   country's administrative boundaries are removed (using
#'   \code{\link[sf]{st_difference}}).
#'
#' \item A field in the terrestrial administrative boundary data set is created
#'   that contains thhe value \code{"LAND"}.
#'
#' \item A field in the economic exclusive zone data set is created that
#'   contains the value \code{"EEZ"}.
#'
#' \item The terrestrial and exclusive economic zone dats ets are merged
#'   (using \code{\link[sf]{rbind.sf}}).
#'
#' \item The merged data set is sorted by country code (\code{"ISO3"}) and
#'   (\code{"TYPE"}; using \code{\link[sf]{filter.sf}}).
#'
#' }
#'
#' @return \code{\link[sf]{sf}} spatial data object. This object contains the
#'  following two fields: \code{"ISO3"} which shows the country associated
#'  with each geometry, \code{"TYPE"} which indicates if a geometry corresponds
#'  to the country's terrestrial administrative area (\code{"LAND"}) or its
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
#' global_data <- land_and_eez_fetch("global")
#' }}
#' @export
land_and_eez_fetch <- function(x, crs = 3395, snap_tolerance = 1,
                               simplify_tolerance = 0,
                               download_dir = rappdirs::user_data_dir("wdpar"),
                               force_download = FALSE, verbose = FALSE) {
  # validate arguments
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  assertthat::assert_that(is.character(x),
                          all(!is.na(x)),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.scalar(snap_tolerance),
                          isTRUE(snap_tolerance >= 0),
                          assertthat::is.scalar(simplify_tolerance),
                          isTRUE(simplify_tolerance >= 0),
                          assertthat::is.dir(download_dir),
                          assertthat::is.flag(force_download),
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
    iso3_id <- iso3_id[!is.na(iso3_id)]
  }
  # fetch data
  ## gadm
  if (verbose) message("fetching GADM data...")
  if ("global" %in% x) {
    ### global gadm data set
    gadm_url <- paste0("http://data.biogeo.ucdavis.edu/data/",
                       "gadm2.8/gadm28_levels.gdb.zip")
    gadm_path <- file.path(gadm_dir, "gadm28_levels.gdb.zip")
    gadm_gdb <- file.path(gadm_dir, "gadm28_levels.gdb")
    if (!file.exists(gadm_gdb) || force_download) {
      if (verbose) {
        result <- httr::GET(gadm_url,
                            httr::write_disk(gadm_path, overwrite = TRUE),
                            httr::progress())
        message("\n")
      } else {
        result <- httr::GET(gadm_url, httr::write_disk(gadm_path,
                                                       overwrite = TRUE))
      }
      utils::unzip(gadm_path, exdir = gadm_dir)
    }
    gadm_data <- sf::read_sf(gadm_gdb, "adm0")
    gadm_data <- lwgeom::st_make_valid(gadm_data)
  } else {
    ### fetch data for one or more iso3 codes
    gadm_data <- lapply(iso3_id, function(x) {
      version <- "2.8"
      dl_url <- paste0("http://biogeo.ucdavis.edu/data/gadm", version, "/rds/",
                       x, "_adm0.rds")
      dl_path <- file.path(gadm_dir, basename(dl_url))
      if (!file.exists(dl_path)) {
        if (verbose) {
          result <- httr::GET(dl_url, httr::write_disk(dl_path,
                                                       overwrite = TRUE),
                                                       httr::progress())
          message("\n")
        } else {
          result <- httr::GET(dl_url, httr::write_disk(dl_path,
                                                       overwrite = TRUE))
        }
      }
      # the sp package is required to read the gadm data, so here we make
      # a call to a function from sp so we can list it in Imports
      # without an error from R CMD check
      {s <- sp::SpatialPolygonsDataFrame}
      suppressMessages({out <- methods::as(readRDS(dl_path), "sf")})
      out <- lwgeom::st_make_valid(sf::st_set_precision(out, 1000000))
      out <- sf::st_union(out)
      out <- lwgeom::st_make_valid(sf::st_set_precision(out, 1000000))
      return(sf::st_sf(ISO3 = x, geometry = out))
    })
    if (length(gadm_data) > 1) {
      gadm_data <- do.call(rbind, gadm_data)
    } else {
      gadm_data <- gadm_data[[1]]
    }
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
        result <- httr::GET(eez_url,
                            httr::write_disk(eez_path, overwrite = TRUE),
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
    eez_data <- lapply(iso3_id, function(x) {
      eez_url <- paste0("http://geo.vliz.be/geoserver/MarineRegions/",
                        "wfs?service=wfs&version=1.0.0&request=GetFeature&",
                        "typeNames=MarineRegions:eez&cql_filter=iso_ter1='",
                        x, "'&outputFormat=SHAPE-ZIP")
      curr_dir <- file.path(eez_dir, x)
      curr_path <- file.path(curr_dir, "eez.zip")
      curr_shp <- file.path(curr_dir, "eez.shp")
      if (!file.exists(curr_shp) || force_download) {
        dir.create(curr_dir, recursive = FALSE, showWarnings = FALSE)
        if (verbose) {
          result <- httr::GET(eez_url, httr::write_disk(curr_path,
                                                        overwrite = TRUE),
                                                        httr::progress())
          message("\n")
        } else {
          result <- httr::GET(eez_url, httr::write_disk(curr_path,
                                                        overwrite = TRUE))
        }
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
  # processing
  ## repair eez data
  if (!is.null(eez_data))
    eez_data <- lwgeom::st_make_valid(sf::st_set_precision(eez_data, 1000000))
  ## reproject and repair data
  gadm_data <- sf::st_transform(gadm_data, crs = crs)
  gadm_data <- lwgeom::st_make_valid(sf::st_set_precision(gadm_data, 1000000))
  if (!is.null(eez_data)) {
    eez_data <- sf::st_transform(eez_data, crs = crs)
    eez_data <- lwgeom::st_make_valid(sf::st_set_precision(eez_data, 1000000))
  }
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    gadm_data <- lwgeom::st_snap_to_grid(gadm_data, snap_tolerance)
    if (!is.null(eez_data))
      eez_data <- lwgeom::st_snap_to_grid(eez_data, snap_tolerance)
  }
  ## repair data
  gadm_data <- lwgeom::st_make_valid(gadm_data)
  if (!is.null(eez_data))
    eez_data <- lwgeom::st_make_valid(eez_data)
  ## simplify data
  if (simplify_tolerance > 0) {
    gadm_data <- sf::st_simplify(gadm_data, TRUE, simplify_tolerance)
    if (!is.null(eez_data))
      eez_data <- sf::st_simplify(eez_data, TRUE, simplify_tolerance)
  }
  ## repair data
  gadm_data <- lwgeom::st_make_valid(gadm_data)
  if (!is.null(eez_data))
    eez_data <- lwgeom::st_make_valid(eez_data)
  ## extract polygons
  gadm_data <- st_subset_polygons(gadm_data)
  if (!is.null(eez_data))
    eez_data <- st_subset_polygons(eez_data)
  ## modify fields
  if (!is.null(eez_data)) {
    eez_data <- eez_data[, c("iso_ter1", "geometry")]
    names(eez_data)[[1]] <- "ISO3"
    names(attr(eez_data, "agr")) <- "ISO3"
    eez_data <- stats::aggregate(eez_data, by = list(eez_data$ISO3),
                                 FUN = function(x) x[[1]])[, -1]
    eez_data <- lwgeom::st_make_valid(eez_data)
  }
  ## remove holes from eez
  if (!is.null(eez_data))
    eez_data <- st_remove_holes(eez_data)
  ## erase gadm from eez
  if (!is.null(eez_data)) {
    gadm_union <- lwgeom::st_make_valid(sf::st_union(gadm_data))
    eez_data <- suppressWarnings(sf::st_difference(eez_data, gadm_union))
    eez_data <- lwgeom::st_make_valid(eez_data)
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
  result <- result[order(result$ISO3, result$TYPE), ]
  # return output
  return(result)
}
