#' @include internal.R geo.R
NULL

#' Fetch land and exclusive economic zone data
#'
#' Fetch and assemble data delineating the spatial extent of each countries
#' terrestial land mass and exclusive economic zone (EEZ).
#'
#' @param x \code{character} country for which to download data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to download data for the planet (approx. 1 GB).
#'
#' @param crs \code{character} or code{integer} coordinate reference system.
#'   The default argument is \code{3395} (Mercator).
#'
#' @param snap_tolerance \code{numeric} tolerance for snapping geometry to a
#'   grid for resolving invalid geometries. The default argument is 1 meter.
#'
#' @param simplify_tolerance \code{numeric} simplification tolerance. The
#'   default argument is 0 meters.
#'
#' @param source \code{character} name of data set to use for mapping the
#'   spatial extent of each countries land mass. Available options are
#'   \code{"gadm"} to obtain data from the Global Administrative Areas (GADM)
#'   database (\url{https://gadm.org} and \code{"ne"} to obtain data from
#'   Natural Earth (\url{http://www.naturalearthdata.com/}). Please note that
#'   the \pkg{rnaturalearth} and \pkg{rnaturalearthhighres} packages are
#'   required to obtain data from Natural Earth. Additionally, please note that
#'   the Natural  Earth data set only has a resolution of 10 km\eqn{^2} and is
#'   therefore only suitable for coarse broad-scale analyses (e.g. global
#'   analysis). The Global Administrative Areas (GADM), on the other hand,
#'   provides much higher resolution data but requires significantly more
#'   processing time. Currently, this package is unable to compile a
#'   land and exclusive economic zone data set using the Global
#'   Administrative Areas (GADM) data set in a feasible period of time (i.e.
#'   less than one week on a standard computer). The default argument is
#'   \code{"gadm"}.
#'
#' @param download_dir \code{character} folder path to download the data.
#'   The default argument is a persistent directory created specifically for
#'   this package (i.e. \code{rappdirs::user_data_dir("wdpar")}).
#'
#' @param force_download \code{logical} if the data has previously been
#'   downloaded and is available at argument to \code{x}, should the data be
#'   downloaded again? The default argument is \code{FALSE}.
#'
#' @param verbose \code{logical} should download progress be reported? The
#'   default argument is \code{FALSE}.
#'
#' @details The data are assembled using the following steps.
#'
#' \enumerate{
#'
#' \item Data delineating the spatial extent of each country's terrestrial
#'   administrative area is obtained. If the data source is specified as Natural
#'   Earth (i.e. the argument to \code{source} is \code{"ne"}), then the 10
#'   km\eqn{^2} resolution data is obtained Natural Earth
#'   (\url{http://www.naturalearthdata.com/}; using the
#'   \code{\link[rnaturalearth]{ne_countries}} function). Otherwise,
#'   if the data source is specified as the Global Administrative Areas
#'   database (i.e. the argument to \code{source} is \code{"gadm"}), then
#'   the terrestrial data is obtained from this database
#'   (\url{http://www.gadm.org/}). Note that the complete Global Administrative
#'   Areas database is approximately 800 MB in size.
#'
#' \item Data delineating the spatial extent of each country's exclusive
#'   economic zone (EEZ) are downloaded (from \url{http://marineregions.org};
#'   Claus \emph{at al.} 2017)). Note that the global data set is approximately
#'   200 MB in size.
#'
#' \item A field denoting the country code of each geometry in each data set
#'   is created (\code{"ISO3"}).
#'
#' \item Both data sets are reprojected (using the argument to \code{crs} and
#'   the \code{\link[sf]{st_transform}} function).
#'
#' \item The geometries are snapped to a grid to preempt any geometry issues
#'   and reduce computational burden (using the argument to
#'   \code{snap_tolerance} and the \code{link[lwgeom]{st_snap_to_grid}}
#'   function).
#'
#' \item The geometries are simplified to reduce computational burden (using
#'   argument to \code{simplify_tolerance} and \code{\link[sf]{st_simplify}}).
#'
#' \item Any invalid geometries are fixed (using the
#'   \code{\link[lwgeom]{st_make_valid}} function).
#'
#' \item Holes are removed from the exclusive economic zone data (using the
#'   \code{\link{st_remove_holes} function} to extract the outer shell of the
#'   exclusive economic zone for each country.
#'
#' \item Any gaps between the exclusive economic zone data and the land data
#'   are filled in and added to the exclusive economic zone data.
#'
#' \item Any invalid geometries that have manifested are fixed (using the
#'   \code{\link[lwgeom]{st_make_valid}} function).
#'
#' \item Areas in the exclusive economic zone data that overlap with the
#'   country's administrative boundaries are removed (using the
#'   \code{\link[sf]{st_difference}} function).
#'
#' \item A field in the terrestrial administrative boundary data set is created
#'   that contains the value \code{"LAND"}.
#'
#' \item A field in the economic exclusive zone data set is created that
#'   contains the value \code{"EEZ"}.
#'
#' \item The terrestrial and exclusive economic zone data sets are merged
#'   (using the \code{\link[sf]{rbind.sf}} function).
#'
#' \item The merged data set is sorted by country code (\code{"ISO3"}).
#'
#' }
#'
#' @return \code{\link[sf]{sf}} spatial data object. This object contains the
#'  following fields: \code{"ISO3"} which shows the country associated
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
#' # fetch land and EEZ data for Saint Kitts and Nevis
#' kna_data <- land_and_eez_fetch("KNA")
#'
#' # plot data
#' plot(kna_data)
#'
#' \dontrun{
#' # fetch land and EEZ data for the planet
#' # warning this can involve downloading ~500 Mb data and can take more than 10
#' # minutes to run on a standard computer
#' global_data <- land_and_eez_fetch("global")
#' }}
#' @export
land_and_eez_fetch <- function(x, crs = 3395,
                               snap_tolerance = 1,
                               simplify_tolerance = 0,
                               source = c("gadm", "ne")[1],
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
                          assertthat::is.string(source),
                          source %in% c("gadm", "ne"),
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
    iso3_id <- unique(countrycode::codelist$iso3c)
    iso3_id <- iso3_id[!is.na(iso3_id)]
  }
  # fetch data
  ## land
  if (source == "gadm") {
    if ("global" %in% x) {
      ### global gadm data set
      warning(paste("processing the complete global GADM data set can take an",
                    "infeasible period of time to complete"))
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
      land_data <- sf::read_sf(gadm_gdb, "adm0")
      land_data <- lwgeom::st_make_valid(land_data)
    } else {
      ### fetch data for one or more iso3 codes
      land_data <- lapply(iso3_id, function(x) {
        version <- "2.8"
        dl_url <- paste0("http://biogeo.ucdavis.edu/data/gadm",
                         version, "/rds/", x, "_adm0.rds")
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
      if (length(land_data) > 1) {
        land_data <- do.call(rbind, land_data)
      } else {
        land_data <- land_data[[1]]
      }
    }
  }
  if (source == "ne") {
    if (verbose) message("fetching Natural Earth data...")
    if ("global" %in% x) {
      ### global natural earth data set
      land_data <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
    } else {
      ### fetch data for one or more iso3 codes
      iso3_id_names <- countrycode::codelist$ecb.name[match(iso3_id,
                                                            codelist$iso3c)]
      land_data <- rnaturalearth::ne_countries(country = iso3_id_names,
                                               scale = 10,
                                               returnclass = "sf")
    }
    land_data <- land_data[, c("iso_a3")]
    names(land_data)[[1]] <- "ISO3"
  }
  ## eez
  if (verbose) message("fetching EEZ data...")
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
  ## add ISO3 column to eez data and remove extra columns if needed
  if (!is.null(eez_data)) {
    eez_data <- eez_data[, c("iso_ter1", "geometry")]
    names(eez_data)[[1]] <- "ISO3"
  }
  ## reproject data
  if (verbose) message("reprojecting data...")
  land_data <- sf::st_transform(land_data, crs = crs)
  if (!is.null(eez_data)) {
    eez_data <- sf::st_transform(eez_data, crs = crs)
  }
  ## snap to grid
  if (snap_tolerance > 0) {
    if (verbose) message("snapping data to grid...")
    land_data <- lwgeom::st_snap_to_grid(land_data, snap_tolerance)
    if (!is.null(eez_data))
      eez_data <- lwgeom::st_snap_to_grid(eez_data, snap_tolerance)
  }
  ## simplify data
  if (simplify_tolerance > 0) {
    if (verbose) message("simplifying data...")
    land_data <- sf::st_simplify(land_data, TRUE, simplify_tolerance)
    if (!is.null(eez_data))
      eez_data <- sf::st_simplify(eez_data, TRUE, simplify_tolerance)
  }
  ## remove holes from eez
  if (!is.null(eez_data)) {
    if (verbose) message("removing holes from EEZ...")
    eez_data <- st_remove_holes(eez_data)
  }
  ## repair data
  if (verbose) message("repairing geometry...")
  land_data <- lwgeom::st_make_valid(land_data)
  if (!is.null(eez_data)) {
    eez_data <- lwgeom::st_make_valid(eez_data)
  }
  ## fill in gaps in eez
  if (!is.null(eez_data)) {
    if (verbose) message("filling in EEZ gaps...")
    land_agg <- stats::aggregate(land_data, by = list(land_data$ISO3),
                                 FUN = `[[`, 1)[, -1]
    gap_data <- rbind(eez_data, land_agg)
    o1 <<- gap_data
    gap_data <- stats::aggregate(gap_data, by = list(gap_data$ISO3),
                                 FUN = `[[`, 1)[, -1]
    o2 <<- gap_data
    gap_data <- st_extract_holes(gap_data)
    o3 <<- gap_data
    gap_data <- gap_data[!sf::st_is_empty(gap_data), ]
    o4 <<- gap_data
    eez_data <- rbind(eez_data, gap_data)
    o5 <<- eez_data
    eez_data <- stats::aggregate(eez_data, by = list(eez_data$ISO3),
                                 FUN = `[[`, 1)[, -1]
    o6 <<- eez_data
    eez_data <- lwgeom::st_make_valid(eez_data)
    o7 <<- eez_data
  }
  ## extract polygons
  if (verbose) message("extracting polygons from data...")
  land_data <- st_subset_polygons(land_data)
  if (!is.null(eez_data))
    eez_data <- st_subset_polygons(eez_data)
  ## repair data
  if (verbose) message("repairing geometry...")
  land_data <- lwgeom::st_make_valid(land_data)
  if (!is.null(eez_data)) {
    eez_data <- lwgeom::st_make_valid(eez_data)
  }
  ## erase land from eez
  if (!is.null(eez_data)) {
    if (verbose) message("erasing land from EEZ...")
    print(1)
    gadm_union <- lwgeom::st_make_valid(sf::st_union(sf::st_combine(land_agg)))
    print(2)
    eez_data <- suppressWarnings(sf::st_difference(eez_data, gadm_union))
  }
  ## repair data
  if (!is.null(eez_data)) {
    if (verbose) message("repairing geometry...")
    eez_data <- lwgeom::st_make_valid(eez_data)
  }
  ## add fields indicating type
  if (verbose) message("adding \"TYPE\" fields...")
  land_data$TYPE <- "LAND"
  if (!is.null(eez_data))
    eez_data$TYPE <- "EEZ"
  ## merge land and eez
  if (verbose) message("merging land and EEZ data...")
  if (!is.null(eez_data)) {
    result <- rbind(land_data, eez_data)
  } else {
    result <- land_data
  }
  ## sort data
  if (verbose) message("sorting data...")
  result <- result[order(result$ISO3, result$TYPE), ]
  # return output
  return(result)
}
