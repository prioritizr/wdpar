#' @include internal.R geo.R
NULL

#' Clean data from the World Database on Protected Areas
#'
#' Clean data from the World Database on Protected Areas (WDPA).
#'
#' @param x \code{\link[sf]{sf}} object containing data from the WDPA.
#'
#' @param crs \code{character} coordinate reference system in PROJ4 format.
#'   Defaults to \code{3395} (Mercator).
#'
#' @param snap_tolerance \code{numeric} tolerance for snapping geometry to a
#'   grid for resolving invalid geometries. Defaults to 1 meter.
#'
#' @param simplify_tolerance \code{numeric} simplification tolerance.
#'   Defaults to 0 meters.
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
#'
#' @param verbose \code{logical} should progress on data cleaning be reported?
#'   Defaults to \code{FALSE}.
#'
#' @details This function cleans data from World Database on Protected Areas
#'   following best practices (Butchart \emph{et al.} 2015, Runge \emph{et al.}
#'   2015,
#'   \url{https://protectedplanet.net/c/calculating-protected-area-coverage}).
#'   \enumerate{
#'   \item Repair invalid geometry (using \code{\link{st_parallel_make_valid}}).
#'   \item Exclude protected areas that are not currently implemented
#'     (retain areas with the status \code{"Designated"},
#'     \code{"Inscribed"}, \code{"Established"}).
#'   \item Exclude UNESCO Biosphere Reserves (Coetzer \emph{et al.} 2014).
#'   \item Create a field (\code{"GEOMTRY_TYPE"}) indicating if areas are
#'     represented as point localities (\code{"POINT"}) or as polygons
#'     (\code{"POLYGON"}).
#'   \item Exclude areas represented as point localities that do not
#'     have a reported spatial extent (i.e. missing data for the field
#      \code{"REP_AREA"}).
#'   \item Reproject data to coordinate system specified in argument to
#'     \code{crs} (using \code{\link{st_parallel_transform}}).
#'   \item Fix any invalid geometries (using
#'     \code{\link{st_parallel_make_valid}}).
#'   \item Buffer areas represented as point localities to circular areas
#'     using their reported spatial extent (using data in the field
#'     \code{"REP_AREA"} and \code{\link[sf]{st_buffer}}).
#'   \item Snap the protected area geometries to a grid to fix any unresolved
#'     geometry issues (using argument to \code{snap_tolerance} and
#'     \code{link[lwgeom]{st_snap_to_grid}}).
#'   \item Fix any invalid geometries (using
#'     \code{\link{st_parallel_make_valid}}).
#' \item Simplify the protected area geometries (using argument to
#'     \code{simplify_tolerance} and \code{link{st_parallel_simplify}}).
#' \item Fix any invalid geometries (using
#'   \code{\link{st_parallel_make_valid}}).
#'   \item Assemble land and exclusive economic zone data (using
#'     \code{\link{land_and_eez_fetch}}).
#'   \item Extract terrestrial protected areas and erase localities that occur
#'    in the ocean.
#'   \item Extract marine protected areas and erase regions that occur on land.
#'   \item Merge terrestrial and marine protected area data sets.
#'   \item The \code{"MARINE"} field is converted from integer codes
#'     to descriptive name (\code{0} = \code{"terrestrial"},
#'     \code{1} = \code{"partial"}, \code{2} = \code{"marine"}).
#'   \item Zeroes in the \code{"STATUS_YR"} field are replaced with \code{NA}
#'     values.
#'   \item Zeroes in the \code{"NO_TK_AREA"} field are replaced with \code{NA}
#'     values for areas for with such data are not reported or applicable
#'     (i.e. areas with the values \code{"Not Applicable"}
#'     or \code{"Not Reported"} in the \code{"NO_TK_AREA"} field).
#'   \item Erase overlapping geometries (discussed in Deguignet \emph{et al.}
#'     2017). Geometries are erased such that protected areas associated with
#'     more effective management categories (\code{"IUCN_CAT"}) or have
#'     historical precdence are retained (using
#'     \code{\link[sf]{st_difference}}).
#'   \item Slivers are removed (geometries an area less than 1e-10 square
#'     kilometers).
#'   \item Calculate size of areas in square kilometers (stored in field
#'     \code{"AREA_KM2"}).
#'  }
#'
#' @return \code{\link[sf]{sf}} object.
#'
#' @seealso \code{\link{wdpa_fetch}},
#'   \url{https://protectedplanet.net/c/calculating-protected-area-coverage}.
#'
#' @references
#' Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP,
#' Harfoot M, ... & Brooks TM (2015) Shortfalls and solutions for
#' meeting national and global conservation area targets.
#' \emph{Conservation Letters}, \strong{8}: 329--337.
#'
#' Coetzer KL, Witkowski ET, & Erasmus BF (2014) Reviewing
#' Biosphere Reserves globally: Effective conservation action or bureaucratic
#' label? \emph{Biological Reviews}, \strong{89}: 82--104.
#'
#' Deguignet M, Arnell A, Juffe-Bignoli D, Shi Y, Bingham H, MacSharry B &
#' Kingston N (2017) Measuring the extent of overlaps in protected area
#' designations. \emph{PloS One}, \strong{12}: e0188681.
#'
#' Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
#' (2015) Protected areas and global conservation of migratory birds.
#' \emph{Science}, \strong{350}: 1255--1258.
#'
#' @examples
#' \donttest{
#' # fetch data for the Marshall Islands
#' mhl_raw_data <- wdpa_fetch("MHL", wait = TRUE)
#'
#' # clean data
#' mhl_data <- wdpa_clean(mhl_raw_data)
#'
#' # plot cleaned data set
#' plot(mhl_data)
#'
#' # plot geometries for visual comparison
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(mhl_raw_data), main = "orginal data", col = "white")
#' plot(sf::st_geometry(mhl_data), main = "cleaned data", col = "white")
#'
#' \dontrun{
#' # fetch data for all protected areas on the planet
#' # note that this might take some time given that the global data set is
#' # over 1 GB in size
#' global_raw_data <- wdpa_fetch("global")
#'
#' # set number of threads for processing
#' n_threads <- max(1, parallel::detectCores(TRUE) - 1)
#'
#' # clean global data set using parallel processing
#' global_data <- wdpa_clean(global_raw_data, threads = n_threads)
#'
#' # plot data
#' plot(global_data)
#' }}
#' @export
wdpa_clean <- function(x, crs = 3395, snap_tolerance = 1,
                       simplify_tolerance = 0, threads = 1,
                       verbose = FALSE) {
  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"),
                          nrow(x) > 0,
                          all(assertthat::has_name(x, c("ISO3", "STATUS",
                                                    "DESIG_ENG", "REP_AREA",
                                                    "MARINE"))),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.scalar(snap_tolerance),
                          isTRUE(snap_tolerance >= 0),
                          assertthat::is.scalar(simplify_tolerance),
                          isTRUE(simplify_tolerance >= 0),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          assertthat::is.flag(verbose),
                          pingr::is_online())
  # clean data
  ## repair geometry
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_make_valid(sf::st_set_precision(x, 1000000), threads)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## remove areas that are not currently in action
  if (verbose) message("removing areas that are not implemented: ",
                       cli::symbol$continue, "\r", appendLF = FALSE)
  x <- x[x$STATUS %in% c("Designated", "Inscribed", "Established"), ]
  if (verbose) {
    utils::flush.console()
    message("removing areas that are not implemented: ", cli::symbol$tick)
  }
  ## remove UNESCO sites
  if (verbose) message("removing UNESCO reserves: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- x[x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve", ]
  if (verbose) {
    utils::flush.console()
    message("removing UNESCO reserves: ", cli::symbol$tick)
  }
  ## assign column indicating geometry type
  is_point <- vapply(sf::st_geometry(x), inherits, logical(1),  "POINT") |
              vapply(sf::st_geometry(x), inherits, logical(1),  "MULTIPOINT")
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## remove protected areas represented as points that do not have
  ## a reported area
  if (verbose) message("removing points with no reported area: ",
                       cli::symbol$continue, "\r", appendLF = FALSE)
  x <- x[!(x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA)), ]
  if (verbose) {
    utils::flush.console()
    message("removing points with no reported area: ", cli::symbol$tick)
  }
  ## reproject data
  if (verbose) message("projecting areas: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_transform(x, crs, threads)
  if (verbose) {
    utils::flush.console()
    message("projecting areas: ", cli::symbol$tick)
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_make_valid(sf::st_set_precision(x, 1000000), threads)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## buffer areas represented as points
  x_points_pos <- which(x$GEOMETRY_TYPE == "POINT")
  if (length(x_points_pos) > 0) {
    if (verbose) message("buffering points: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_points_data <- x[x_points_pos, ]
    x_points_data <- sf::st_buffer(x_points_data,
                       sqrt((x_points_data$REP_AREA * 1000000 / pi)))
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
    if (verbose) {
      utils::flush.console()
      message("buffering points: ", cli::symbol$tick)
    }
  }
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    if (verbose) message("snapping geometry to grid: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
      x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
    if (verbose) {
      utils::flush.console()
      message("snapping geometry to grid: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_make_valid(x, threads)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## simplify geometres
  if (simplify_tolerance > 0) {
    if (verbose) message("simplifying geometry: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
      x <- st_parallel_simplify(x, TRUE, simplify_tolerance)
    if (verbose) {
      utils::flush.console()
      message("simplifying geometry: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_make_valid(x, threads)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## fetch land and eez data
  if (verbose) message("assembling land and eez data: ", cli::symbol$continue,
                       "\r", appendLF = FALSE)
  countries <- unlist(strsplit(x$ISO3, ";"), recursive = TRUE,
                      use.names = FALSE)
  countries <- unique(countries)
  countries <- countries[!is.na(countries)]
  if (length(countries) > 20)
    countries <- "global"
    land_ezz_data <- land_and_eez_fetch(countries, crs = crs,
                                        snap_tolerance = snap_tolerance,
                                        simplify_tolerance = simplify_tolerance,
                                        threads = threads,
                                        verbose = verbose)
    land_pos <- land_ezz_data$TYPE == "LAND"
    land_data <- st_parallel_union(land_ezz_data[land_pos, ], threads = threads)
  if (verbose) {
    utils::flush.console()
    message("assembling land and eez data: ", cli::symbol$tick)
  }
  ## erase terrestrial areas that do not occur on land
  terrestrial_present <- FALSE
  if (any(x$MARINE == "0")) {
    if (verbose) message("erasing terrestrial areas not on land: ",
                         cli::symbol$continue, "\r", appendLF = FALSE)
    terrestrial_present <- TRUE
    x_terrestrial_data <- x[x$MARINE == "0", ]
    x_terrestrial_data <- suppressWarnings(st_parallel_intersection(
                            x_terrestrial_data, land_data, threads = threads))
    if (verbose) {
      utils::flush.console()
      message("erasing terrestrial areas not on land: ", cli::symbol$tick)
    }
  }
  ## erase marine areas that occur on land
  marine_present <- FALSE
  if (any(x$MARINE == "2")) {
    if (verbose) message("erasing marine areas on land: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
    marine_present <- TRUE
    x_marine_data <- x[x$MARINE == "2", ]
    x_marine_data <- suppressWarnings(st_parallel_difference(x_marine_data,
                                                             land_data,
                                                             threads = threads))
    if (verbose) {
      utils::flush.console()
      message("erasing marine areas on land: ", cli::symbol$tick)
    }
  }
  ## combine terrestrial and marine data sets
  if (verbose)  message("merging marine and terrestrial areas: ",
                        cli::symbol$continue, "\r", appendLF = FALSE)
  if (terrestrial_present && marine_present) {
    x <- rbind(x[x$MARINE == "1", ], rbind(x_terrestrial_data,
                                           x_marine_data))
  } else if (terrestrial_present) {
    x <- rbind(x[x$MARINE == "1", ], x_terrestrial_data)
  } else if (marine_present) {
    x <- rbind(x[x$MARINE == "1", ], x_marine_data)
  } else {
    stop("processing removed all data")
  }
  if (verbose) {
    utils::flush.console()
    message("merging marine and terrestrial areas: ", cli::symbol$tick)
  }
  ## format columns
  if (verbose) message("formatting attribute data: ", cli::symbol$continue,
                       "\r", appendLF = FALSE)
  x$MARINE[x$MARINE == "0"] <- "terrestrial"
  x$MARINE[x$MARINE == "1"] <- "partial"
  x$MARINE[x$MARINE == "2"] <- "marine"
  x$STATUS_YR[x$STATUS_YR == 0] <- NA_real_
  x$NO_TK_AREA[x$NO_TAKE %in% c("Not Reported", "Not Applicable")] <- NA_real_
  if (verbose) {
    utils::flush.console()
    message("formatting attribute data: ", cli::symbol$tick)
  }
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    if (verbose) message("snapping geometry to grid: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
    x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
    if (verbose) {
      utils::flush.console()
      message("snapping geometry to grid: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- st_parallel_make_valid(x, threads)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## remove overlaps data
  if (verbose) message("erasing overlaps: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x$IUCN_CAT <- factor(as.character(x$IUCN_CAT),
                       levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                                  "Not Reported", "Not Applicable",
                                  "Not Assigned"))
  x <- st_erase_overlaps(x[order(x$IUCN_CAT, x$STATUS_YR), ])
  x$IUCN_CAT <- as.character(x$IUCN_CAT)
  if (verbose) {
    utils::flush.console()
    message("erasing overlaps: ", cli::symbol$tick)
  }
  ## remove slivers
  if (verbose) message("removing slivers: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- x[!vapply(sf::st_geometry(x), inherits,
                logical(1), "GEOMETRYCOLLECTION"), ]
  x <- suppressWarnings(sf::st_cast(x, "POLYGON"))
  x <- x[as.numeric(sf::st_area(x)) > 0.1, ]
  if (verbose) {
    utils::flush.console()
    message("removing slivers: ", cli::symbol$tick)
  }
  ## calculate area in square kilometers
  if (verbose) message("calulating area: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  areas <- as.numeric(sf::st_area(x)) * 1e+6
  x$AREA_KM2 <- as.numeric(areas)
  if (verbose) {
    utils::flush.console()
    message("calulating area: ", cli::symbol$tick)
  }
  ## move geometry to last column
  x <- x[, c(setdiff(names(x), "geometry"), "geometry")]
  # return cleaned data
  return(x)
}
