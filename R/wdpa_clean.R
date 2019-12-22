#' @include internal.R geo.R
NULL

#' Clean data from the World Database on Protected Areas
#'
#' Clean data obtained from the World Database on Protected Areas (WDPA).
#'
#' @param x \code{\link[sf]{sf}} object containing protected area data.
#'
#' @param crs \code{character} or code{integer} object representing a
#'   coordinate reference system. Defaults to World Behrmann
#'  (\emph{ESRI:54017}).
#'
#' @param snap_tolerance \code{numeric} tolerance for snapping geometry to a
#'   grid for resolving invalid geometries. Defaults to 1 meter.
#'
#' @param simplify_tolerance \code{numeric} simplification tolerance.
#'   Defaults to 0 meters.
#'
#' @param geometry_precision \code{numeric} level of precision for processing
#'   the spatial data (used with \code{\link[sf]{st_set_precision}}). The
#'   default argument corresponds to the nearest millimeter (i.e. 1000).
#'
#' @param erase_overlaps \code{logical} should overlapping boundaries be removed
#'   erased? This can be useful when the protected area boundaries are
#'   going to be rasterized, and so processing time can be substantially
#'   reduced by skipping this step because overlapping boundaries will not be a
#'   problem. Defaults to \code{TRUE}.
#'
#' @param verbose \code{logical} should progress on data cleaning be reported?
#'   Defaults to \code{TRUE} in an interactive session, otherwise
#'   \code{FALSE}.
#'
#' @details This function cleans data from World Database on Protected Areas
#'   following best practices (Butchart \emph{et al.} 2015, Runge \emph{et al.}
#'   2015,
#'   \url{https://protectedplanet.net/c/calculating-protected-area-coverage}).
#'   To obtain accurate protected area coverage statistics for a country,
#'   please note that you will need to manually clip the cleaned data to
#'   the countries' coastline and its Exclusive Economic Zone (EEZ).
#'   Although this function can \emph{in theory} be used to clean the global
#'   dataset, this process can take several weeks to complete. Therefore, it is
#'   strongly recommended to use alternative methods for cleaning the global
#'   dataset.
#'
#'   \enumerate{
#'
#'   \item Repair invalid geometry (using \code{\link[lwgeom]{st_make_valid}}).
#'
#'   \item Exclude protected areas that are not currently implemented
#'     (i.e. exclude areas without the status \code{"Designated"},
#'     \code{"Inscribed"}, \code{"Established"}).
#'
#'   \item Exclude United Nations Educational, Scientific and Cultural
#'     Organization (UNESCO) Biosphere Reserves (Coetzer \emph{et al.} 2014).
#'
#'   \item Create a field (\code{"GEOMETRY_TYPE"}) indicating if areas are
#'     represented as point localities (\code{"POINT"}) or as polygons
#'     (\code{"POLYGON"}).
#'
#'   \item Exclude areas represented as point localities that do not
#'     have a reported spatial extent (i.e. missing data for the field
#      \code{"REP_AREA"}).
#'
#'   \item Geometries are wrapped to the dateline (using
#'     \code{\link[sf]{st_wrap_dateline}} with the options
#'     \code{"WRAPDATELINE=YES"} and \code{"DATELINEOFFSET=180"}).
#'
#'   \item Reproject data to coordinate system specified in argument to
#'     \code{crs} (using \code{\link[sf]{st_transform}}).
#'
#'   \item Fix any invalid geometries that have manifested (using
#'     (using \code{\link[lwgeom]{st_make_valid}}).
#'
#'   \item Buffer areas represented as point localities to circular areas
#'     using their reported spatial extent (using data in the field
#'     \code{"REP_AREA"} and \code{\link[sf]{st_buffer}}; see Visconti
#'     \emph{et al.} 2013).
#'
#'   \item Snap the geometries to a grid to fix any remaining
#'     geometry issues (using argument to \code{snap_tolerance} and
#'     \code{\link[lwgeom]{st_snap_to_grid}}).
#'
#'   \item Fix any invalid geometries that have manifested (using
#'     (using \code{\link[lwgeom]{st_make_valid}}).
#'
#'   \item Simplify the protected area geometries to reduce computational burden
#'     (using argument to \code{simplify_tolerance} and
#'     \code{\link[sf]{st_simplify}}).
#'
#'   \item Fix any invalid geometries that have manifested (using
#'     (using \code{\link[lwgeom]{st_make_valid}}).
#'
#'   \item The \code{"MARINE"} field is converted from integer codes
#'     to descriptive names (i.e. \code{0} = \code{"terrestrial"},
#'     \code{1} = \code{"partial"}, \code{2} = \code{"marine"}).
#'
#'   \item Zeros in the \code{"STATUS_YR"} field are replaced with
#'     missing values (i.e. \code{NA_real_} values).
#'
#'   \item Zeros in the \code{"NO_TK_AREA"} field are replaced with \code{NA}
#'     values for areas where such data are not reported or applicable
#'     (i.e. areas with the values \code{"Not Applicable"}
#'     or \code{"Not Reported"} in the \code{"NO_TK_AREA"} field).
#'
#'   \item Overlapping geometries are erased from the protected area data
#'     (discussed in Deguignet \emph{et al.} 2017). Geometries are erased such
#'     that areas associated with more effective management
#'     categories (\code{"IUCN_CAT"}) or have historical precedence are retained
#'     (using \code{\link[sf]{st_difference}}).
#'
#'   \item Slivers are removed (geometries with areas less than 0.1 square
#'     meters).
#'
#'   \item The size of areas are calculated in square kilometers and stored in
#'     the field \code{"AREA_KM2"}.
#'
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
#' Visconti P, Di Marco M, Alvarez-Romero JG, Januchowski-Hartley SR, Pressey,
#' RL, Weeks R & Rondinini C (2013) Effects of errors and gaps in spatial data
#' sets on assessment of conservation progress. \emph{Conservation Biology},
#' \strong{27}: 1000--1010.
#'
#' @examples
#' \donttest{
#' # fetch data for the Liechtenstein
#' lie_raw_data <- wdpa_fetch("LIE", wait = TRUE)
#'
#' # clean data
#' lie_data <- wdpa_clean(lie_raw_data)
#'
#' # plot cleaned dataset
#' plot(lie_data)
#' }
#' @export
wdpa_clean <- function(x,
                       crs = paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0",
                       "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"),
                       snap_tolerance = 1,
                       simplify_tolerance = 0, geometry_precision = 1000,
                       erase_overlaps = TRUE,
                       verbose = interactive()) {
  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"),
                          nrow(x) > 0,
                          all(assertthat::has_name(x, c("ISO3", "STATUS",
                                                        "DESIG_ENG", "REP_AREA",
                                                        "MARINE"))),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.number(snap_tolerance),
                          isTRUE(snap_tolerance >= 0),
                          assertthat::is.number(simplify_tolerance),
                          isTRUE(simplify_tolerance >= 0),
                          assertthat::is.count(geometry_precision),
                          assertthat::is.flag(erase_overlaps),
                          assertthat::is.flag(verbose),
                          curl::has_internet())
  # check that x is in wgs1984
  assertthat::assert_that(sf::st_crs(x) == sf::st_crs(4326),
   msg = "argument to x is not longitude/latitude (i.e. EPSG:4326)")
  # clean data
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
  is_point <- vapply(sf::st_geometry(x), inherits, logical(1),
                     c("POINT", "MULTIPOINT"))
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
  ## repair geometry
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- rbind(suppressWarnings(sf::st_collection_extract(x, "POLYGON")),
             x[vapply(sf::st_geometry(x), inherits, logical(1),
                      c("POINT", "MULTIPOINT")), drop = FALSE])
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## wrap dateline issues
  if (verbose) message("wrapping dateline: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- suppressWarnings(sf::st_wrap_dateline(x,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")))
  x <- x[!sf::st_is_empty(x), ]
  x <- rbind(suppressWarnings(sf::st_collection_extract(x, "POLYGON")),
             x[vapply(sf::st_geometry(x), inherits, logical(1),
                      c("POINT", "MULTIPOINT")), drop = FALSE])
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("wrapping dateline: ", cli::symbol$tick)
  }
  ## repair geometry
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- rbind(suppressWarnings(sf::st_collection_extract(x, "POLYGON")),
             x[vapply(sf::st_geometry(x), inherits, logical(1),
                      c("POINT", "MULTIPOINT")), drop = FALSE])
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## reproject data
  if (verbose) message("projecting areas: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_transform(x, crs)
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("projecting areas: ", cli::symbol$tick)
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- rbind(suppressWarnings(sf::st_collection_extract(x, "POLYGON")),
             x[vapply(sf::st_geometry(x), inherits, logical(1),
                      c("POINT", "MULTIPOINT")), drop = FALSE])
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## buffer polygons by zero to fix any remaining issues
  x_polygons_pos <- which(x$GEOMETRY_TYPE == "POLYGON")
  if (length(x_polygons_pos) > 0) {
    if (verbose) message("buffering by zero: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_polygons_data <- x[x_polygons_pos, ]
    x_polygons_data <- sf::st_set_precision(x_polygons_data, geometry_precision)
    x_polygons_data <- sf::st_buffer(x_polygons_data, 0)
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POINT"), ], x_polygons_data)
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("buffering by zero: ", cli::symbol$tick)
    }
  }
  ## buffer areas represented as points
  x_points_pos <- which(x$GEOMETRY_TYPE == "POINT")
  if (length(x_points_pos) > 0) {
    if (verbose) message("buffering points: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_points_data <- x[x_points_pos, ]
    x_points_data <- sf::st_buffer(x_points_data,
                       sqrt((x_points_data$REP_AREA * 1e6) / pi))
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("buffering points: ", cli::symbol$tick)
    }
  }
  ## simplify geometries
  if (simplify_tolerance > 0) {
    if (verbose) message("simplifying geometry: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
      x <- sf::st_set_precision(x, geometry_precision)
      x <- sf::st_simplify(x, TRUE, simplify_tolerance)
      x <- sf::st_set_precision(x, geometry_precision)
      x <- x[!sf::st_is_empty(x), ]
      x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
      x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("simplifying geometry: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    if (verbose) message("snapping geometry to grid: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
      x <- sf::st_set_precision(x, geometry_precision)
      x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
      x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("snapping geometry to grid: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
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
  ## remove overlaps data
  if (erase_overlaps) {
    if (verbose) message("erasing overlaps: ", cli::symbol$continue)
    x$IUCN_CAT <- factor(as.character(x$IUCN_CAT),
                         levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                                    "Not Reported", "Not Applicable",
                                    "Not Assigned"))
    x <- sf::st_set_precision(x, geometry_precision)
    x <- st_erase_overlaps(x[order(x$IUCN_CAT, x$STATUS_YR), ], verbose)
    x$IUCN_CAT <- as.character(x$IUCN_CAT)
    x <- x[!sf::st_is_empty(x), ]
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) message("erasing overlaps: ", cli::symbol$tick)
  }
  ## remove slivers
  if (verbose) message("removing slivers: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- x[as.numeric(sf::st_area(x)) > 0.1, ]
  if (verbose) {
    utils::flush.console()
    message("removing slivers: ", cli::symbol$tick)
  }
  ## calculate area in square kilometers
  if (verbose) message("calulating area: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  areas <- as.numeric(sf::st_area(x)) * 1e-6
  x$AREA_KM2 <- as.numeric(areas)
  if (verbose) {
    utils::flush.console()
    message("calculating area: ", cli::symbol$tick)
  }
  ## move geometry to last column
  if ((!"geometry" %in% names(x))) {
    geom_col <- attr(x, "sf_column")
    attr(x, "sf_column") <- "geometry"
    names(x)[names(x) == geom_col] <- "geometry"
  }
  x <- x[, c(setdiff(names(x), "geometry"), "geometry")]
  # return cleaned data
  return(x)
}
