#' @include internal.R geo.R
NULL

#' Clean data
#'
#' Clean data from the World Database on Protected Areas (WDPA).
#'
#' @param x \code{\link[sf]{sf}} object containing data from the WDPA.
#'
#' @param crs \code{character} coordinate reference system in PROJ4 format.
#'   Defaults to \code{3395} (Mercator).
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
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
#'   \item Repair any invalid geometry that has since manifested (using
#'     \code{\link{st_parallel_make_valid}}).
#'   \item Buffer areas represented as point localities to circular areas
#'     using their reported spatial extent (using data in the field
#'     \code{"REP_AREA"} and \code{\link[sf]{st_buffer}}).
#'   \item Extract terrestrial protected areas, dissolve them to remove
#'     overlaps (Deguignet \emph{et al.} 2017), and erase regions that occur in
#'     the ocean.
#'   \item Extract marine protected areas, dissolve them to remove overlaps,
#'     and erase regions that occur on land.
#'   \item Merge terrestrial and marine protected area data sets.
#'   \item Intersect the merged data sets with the country and exclusive
#'     economic zone data (\code{\link[sf]{st_intersection}}.
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
#' Harfoot M, ... & Brooks TM (2015). Shortfalls and solutions for
#' meeting national and global conservation area targets.
#' \emph{Conservation Letters}, \strong{8}: 329--337.
#'
#' Coetzer KL, Witkowski ET, & Erasmus BF (2014) Reviewing
#' Biosphere Reserves globally: Effective conservation action or bureaucratic
#' label? \emph{Biological Reviews}, \strong{89}: 82--104.
#'
#' Deguignet M, Arnell A, Juffe-Bignoli D, Shi Y, Bingham H, MacSharry B &
#' Kingston N (2017). Measuring the extent of overlaps in protected area
#' designations. \emph{PloS One}, \strong{12}: e0188681.
#'
#' Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
#' (2015) Protected areas and global conservation of migratory birds.
#' \emph{Science}, \strong{350}: 1255--1258.
#'
#' @examples
#' # read simulated WDPA dataset used for examples and testing
#' fake_data <- wdpa_read(system.file("inst/extdata/WDPA_Nov2017_FAKE.zip",
#'                                    package = "wdpar"))
#'
#' # clean the simulated WDPA dataset
#' cleaned_fake_data <- wdpa_clean(fake_data)
#'
#' # plot the raw and cleaned simulated datasets
#' par(mfrow = c(1, 2))
#' plot(fake_data)
#' plot(cleaned_fake_data)
#' @export
wdpa_clean <- function(x, crs = 3395, threads = 1) {
  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # clean data
  ## repair geometry
  x <- st_parallel_make_valid(x, threads)
  ## remove areas that are not currently in action
  x <- x[x$STATUS %in% c("Designated", "Inscribed", "Established"), ]
  ## remove UNESCO sites
  x <- x[x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve", ]
  ## remove protected areas represented as points that do not have
  ## a reported area
  x <- x[x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA), ]
  ## assign column indicating geometry type
  is_point <- vapply(x$geometry, inherits, logical(1),  "POINT")
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## reproject data
  x <- st_parallel_transform(x, crs, threads)
  ## repair geometry again
  x <- st_parallel_make_valid(x, threads)
  ## buffer areas represented as points
  x_points_pos <- which(x_types == "POINT")
  x_points_data <- x[x_points_pos, ]
  x_points_data <- sf::st_buffer(x_points_data,
                                 sqrt(x_points_data$REP_AREA / pi))
  x_polygon_data$GEOMETRY_TYPE <- x_types[x_polygons_pos]
  x <- sf::rbind(x[which(x_types == "POLYGON"), ], x_points_data)
  ## dissolve data
  x <- sf::union(x)
  ## calculate area in square kilometers
  areas <- units::set_units(sf::st_area(x), km^2)
  x$AREA_KM2 <- as.numeric(areas)
  ## remove slivers
  x <- x[x$AREA_KM2 > 1e-10, ]
  # return cleaned data
  return(x)
}
