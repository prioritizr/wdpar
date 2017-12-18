#' @include internal.R
NULL

#' Clean data
#'
#' Clean data from the World Database on Protected Areas (WDPA) following best
#' practices (outlined in Runge \emph{et al.} 2015).
#'
#' @param x \code{\link[sf]{sf}} object containing data from the WDPA.
#'
#' @param crs \code{character} coordinate reference system in PROJ4 format.
#'   Defaults to \code{sf::st_crs(3395)} (Mercator).
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
#'
#' @details This function cleans the data by performing the
#'
#' @return \code{\link[sf]{sf}} object.
#'
#' @seealso \code{\link{wdpa_fetch}}.
#'
#' @references
#'
#' @examples
#'
#' @export
wdpa_clean <- function(x, crs = sf::st_crs(3395), threads = 1) {
  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"), inherits(crs, "crs"),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # clean data
  ## repair geometry
  x <- st_parallel_make_valid(x, threads)
  ## remove invalid areas
  x <- x[(x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve") &
         (!x$STATUS %in% c("Proposed", "Not Reported")), ]
  ## reproject data
  x <- st_parallel_transform(x, crs, threads)
  ## repair geometry again
  x <- st_parallel_make_valid(x, threads)
  ## buffer areas represented as points
  x_types <-
  x_polygons_pos <- which(x_types != "POINT")
  x_points_pos <- which(x_types == "POINT")
  x_points_data <- x[x_points_pos, ]
  x_points_data <- x_points_data[is.finite(x_points_data$REP_AREA)]
  x_points_data <- sf::st_transform(x_points_data, crs)
  x_points_data <- sf::st_buffer(x_points_data,
                                 sqrt(x_points_data$REP_AREA / pi))
  x_points_data$REPR_TYPE <- "POINT"
  x_polygon_data <- x[x_polygons_pos, ]
  x_polygon_data$REPR_TYPE <- x_types[x_polygons_pos]
  x <- sf::rbind(x_polygon_data, x_points_data)
  ## erase overlapping areas
  x <- 
  ## calculate area in square kilometers
  areas <- units::set_units(sf::st_area(x), km^2)
  x$AREA_KM2 <- as.numeric(areas)
  ## remove slivers
  x <- x[x$AREA_KM2 > 1e-10, ]
  # return cleaned data
  return(x)
}
