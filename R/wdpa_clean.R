#' @include internal.R geo.R
NULL

#' Clean data
#'
#' Clean data from the World Database on Protected Areas (WDPA).
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
  ## assign column indicating geometry type
  is_point <- vapply(x$geometry, inherits, logical(1),  "POINT")
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## remove invalid areas
  x <- x[(x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve") &
         (x$STATUS %in% c("Designated", "Inscribed", "Established")), ]
  ## remove protected areas represented as points that do not have
  ## a reported area
  x <- x[x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA), ]
  ## reproject data
  x <- st_parallel_transform(x, crs, threads)
  ## repair geometry again
  x <- st_parallel_make_valid(x, threads)
  ## buffer areas represented as points
  x_points_pos <- which(x_types == "POINT")
  x_points_data <- x[x_points_pos, ]
  x_points_data <- sf::st_buffer(x_points_data,
                                 sqrt(x_points_data$REP_AREA / pi))
  x_polygon_data$REPR_TYPE <- x_types[x_polygons_pos]
  x <- sf::rbind(x[which(x_types == "POLYGON"), ], x_points_data)
  ## dissolve data
  x <- sf::unary_union(x)
  ## calculate area in square kilometers
  areas <- units::set_units(sf::st_area(x), km^2)
  x$AREA_KM2 <- as.numeric(areas)
  ## remove slivers
  x <- x[x$AREA_KM2 > 1e-10, ]
  # return cleaned data
  return(x)
}
