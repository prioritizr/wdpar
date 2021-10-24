#' @include internal.R
NULL

#' Dissolve data from the World Database on Protected Areas
#'
#' Create a dataset of spatial boundaries that contains no
#' overlapping geometries.
#'
#' @inheritParams st_erase_overlaps
#' @inheritParams wdpa_clean
#'
#' @details
#' This function is basically a wrapper for [sf::st_union()].
#' It also contains additional parameters to assist with processing
#' large and complex geometry data.
#'
#' @inherit st_erase_overlaps return
#'
#' @seealso [sf::st_union()], [st_erase_overlaps()].
#'
#' @examples
#' # create data
#' pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0, 0), byrow = TRUE,
#'                                   ncol = 2))) * 100
#' pl2 <- sf::st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5),
#'                                   byrow = TRUE, ncol = 2))) * 100
#' pl3 <- sf::st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25),
#'                                   byrow = TRUE, ncol = 2))) * 100
#' x <- sf::st_sf(order = c("A", "B", "C"),
#'                geometry = sf::st_sfc(list(pl1, pl2, pl3), crs = 3395))
#'
#' # dissolve data
#' y <- wdpa_dissolve(x)
#'
#' # plot data for visual comparison
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(x), xlim = c(0, 200), ylim = c(0, 250),
#'      main = "original", col = "transparent")
#' plot(sf::st_geometry(y), , xlim = c(0, 200), ylim = c(0, 250),
#'      main = "dissolved", col = "transparent")
#' @export
wdpa_dissolve <- function(x, geometry_precision = 1500) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.count(geometry_precision),
    assertthat::noNA(geometry_precision)
  )
  # repair geometry
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  # dissolve geometry
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_union(x)
  x <- sf::st_sf(id = 1, geometry = x)
  # return result
  x
}
