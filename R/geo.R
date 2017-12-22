#' @include internal.R
NULL

#' Transform or convert coordinates of simple feature (parallelized)
#'
#' Reproject spatial data using parallelized computations.
#'
#' @param x object of class \code{sfc} or \code{sf}.
#'
#' @param crs coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @param ... not used.
#'
#' @param threads \code{integer} number of cores for processing data.
#'
#' @details This function is essentially just a wrapper for
#'   \code{\link[sf]{st_transform}}. See the documentation for
#'   \code{\link[sf]{st_transform}} for more information.
#'
#' @return Object with reprojected coordinates.
#'
#' @seealso \code{\link[sf]{st_transform}}.
#'
#' @export
st_parallel_transform <- function(x, crs, ..., threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # process data
  parallel_sf_operation(x, sf::st_transform, args = list(crs = crs),
                        threads = threads)
}

#' Make an invalid geometry valid (parallelized)
#'
#' Fix geometry issues using parallelized computations.
#'
#' @param x object of class \code{sfc} or \code{sf}.
#'
#' @param threads \code{integer} number of cores for processing data.
#'
#' @details This function is essentially just a wrapper for
#'   \code{\link[lwgeom]{st_make_valid}}. See the documentation for
#'   \code{\link[lwgeom]{st_make_valid}} for more information.
#'
#' @return Object with fixed geometry.
#'
#' @seealso \code{\link[lwgeom]{st_make_valid}}.
#'
#' @export
st_parallel_make_valid <- function(x, threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  # process data
  parallel_sf_operation(x, lwgeom::st_make_valid, remove_empty = TRUE,
                        threads = threads)
}

#' Geometric operations on pairs of simple feature geometry sets (parallelized)
#'
#' Perform geometric set operations with simple feature
#' collections using parallel processing.
#'
#' @param x object of class \code{sfc} or \code{sf}.
#'
#' @param y object of class \code{sfc} or \code{sf}.
#'
#' @param threads \code{integer} number of cores for processing data.
#'
#' @details These functions are essentially just a wrapper for
#'   \code{\link[sf]{st_difference}}, \code{\link[sf]{st_intersection}},
#'   \code{\link[sf]{st_sym_difference}}.
#'
#' @return The intersection, difference or symmetric difference between two
#'   sets of geometries.
#'
#' @seealso \code{\link[sf]{st_difference}}, \code{\link[sf]{st_intersection}},
#'   \code{\link[sf]{st_sym_difference}}.
#'
#' @name geometric_set_operations
NULL

#' @rdname geometric_set_operations
#'
#' @export
st_parallel_difference <- function(x, y, threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          inherits(y, c("sf", "sfc")),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  # process data
  parallel_sf_operation(x, sf::st_difference, args = list(y = y),
                        threads = threads)
}

#' @rdname geometric_set_operations
#'
#' @export
st_parallel_intersection <- function(x, y, threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          inherits(y, c("sf", "sfc")),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  # process data
  parallel_sf_operation(x, sf::st_intersection, args = list(y = y),
                        threads = threads)
}

#' @rdname geometric_set_operations
#'
#' @export
st_parallel_sym_difference <- function(x, y, threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          inherits(y, c("sf", "sfc")),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  # process data
  parallel_sf_operation(x, sf::st_sym_difference, args = list(y = y),
                        threads = threads)
}

#' Removes holes
#'
#' Remove holes from polygons or multipolygons.
#'
#' @param x object of class \code{sfc}, \code{sfg} or \code{sf}.
#'
#' @return Object with holes removed.
#'
#' @export
st_remove_holes <- function(x) UseMethod("st_remove_holes")

#' @export
st_remove_holes.sf <- function(x) {
  x[[attr(x, "sf_column")]] <- st_remove_holes.sfc(x[[attr(x, "sf_column")]])
  return(x)
}

#' @export
st_remove_holes.sfc <- function(x) {
  for (i in seq_along(x))
    x[[i]] <- st_remove_holes(x[[i]])
  return(x)
}

#' @export
st_remove_holes.sfg <- function(x) {
  x_attr <- attributes(x)
  if (inherits(x, "POLYGON")) {
    x <- x[1]
  } else if (inherits(x, "MULTIPOLYGON")) {
    for (i in seq_along(x)) {
      x[[i]] <- x[[i]][1]
    }
  }
  attributes(x) <- x_attr
  return(x)
}
