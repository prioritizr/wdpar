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
#' @param threads \code{integer} number of threads for processing data. Defaults
#'   to 1.
#'
#' @details This function is essentially just a wrapper for
#'   \code{\link[sf]{st_transform}}. See the documentation for
#'   \code{\link[sf]{st_transform}} for more information.
#'
#' @return Object with reprojected coordinates.
#'
#' @seealso \code{\link[sf]{st_transform}}.
#'
#' @examples
#' # create data
#' pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
#'                                    byrow = TRUE, ncol = 2)))
#' pl2 <- sf::st_polygon(list(matrix(c(5, 5, 7, 5, 7, 7, 5, 7, 5, 5),
#'                                    byrow = TRUE, ncol = 2)))
#' x <- sf::st_sf(z = runif(2), geometry = sf::st_sfc(list(pl1, pl2),
#'                                                    crs = 4326))
#'
#' # transform data using sf::st_transform
#' y1 <- sf::st_transform(x, 3395)
#'
#' # transform data using parallelized implementation
#' y2 <- st_parallel_transform(x, 3395, threads = 2)
#'
#' # plot objects for visual comparison
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(y1), main = "sf::st_transform", axes = TRUE)
#' plot(sf::st_geometry(y2), main = "st_parallel_transform", axes = TRUE)
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
#' @param threads \code{integer} number of threads for processing data. Defaults
#'   to 1.
#'
#' @details This function is essentially just a wrapper for
#'   \code{\link[lwgeom]{st_make_valid}}. See the documentation for
#'   \code{\link[lwgeom]{st_make_valid}} for more information.
#'
#' @return Object with fixed geometry.
#'
#' @seealso \code{\link[lwgeom]{st_make_valid}}.
#'
#' @examples
#' # create data
#'  x <- sf::st_sf(z = 1, geomtry = sf::st_sfc(sf::st_polygon(list(
#'         rbind(c(0, 0), c(0.5, 0), c(0.5, 0.5), c(0.5, 0), c(1, 0),
#'               c(1, 1), c(0, 1), c(0, 0))))), agr = "constant")
#'
#' # repair geometry using lwgeom::st_make_valid
#' y1 <- lwgeom::st_make_valid(x)
#'
#' # repair geometry using parallelized implementation
#' y2 <- st_parallel_make_valid(x, threads = 2)
#'
#' # plot objects for visual comparison
#' par(mfrow = c(1, 2))
#' plot(y1, main = "lwgeom::st_make_valid", axes = TRUE)
#' plot(y2, main = "st_parallel_make_valid", axes = TRUE)
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

#' Simplify geometry (parallelized)
#'
#' Simplify the geometry in a simple feature data set using parallel processing.
#'
#' @param x Object of class \code{sfc} or \code{sf}.
#'
#' @param preserveTopology \code{logical} carry out topology preserving
#'   simplification? Defaults to \code{FALSE}.
#'
#' @param dTolerance \code{numeric} tolerance parameter affecting how much
#'   the geometry is simplified.
#'
#' @param threads \code{integer} number of threads for processing data. Defaults
#'   to 1.
#'
#' @details This function is a wrapper for \code{\link[sf]{st_simplify}}.
#'
#' @return Simplified object.
#'
#' @seealso \code{\link[sf]{st_simplify}}.
#'
#' @examples
#' # load data
#' nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf")) %>%
#'       sf::st_transform(3395)
#'
#' # simplify data to 1 km
#' nc_simplified <- sf::st_simplify(nc, dTolerance = 5000)
#'
#' # plot data for visual comparisons
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(nc), col = "white")
#' plot(sf::st_geometry(nc_simplified), col = "white")
#' @export
st_parallel_simplify <- function(x, preserveTopology = FALSE, dTolerance = 0,
                                 threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc")),
                          assertthat::is.flag(preserveTopology),
                          assertthat::is.scalar(dTolerance),
                          is.finite(dTolerance),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  # process data
  parallel_sf_operation(x, sf::st_simplify,
                        args = list(preserveTopology = preserveTopology,
                                    dTolerance = dTolerance),
                        remove_empty = TRUE,
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
#' @param threads \code{integer} number of threads for processing data. Defaults
#'   to 1.
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
#' @examples
#' # create x object
#' pl1 <- sf::st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1),
#'                                  c(-1, -1))))
#'  pl2 <- pl1 + 2
#'  pl3 <- pl1 + c(-0.2, 2)
#'  x <- sf::st_sf(v = letters[1:3], geometry = sf::st_sfc(pl1, pl2, pl3),
#'                                                         crs = 3395)
#'
#' # create z object
#'  pl4 <- pl1 * 0.8
#'  pl5 <- pl4 * 0.5 + c(2, 0.7)
#'  pl6 <- pl4 + 1
#'  pl7 <- pl1 * 0.5 + c(2, -0.5)
#'  z <-  sf::st_sf(v = letters[4:7], geometry = sf::st_sfc(pl4, pl5, pl6, pl7),
#'                  crs = 3395)
#'
#' # calculate difference using sf::st_difference
#' y1 <- sf::st_difference(x, z)
#'
#' # calculate difference using parallelized implementation
#' y2 <- st_parallel_difference(x, z, threads = 2)
#'
#' # calculate intersection using sf::st_intersection
#' y3 <- sf::st_intersection(x, z)
#'
#' # calculate intersection using parallelized implementation
#' y4 <- st_parallel_intersection(x, z, threads = 2)
#'
#' # calculate symmetric difference using sf::st_sym_difference
#' y5 <- sf::st_sym_difference(x, z)
#'
#' # calculate symmetric difference using parallelized implementation
#' y6 <- st_parallel_sym_difference(x, z, threads = 2)
#'
#' # plot objects for visual comparison
#' par(mfrow = c(4, 2))
#' plot(st_geometry(x), main = "x", axes = TRUE, col = "lightblue")
#' plot(st_geometry(z), main = "z", axes = TRUE, col = "lightblue")
#' plot(st_geometry(y1), main = "sf::st_difference", axes = TRUE,
#'      col = "lightblue")
#' plot(st_geometry(y2), main = "st_parallel_difference", axes = TRUE,
#'      col = "lightblue")
#' plot(st_geometry(y3), main = "sf::st_intersection", axes = TRUE,
#'      col = "lightblue")
#' plot(st_geometry(y4), main = "st_parallel_intersection", axes = TRUE,
#'      col = "lightblue")
#' plot(st_geometry(y5), main = "sf::st_sym_difference", axes = TRUE,
#'      col = "lightblue")
#' plot(st_geometry(y6), main = "st_parallel_sym_difference", axes = TRUE,
#'      col = "lightblue")
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
#' @examples
#' # create data
#' outer1 <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
#' hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
#' hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
#' outer2 <- matrix(c(20, 20, 30, 25, 30, 30, 25, 30, 20, 20), ncol = 2,
#'                  byrow = TRUE)
#' x <- sf::st_sfc(sf::st_polygon(list(outer1, hole1, hole2)),
#'                 sf::st_multipolygon(list(list(outer2), list(outer1, hole1,
#'                                                              hole2))),
#'                 crs = 4326)
#'
#' # remove holes
#' y <- st_remove_holes(x)
#'
#' # plot geometries for visual comparison
#' par(mfrow = c(1, 2))
#' plot(x, main = "original")
#' plot(y, main = "holes removed")
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

#' Erase overlaps
#'
#' Erase overlapping geometries.
#'
#' @param x \code{sf} object.
#'
#' @details This is a more robust---albeit slower---implementation for
#'   \code{\link{st_difference}} when \code{y} is missing.
#'
#' @return code{sf} object.
#'
#' @seealso \code{\link{st_difference}}.
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
#'                geometry = st_sfc(list(pl1, pl2, pl3), crs = 3395))
#'
#' # erase overlaps
#' y <- st_erase_overlaps(x)
#'
#' # plot data for visual comparison
#' par(mfrow = c(1, 2))
#' plot(sf::st_geometry(x), main = "original", col = "white")
#' plot(sf::st_geometry(y), main = "no overlaps", col = "white")
#' @export
st_erase_overlaps <- function(x) {
  # validate arguments
  assertthat::assert_that(inherits(x, "sf"))
  # processing
  g <- sf::st_geometry(x)
  o <- g[1]
  for (i in seq(2, length(g))) {
    # find overlapping geometries
    ovr <- sf::st_overlaps(g[i], o)[[1]]
    # if overlapping geometries then calculate difference
    if (length(ovr) > 0) {
      # calculate difference
      d <- sf::st_difference(g[i], sf::st_union(o[ovr]))
    } else {
      d <- g[i]
    }
    # find empty geometries
    empty <- vapply(d, inherits, logical(1), "GEOMETRYCOLLECTION")
    # process geometry if its not empty
    if (!all(empty)) {
      # remove slivers (areas less then 1 m^2)
      d <- d[!empty]
      d <- sf::st_cast(d, "POLYGON")
      d <- d[as.numeric(sf::st_area(d)) > 1]
      d <- sf::st_cast(d, "MULTIPOLYGON")
      if (length(d) == 0)
        d <- list(sf::st_geometrycollection(list()))
    } else {
      d <- list(sf::st_geometrycollection(list()))
    }
    # store geometry
    o[i] <- d[[1]]
  }
  x <- sf::st_set_geometry(x, o)
  # return output
  return(x)
}
