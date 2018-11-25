#' @include internal.R
NULL

#' Removes holes
#'
#' Remove holes from polygons or multipolygons from a \code{link[sf]{sf}},
#' \code{\link[sf]{sfc}}, or \code{\link[sf]{st}} object.
#'
#' @param x \code{link[sf]{sf}}, \code{\link[sf]{sfc}}, or \code{\link[sf]{st}}
#'   object.
#'
#' @return Object of the same class as argument to \code{x} with the holes
#'   removed.
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
#'                                                             hole2))),
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
  sf::st_set_geometry(x, st_remove_holes.sfc(sf::st_geometry(x)))
}

#' @export
st_remove_holes.sfc <- function(x) {
  for (i in seq_along(x))
    x[[i]] <- st_remove_holes.sfg(x[[i]])
  return(sf::st_sfc(x))
}

#' @export
st_remove_holes.sfg <- function(x) {
  if (inherits(x, "POLYGON")) {
    x <- sf::st_polygon(x[1])
  } else if (inherits(x, "MULTIPOLYGON")) {
    x <- lapply(x, `[`, 1)
    x <- lapply(x, sf::st_polygon)
    x <- lwgeom::st_make_valid(sf::st_sfc(x))
    x <- sf::st_union(x)[[1]]
  }
  return(x)
}

#' Make valid polygon data
#'
#' Force a \code{link[sf]{sf}} or \code{\link[sf]{sfc}} object to contain
#' valid polygon geometries.
#'
#' @param x \code{link[sf]{sf}} or \code{\link[sf]{sfc}} object.
#'
#' @details This function will any polygon geometries contained within
#'  \code{\link[sf]{st_geometrycollection}} objects and union them togeather.
#'  Any \code{\link[sf]{st_geometrycollection}} objects that do not contain
#'  polygon geometries will be discarded.
#'
#' @return object of same class as argument to \code{x}.
#'
#' @examples
#' # make data containing points, lines, and a geometry collection
#' ## point
#' po <- sf::st_point(c(1, 2))
#'
#' ## line
#' lin <- sf::st_linestring(matrix(1:8, , 2))
#'
#' ## polygon
#' outer <- matrix(c(0, 0, 8, 0, 8, 8, 0, 8, 0, 0), ncol = 2, byrow = TRUE)
#' hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
#' hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
#' pl1 <- sf::st_polygon(list(outer))
#' pl2 <- sf::st_polygon(list(outer + 20, hole1 + 20, hole2 + 20))
#'
#' ## multi-polygon
#' mpl <- sf::st_multipolygon(list(list(outer + 30, hole1 + 30, hole2 + 30),
#'                                 list(outer + 40, hole1 + 40),
#'                                 list(outer + 50)))
#'
#' ## geometry collections
#' geomcol1 <- sf::st_geometrycollection(list(pl1 + 60, po + 60, pl2 + 60,
#'                                            lin + 60, mpl + 60))
#' geomcol2 <- sf::st_geometrycollection(list(po + 70, lin + 70))
#' geomcol3 <- sf::st_geometrycollection(list(pl1 + 80))
#' geomcol4 <- sf::st_geometrycollection()
#'
#' # create sf object
#' x <- sf::st_sf(label = letters[1:9],
#'                geometry = sf::st_sfc(po, pl1, lin, pl2, mpl, geomcol1,
#'                                      geomcol2, geomcol3, geomcol4))
#'
#' # subset polygons
#' y <- st_make_valid_polygons(x)
#'
#' # print the data for visual comparisons
#' print(x)
#' print(y)
#' @export
st_make_valid_polygons <- function(x) UseMethod("st_make_valid_polygons")

#' @export
st_make_valid_polygons.sf <- function(x) {
  g <- st_make_valid_polygons(sf::st_geometry(x))
  x <- x[attr(g, "idx"), ]
  x <- sf::st_set_geometry(x, g)
  return(x)
}

#' @export
st_make_valid_polygons.sfc <- function(x) {
  # find geometry collections
  pos <- which(vapply(x, inherits, logical(1), "GEOMETRYCOLLECTION"))
  for (i in pos)
    x[[i]] <- sf::st_union(sf::st_collection_extract(x[[i]], "POLYGON"))[[1]]
  # find non-polygon objects
  pos <- which(vapply(x, inherits, logical(1), c("POLYGON", "MULTIPOLYGON")))
  # return only polygon objects
  x <- x[pos]
  attr(x, "idx") <- pos
  x
}

#' Erase overlaps
#'
#' Erase overlapping geometries in a \code{\link[sf]{sf}} object.
#'
#' @param x code{\link[sf]{sf}} object.
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
#'                geometry = sf::st_sfc(list(pl1, pl2, pl3), crs = 3395))
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
      d <- suppressWarnings(sf::st_difference(g[i],
             sf::st_collection_extract(lwgeom::st_make_valid(sf::st_buffer(sf::st_union(o[ovr]), 0)), "POLYGON")))
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

#' Extract holes
#'
#' Extract holes from polygons or multipolygons from a \code{link[sf]{sf}},
#' \code{\link[sf]{sfc}}, or \code{\link[sf]{st}} object.
#'
#' @param x \code{link[sf]{sf}}, \code{\link[sf]{sfc}}, or \code{\link[sf]{st}}
#'   object.
#'
#' @return Object of the same class as argument to \code{x} with the holes
#'   represented as geometries.
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
#'                                                             hole2))),
#'                 crs = 4326)
#' # extract holes
#' y <- st_extract_holes(x)
#'
#' # plot geometries for visual comparison
#' par(mfrow = c(1, 2))
#' plot(x, main = "original")
#' plot(y, main = "holes")
#' @export
st_extract_holes <- function(x) UseMethod("st_extract_holes")

#' @export
st_extract_holes.sf <- function(x) {
  sf::st_set_geometry(x, st_extract_holes.sfc(sf::st_geometry(x)))
}

#' @export
st_extract_holes.sfc <- function(x) {
  out <- rcpp_st_extract_holes(x)
  nulls <- !vapply(out, inherits, logical(1), "XY")
  out[nulls] <- x[nulls]
  out <- sf::st_sfc(out)
  if (!is.null(attr(x, "precision")))
    attr(out, "precision") <- attr(x, "precision")
  if (!is.null(attr(x, "crs")))
    attr(out, "crs") <- attr(x, "crs")
  out
}

#' @export
st_extract_holes.sfg <- function(x) {
  st_extract_holes.sfc(sf::st_sfc(x))[[1]]
}
