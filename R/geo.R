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
    x[[i]] <- st_remove_holes(x[[i]])
  return(x)
}

#' @export
st_remove_holes.sfg <- function(x) {
  if (inherits(x, "POLYGON")) {
    x <- sf::st_polygon(x[1])
  } else if (inherits(x, "MULTIPOLYGON")) {
    x <- sf::st_sfc(lapply(x, function(y) sf::st_polygon(y[1])))
    x <- sf::st_union(x)[[1]]
  }
  return(x)
}

#' Subset polygons
#'
#' Subset polygons from a \code{link[sf]{sf}} or \code{\link[sf]{sfc}} object.
#'
#' @param x \code{link[sf]{sf}} or \code{\link[sf]{sfc}} object.
#'
#' @details This function will extract any polygon or geometries, including
#'  those that are contained within \code{\link[sf]{st_geometrycollection}}
#'  objects.
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
#' y <- st_subset_polygons(x)
#'
#' # print the data for visual comparisons
#' print(x)
#' print(y)
#' @export
st_subset_polygons <- function(x) UseMethod("st_subset_polygons")

#' @export
st_subset_polygons.sf <- function(x) {
  g <- st_subset_polygons(sf::st_geometry(x))
  x <- x[attr(g, "idx"), ]
  x <- sf::st_set_geometry(x, g)
  return(x)
}

#' @export
st_subset_polygons.sfc <- function(x) {
  # define function to recursively extract polygons
  extract_data <- function(p) {
    if (inherits(p, "POLYGON"))
      return(list(p))
    if (inherits(p, "MULTIPOLYGON"))
      return(lapply(p, sf::st_polygon))
    if (!inherits(p, "GEOMETRYCOLLECTION"))
      return(list(sf::st_geometrycollection()))
    return(lapply(p, extract_data))
  }
  # define function to flatten nested list to required classes
  flatten_list <- function(x, classes) {
    x <- list(x)
    repeat {
        x <- Reduce(c, x)
        if (all(vapply(x, inherits, logical(1), classes))) return(x)
    }
  }
  # store crs
  crs <- sf::st_crs(x)
  # extract polygons and multi-polygons
  x <- lapply(x, extract_data)
  # convert multiply-nested list to singly-nested list
  x <- lapply(x, flatten_list, c("POLYGON", "MUTLIPOLYGON",
                                 "GEOMETRYCOLLECTION"))
  # convert singly-nested list to flat list
  x <- lapply(x, function(x) {
    gcl <- vapply(x, inherits, logical(1), "GEOMETRYCOLLECTION")
    if (all(gcl))
      return(sf::st_geometrycollection())
    x <- x[!gcl]
    if (length(x) == 1)
      return(x[[1]])
    return(sf::st_union(sf::st_sfc(x))[[1]])
  })
  # convert flat list to sfc
  x <- sf::st_sfc(x)
  # find indices containing empty geometries
  idx <- which(!is.na(sf::st_dimension(x)))
  # remove empty geometry collections
  x <- x[idx]
  # add attribute containing original indices
  attr(x, "idx") <- idx
  # add crs
  sf::st_crs(x) <- crs
  # return sfc
  return(x)
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
