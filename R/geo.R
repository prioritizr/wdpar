#' @include internal.R
NULL

#' Transform or convert coordinates of simple feature (parallelized)
#'
#' Reproject spatial data using parallelized computations.
#'
#' @param x object of class \code{sfc}, \code{sfg} or \code{sf}.
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
  # initialize cluster
  if (threads > 1) {
    cl <- parallel::makeCluster(threads, "PSOCK")
    parallel::clusterEvalQ(cl, library(sf))
    parallel::clusterExport(cl, c("x", "crs"), envir = environment())
    doParallel::registerDoParallel(cl)
  }
  # perform processing
  result <- plyr::llply(distribute_load(ifelse(inherits(x, "sf"), nrow(x),
                                               length(x)), threads),
                        .parallel = threads > 1,
                        function(i) {
                          if (inherits(x, "sf")) {
                            return(sf::st_transform(x[i, ], crs))
                          } else {
                            return(sf::st_transform(x[i], crs))
                          }
                        })
  # cleanup
  if (threads > 1) {
    cl <- parallel::stopCluster(cl)
  }
  # post-processing
  if (inherits(x, "sf")) {
    ## merge results
    if (length(result) > 1) {
      result <- do.call(rbind, result)
    } else {
      result <- result[[1]]
    }
    ## update attributes
    attr(result, "row.names") <- as.integer(attr(result, "row.names"))
  } else {
    ## merge results
    result2 <- result
    result <- result[[1]]
    if (length(result2) > 1) {
      for (i in seq_along(result2)[-1])
        result <- append(result, result2[[i]])
    }
    ## update attributes
    attr(result, "row.names") <- NULL
  }
  attr(result, "agr") <- attr(x, "agr")
  # return result
  return(result)
}

#' Make an invalid geometry valid (parallelized)
#'
#' Fix geometry issues using parallelized computations.
#'
#' @param x object of class \code{sfc}, \code{sfg} or \code{sf}.
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
  assertthat::assert_that(inherits(x, c("sf", "sfc", "sfg")),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  if (threads > 1) {
    cl <- parallel::makeCluster(threads, "PSOCK")
    parallel::clusterEvalQ(cl, library(sf))
    parallel::clusterExport(cl, "x", envir = environment())
    doParallel::registerDoParallel(cl)
  }
  # perform processing
  result <- plyr::llply(distribute_load(ifelse(inherits(x, "sf"), nrow(x),
                                               length(x)), threads),
                        .parallel = threads > 1,
                        function(i) {
                          if (inherits(x, "sf")) {
                            return(lwgeom::st_make_valid(x[i, ]))
                          } else {
                            return(lwgeom::st_make_valid(x[i]))
                          }
                        })
  # cleanup
  if (threads > 1) {
    cl <- parallel::stopCluster(cl)
  }
  # post-processing
  if (inherits(x, "sf")) {
    ## merge results
    if (length(result) > 1) {
      result <- do.call(rbind, result)
    } else {
      result <- result[[1]]
    }
    ## remove empty geometries
    geom_length <- vapply(result[[attr(result, "sf_column")]], length,
                          integer(1))
    result <- result[geom_length > 0, ]
    ## update attributes
    attr(result, "row.names") <- as.integer(attr(result, "row.names"))
  } else {
    ## merge results
    result2 <- result
    result <- result[[1]]
    if (length(result2) > 1) {
      for (i in seq_along(result)[-1])
        result <- append(result, result2[[i]])
    }
    ## remove empty geometries
    geom_length <- vapply(result, length, integer(1))
    result <- result[geom_length > 0]
    ## update attributes
    attr(result, "row.names") <- NULL
  }
  attr(result, "agr") <- attr(x, "agr")
  # return result
  return(result)
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
