#' @include internal.R
NULL

#' SAGA Spatial Difference
#'
#' This function performs a spatial difference using the System for Automated
#' Geoscientific Analyses (SAGA) software.
#'
#' @param x \code{\link[sf]{sfc}} object.
#'
#' @param y \code{\link[sf]{sfc}} object.
#'
#' @param saga_env \code{\link[base]{list}} object that contains information
#'   for interfacing with SAGA. This object is typically created using
#'   \code{\link[RSAGA]{rsaga.env}}.
#'
#' @details This function requires the SAGA software to be installed on the
#'   system. For information on installing SAGA, refer to:
#'   https://sourceforge.net/p/saga-gis/wiki/Binary%20Packages/.
#'
#' @return \code{\link[sf]{sf}} object.
#'
#' @seealso \code{\link[sf]{st_difference}}, \url{http://www.saga-gis.org}.
#'
#' @examples
#' # load sf package
#' librarys(sf)
#'
#' # make example data
#' b0 <- sf::st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1),
#'                                 c(-1, -1))))
#' b1 <- b0 + 2
#' b2 <- b0 + c(-0.2, 2)
#' x <- st_union(st_sfc(b0, b1, b2))
#' a0 <- b0 * 0.8
#' a1 <- a0 * 0.5 + c(2, 0.7)
#' a2 <- a0 + 1
#' a3 <- b0 * 0.5 + c(2, -0.5)
#' y <- st_union(st_sfc(a0, a1, a2, a3))
#'
#' # make data for comparison
#' o1 <- st_difference(x, y)
#' o2 <- saga_difference(x, y)
#'
#' # test if sf and saga yield same result
#' st_precision(o1) <- 1000
#' st_precision(o2) <- 1000
#' st_equals(o1, o2, sparse = FALSE)[1] # TRUE
#'
#' # plot sf and saga outputs for visual comparison
#' par(mfrow = c(1, 2))
#' plot(o1, main = "st_difference")
#' plot(o2, main = "saga_difference")
#' @export
saga_difference <- function(x, y, saga_env = RSAGA::rsaga.env()) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "sfc"), inherits(y, "sfc"),
                          identical(sf::st_crs(x)[[2]], sf::st_crs(y)[[2]]),
                          inherits(saga_env, "list"))
  # create temp files
  d <- file.path(tempdir(), basename(tempfile()))
  dir.create(d, showWarnings = FALSE, recursive = FALSE)
  x_path <- tempfile(fileext = ".shp", tmpdir = d)
  y_path <- tempfile(fileext = ".shp", tmpdir = d)
  z_path <- tempfile(fileext = ".shp", tmpdir = d)
  # sink x and y to disk
  sf::write_sf(sf::st_sf(x), x_path, delete_layer = TRUE, quiet = TRUE)
  sf::write_sf(sf::st_sf(y), y_path, delete_layer = TRUE, quiet = TRUE)
  # geoprocessing
  suppressMessages({suppressWarnings({
    RSAGA::rsaga.geoprocessor(
      lib = "shapes_polygons",
      module = "Difference",
      list(A = x_path, B = y_path, RESULT = z_path, SPLIT = 0),
           env = saga_env,
           check.parameters = FALSE,
           show.output.on.console = FALSE)
  })})
  # load output
  out <- sf::read_sf(z_path, quiet = TRUE)$geometry
  # if out is an empty geometry set, then replace with empty geometry collection
  if (length(out) == 0L)
    out <- sf::st_sfc(sf::st_polygon())
  # assign coordinate system
  suppressWarnings({
    out <- sf::st_set_crs(out, sf::st_crs(x))
  })
  # delete tmp files
  unlink(d, force = TRUE, recursive = TRUE)
  # return output
  out
}

#' SAGA Spatial Union
#'
#' This function performs a spatial union using the System for Automated
#' Geoscientific Analyses (SAGA) software.
#'
#' @inheritParams saga_difference
#'
#' @inherit saga_difference return details
#'
#' @seealso \code{\link[sf]{st_union}}, \url{http://www.saga-gis.org}.
#'
#' @examples
#' # load packages
#' library(sf)
#'
#' # read in data for example
#' x <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' x <- sf::st_geometry(x)
#'
#' # run unions using sf and SAGA
#' o1 <- st_union(x)
#' o2 <- saga_union(x)
#'
#' # test if sf and saga yield same result
#' st_precision(o1) <- 1000
#' st_precision(o2) <- 1000
#' st_equals(o1, o2, sparse = FALSE)[1] # TRUE
#'
#' # plot sf and saga outputs for visual comparison
#' par(mfrow = c(1, 2))
#' plot(o1, main = "st_difference")
#' plot(o2, main = "saga_difference")
#' @export
saga_union <- function(x, saga_env = RSAGA::rsaga.env()) {
  # assert that arguments are valid
  assertthat::assert_that(inherits(x, "sfc"),
                          inherits(saga_env, "list"))
  # if x only contains a single geometry, then simply return x
  if (length(x) == 1)
    return(x)
  # create temp files
  d <- file.path(tempdir(), basename(tempfile()))
  dir.create(d, showWarnings = FALSE, recursive = FALSE)
  x_path <- tempfile(fileext = ".shp", tmpdir = d)
  z_path <- tempfile(fileext = ".shp", tmpdir = d)
  # sink x and y to disk
  sf::write_sf(sf::st_sf(x), x_path, delete_layer = TRUE, quiet = TRUE)
  # geoprocessing
  suppressMessages({suppressWarnings({
    RSAGA::rsaga.geoprocessor(
      lib = "shapes_polygons",
      module = "Polygon Dissolve",
      list(POLYGONS = x_path, DISSOLVED = z_path, STAT_AVG = 0),
           env = saga_env,
           check.parameters = FALSE,
           show.output.on.console = FALSE)
  })})
  # load output
  out <- sf::read_sf(z_path, quiet = TRUE)$geometry
  suppressWarnings({
    out <- sf::st_set_crs(out, sf::st_crs(x))
  })
  # delete tmp files
  unlink(d, force = TRUE, recursive = TRUE)
  # return output
  out
}
