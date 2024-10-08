#' @include internal.R
NULL

#' Erase overlaps
#'
#' Erase overlapping geometries in a [sf::sf()] object.
#'
#' @param x [sf::sf()] object.
#'
#' @param verbose `logical` should progress be reported? Defaults to
#'   `FALSE`.
#'
#' @details This is a more robust -- albeit slower -- implementation for
#'   [sf::st_difference()] when `y` is missing.
#'
#' @return A [sf::sf()] object.
#'
#' @seealso [sf::st_difference()], [wdpa_dissolve()].
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
#' plot(sf::st_geometry(x), xlim = c(0, 200), ylim = c(0, 250),
#'      main = "original", col = "transparent")
#' plot(sf::st_geometry(y), , xlim = c(0, 200), ylim = c(0, 250),
#'      main = "no overlaps", col = "transparent")
#' @export
st_erase_overlaps <- function(x, verbose = FALSE) {
  # validate arguments
  assertthat::assert_that(inherits(x, "sf"),
                          assertthat::is.flag(verbose))
  # extract precision
  precision <- sf::st_precision(x)
  # processing
  g <- sf::st_geometry(x)
  o <- g[1]
  # initialize progress bar
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :current/:total (:percent) eta: :eta",
      total = length(g) - 1, clear = FALSE, width = 60)
  }
  for (i in seq(2, length(g))) {
    ## find overlapping geometries
    ovr <- sf::st_intersects(g[i], o)[[1]]
    ## if overlapping geometries then calculate difference
    if (length(ovr) > 0) {
      ## create union
      ### run union
      u <- sf::st_union(
        sf::st_set_precision(
          suppressWarnings(sf::st_collection_extract(sf::st_buffer(o[ovr], 0))),
          precision
        )
      )
      ### buffer the union to fix any geometry issues
      u <- sf::st_buffer(u, dist = 0)
      ### repair the geometry if there are any issues
      if (!all(sf::st_is_valid(u))) {
        #nocov start
        u <- suppressWarnings(
          sf::st_collection_extract(
            sf::st_make_valid(sf::st_set_precision(u, precision)),
            "POLYGON"
          )
        )
        #nocov end
      }
      ## calculate difference
      ### run difference
      d <- sf::st_difference(
        suppressWarnings(
          sf::st_collection_extract(
            sf::st_make_valid(sf::st_set_precision(g[i], precision)),
            "POLYGON"
          )
        ),
        suppressWarnings(
          sf::st_collection_extract(
            sf::st_make_valid(sf::st_set_precision(u, precision)),
            "POLYGON"
          )
        )
      )
      if (length(d) == 0L) {
        d <- sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(d)) #nocov
      }
      d <- suppressWarnings(sf::st_collection_extract(d, "POLYGON"))
      ### repair the geometry if there are any issues
      if (!all(sf::st_is_valid(d))) {
        #nocov start
        d <- suppressWarnings(
          sf::st_collection_extract(
            sf::st_make_valid(sf::st_set_precision(d, precision)),
            "POLYGON"
          )
        )
        #nocov end
      }
    } else {
      d <- g[i]
    }
    ## find empty geometries
    empty <- sf::st_is_empty(d)
    ## process geometry if its not empty
    if (!all(empty)) {
      ### remove slivers (areas less then 1 m^2)
      d <- d[!empty]
      d <- sf::st_cast(d, "POLYGON")
      d <- d[as.numeric(sf::st_area(d)) > 1]
      d <- sf::st_cast(d, "MULTIPOLYGON")
      if (length(d) == 0) {
        d <- sf::st_sfc(sf::st_geometrycollection(), crs = sf::st_crs(d)) #nocov
      }
      d <- suppressWarnings(sf::st_collection_extract(d, "POLYGON"))
    }
    ## if d contains multiple geometries, then union them
    if (length(d) > 1) {
      d <- sf::st_union(d)
      d <- suppressWarnings(
        sf::st_collection_extract(
          sf::st_make_valid(sf::st_set_precision(d, precision)),
          "POLYGON"
        )
      )
    }
    ## create empty geometry if empty
    if (length(d) == 0) {
      d <- sf::st_sfc(sf::st_polygon()) #nocov
    }
    ## store geometry
    o[i] <- d[[1]]
    ## increment progress bar
    if (verbose) pb$tick()
  }
  x <- sf::st_set_geometry(x, o)
  x <- sf::st_set_precision(x, precision)
  # return output
  x
}
