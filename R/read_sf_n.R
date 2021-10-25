#' Read spatial data
#'
#' Import spatial data. If desired, only a subset of the available data
#' are imported.
#'
#' @param dsn `character` data source name.
#'
#' @param layer `character` layer name. Defaults to `NULL`.
#'
#' @param n `integer` number of records to import.
#'   Defaults to `NULL` such that all data are imported.
#'
#' @return [sf::sf()] object.
#'
#' @noRd
read_sf_n <- function(dsn, layer = NULL, n = NULL) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(dsn),
                          inherits(layer, c("character", "NULL")),
                          inherits(n, c("numeric", "NULL")))
  if (!is.null(n)) {
   assertthat::assert_that(assertthat::is.count(n),
                           assertthat::noNA(n))
  }
  if (is.null(layer)) {
    layer <- sf::st_layers(dsn)$name[[1]]
  }
  assertthat::assert_that(assertthat::is.string(layer),
                          assertthat::noNA(layer))
  # construct query
  if (!is.null(n)) {
    query <- paste0("SELECT * FROM \"", layer,"\" WHERE FID <= ", n)
  } else {
    query <- paste0("SELECT * FROM \"", layer, "\"")
  }
  # import data
  out <- sf::read_sf(dsn = dsn, query = query)
  if (!is.null(n)) {
    if (nrow(out) > n) {
      out <- out[seq_len(n), ]
    }
  }
  # force sf_geometry column to be called "geometry"
  if (!"geometry" %in% names(out)) {
    old_name <- attr(out, "sf_column")
    names(out)[names(out) == old_name] <- "geometry"
    attr(out, "sf_column") <- "geometry"
  }
  # return result
  out
}
