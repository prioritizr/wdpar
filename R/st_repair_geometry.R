#' Repair geometry
#'
#' Repair the geometry of a [sf::st_sf()] object.
#'
#' @param x [sf::sf()] object.
#'
#' @inheritParams wdpa_clean
#'
#' @details
#' This function works by first using the [sf::st_make_valid()] function
#' to attempt to fix geometry issues. Since the [sf::st_make_valid()] function
#' sometimes produce incorrect geometries in rare cases
#' (e.g. when fixing invalid geometries that cross the dateline),
#' this function then uses the `st_prepair()` function from the \pkg{prepr}
#' package to fix those geometries instead
#' (see <https://github.com/dickoa/prepr> for details).
#'
#' @section Installation:
#' This function uses the \pkg{prepr} package to help repair geometries
#' in certain cases. Because the \pkg{prepr} package is not available on
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' its online code repository. To achieve this, please
#' use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("dickoa/prepr")
#' ```
#'
#' @examples
#' # create sf object
#' p1 <- st_sf(
#'   id = 1,
#'   geometry = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))", crs = 3857)
#' )
#'
#' # repair geometry
#' p2 <- st_repair_geometry(p1)
#'
#' # print object
#' print(p2)
#' @export
st_repair_geometry <- function(x, geometry_precision = 1500) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    !assertthat::has_name(x, "_repair_id"),
    assertthat::is.count(geometry_precision),
    assertthat::noNA(geometry_precision)
  )

  # preliminary calculations
  if (sf::st_crs(x) != st_crs(NA)) {
    ## compute global extent in coordinate system of x (if crs defined)
    global_bbox <- sf::st_as_sfc(
      "POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))",
      crs = 4326
    )
    if (sf::st_crs(x) != sf::st_crs(4326)) {
      global_bbox <- sf::st_transform(global_bbox, sf::st_crs(x))
    }
    global_bbox <- sf::st_bbox(global_bbox)

    ## compute distance threshold for invalid outputs from st_make_valid()
    dist_threshold <- unname(global_bbox$xmax - global_bbox$xmin) * 0.7
  }

  # add in identifier column to keep track of geometries
  x[["_repair_id"]] <- seq_len(nrow(x))

  # set precision
  x <- sf::st_set_precision(x, geometry_precision)

  # apply first pass for fixing geometry
  x2 <- sf::st_make_valid(x)

  # remove empty geometries
  x2 <- x2[!sf::st_is_empty(x2), ]

  # extract polygons and points (if needed)
  x2 <- extract_polygons_and_points(x2)

  # additional processing to detect if invalid geometries
  ## note this only works if x has a defined CRS
  if (sf::st_crs(x) != st_crs(NA)) {
    ## detect if any polygons are invalid
    invalid_idx <- which(
      vapply(sf::st_geometry(x2), FUN.VALUE = logical(1), function(y) {
        b <- sf::st_bbox(y)
        (b$xmax - b$xmin) > dist_threshold
      })
    )

    ## manually fix geometries if needed
    if (length(invalid_idx) > 0) {
      ### verify that prepr package is installed
      assertthat::assert_that(
        requireNamespace("prepr"),
        msg = paste(
          "the \"prepr\" package needs to be installed, use: \n",
          "remotes::install_github(\"dickoa/prepr\")"
        )
      )
      ### fix geometries
      invalid_ids <- x2[["_repair_id"]][invalid_idx]
      x2 <- rbind(
        x2[!x2[["_repair_id"]] %in% invalid_ids, , drop = FALSE],
        prepr::st_prepair(
          x[x[["_repair_id"]] %in% invalid_ids, , drop = FALSE]
        )
      )
    }
  }

  # remove custom id column
  geom_col <- attr(x2, "sf_column")
  x2 <- x2[, setdiff(names(x2), c("_repair_id", geom_col)), drop = FALSE]

  # return result
  x2
}
