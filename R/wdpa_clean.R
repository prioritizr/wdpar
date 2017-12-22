#' @include internal.R geo.R
NULL

#' Clean data  data from the World Database on Protected Areas
#'
#' Clean data from the World Database on Protected Areas (WDPA).
#'
#' @param x \code{\link[sf]{sf}} object containing data from the WDPA.
#'
#' @param crs \code{character} coordinate reference system in PROJ4 format.
#'   Defaults to \code{3395} (Mercator).
#'
#' @param threads \code{numeric} number of threads to use for processing.
#'   Defaults to 1.
#'
#' @details This function cleans data from World Database on Protected Areas
#'   following best practices (Butchart \emph{et al.} 2015, Runge \emph{et al.}
#'   2015,
#'   \url{https://protectedplanet.net/c/calculating-protected-area-coverage}).
#'   \enumerate{
#'   \item Repair invalid geometry (using \code{\link{st_parallel_make_valid}}).
#'   \item Exclude protected areas that are not currently implemented
#'     (retain areas with the status \code{"Designated"},
#'     \code{"Inscribed"}, \code{"Established"}).
#'   \item Exclude UNESCO Biosphere Reserves (Coetzer \emph{et al.} 2014).
#'   \item Create a field (\code{"GEOMTRY_TYPE"}) indicating if areas are
#'     represented as point localities (\code{"POINT"}) or as polygons
#'     (\code{"POLYGON"}).
#'   \item Exclude areas represented as point localities that do not
#'     have a reported spatial extent (i.e. missing data for the field
#      \code{"REP_AREA"}).
#'   \item Reproject data to coordinate system specified in argument to
#'     \code{crs} (using \code{\link{st_parallel_transform}}).
#'   \item Repair any invalid geometry that has since manifested (using
#'     \code{\link{st_parallel_make_valid}}).
#'   \item Buffer areas represented as point localities to circular areas
#'     using their reported spatial extent (using data in the field
#'     \code{"REP_AREA"} and \code{\link[sf]{st_buffer}}).
#'   \item Extract terrestrial protected areas and erase regions that occur in
#'     the ocean.
#'   \item Extract marine protected areas and erase regions that occur on land.
#'   \item Merge terrestrial and marine protected area data sets.
#'   \item Erase overlapping geometries (Deguignet \emph{et al.} 2017).
#'   \item Intersect the merged data sets with the country and exclusive
#'     economic zone data (\code{\link[sf]{st_intersection}}.
#'   \item Calculate size of areas in square kilometers (stored in field
#'     \code{"AREA_KM2"}).
#'   \item Slivers are removed (geometries an area less than 1e-10 square
#'     kilometers).
#'  }
#'
#' @return \code{\link[sf]{sf}} object.
#'
#' @seealso \code{\link{wdpa_fetch}},
#'   \url{https://protectedplanet.net/c/calculating-protected-area-coverage}.
#'
#' @references
#' Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP,
#' Harfoot M, ... & Brooks TM (2015). Shortfalls and solutions for
#' meeting national and global conservation area targets.
#' \emph{Conservation Letters}, \strong{8}: 329--337.
#'
#' Coetzer KL, Witkowski ET, & Erasmus BF (2014) Reviewing
#' Biosphere Reserves globally: Effective conservation action or bureaucratic
#' label? \emph{Biological Reviews}, \strong{89}: 82--104.
#'
#' Deguignet M, Arnell A, Juffe-Bignoli D, Shi Y, Bingham H, MacSharry B &
#' Kingston N (2017). Measuring the extent of overlaps in protected area
#' designations. \emph{PloS One}, \strong{12}: e0188681.
#'
#' Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
#' (2015) Protected areas and global conservation of migratory birds.
#' \emph{Science}, \strong{350}: 1255--1258.
#'
#' @examples
#' \donttest{
#' # fetch data for the Marshall Islands
#' mhl_data <- wdpa_fetch("MHL")
#'
#' # clean data
#' mhl_cleaned_data <- wdpa_clean(mhl_data)
#'
#' # plot data for visual comparison
#' par(mfrow = c(1, 2))
#' plot(mhl_data[, 1], main = "orginal data")
#' plot(mhl_cleaned_data[, 1], main = "cleaned data")
#'
#' \dontrun{
#' # fetch data for all protected areas on the planet
#' # note that this might take some time given that the global data set is
#' # over 1 GB in size
#' global_data <- wdpa_fetch("global")
#'
#' # set number of threads for processing
#' n_threads <- max(1, parallel::detectCores(TRUE) - 1)
#'
#' # clean global data set using parallel processing
#' global_cleaned_data <- wdpa_clean(global_data, threads = n_threads)
#'
#' # plot data
#' plot(global_cleaned_data)
#' }}
#' @export
wdpa_clean <- function(x, crs = 3395, threads = 1) {
  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"),
                          nrow(x) > 0,
                          all(assertthat::has_name(x, c("ISO3", "STATUS",
                                                    "DESIG_ENG", "REP_AREA",
                                                    "MARINE"))),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.count(threads),
                          isTRUE(threads <= parallel::detectCores(TRUE)),
                          pingr::is_online())
  # clean data
  ## repair geometry
  x <- st_parallel_make_valid(x, threads)
  ## remove areas that are not currently in action
  x <- x[x$STATUS %in% c("Designated", "Inscribed", "Established"), ]
  ## remove UNESCO sites
  x <- x[x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve", ]
  ## assign column indicating geometry type
  is_point <- vapply(x$geometry, inherits, logical(1),  "POINT") |
              vapply(x$geometry, inherits, logical(1),  "MULTIPOINT")
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## remove protected areas represented as points that do not have
  ## a reported area
  x <- x[!(x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA)), ]
  ## reproject data
  x <- st_parallel_transform(x, crs, threads)
  ## repair geometry again
  x <- st_parallel_make_valid(x, threads)
  ## buffer areas represented as points
  x_points_pos <- which(x$GEOMETRY_TYPE == "POINT")
  if (length(x_points_pos) > 0) {
    x_points_data <- x[x_points_pos, ]
    x_points_data <- sf::st_buffer(x_points_data,
                       sqrt((x_points_data$REP_AREA * 1000000 / pi)))
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
  }
  ## fetch land and eez data
  if (length(unique(x$ISO3)) > 20) {
    land_ezz_data <- land_and_eez_fetch("global", threads = threads,
                                        verbose = verbose)
  } else {
    land_ezz_data <- lapply(unique(x$ISO3), land_and_eez_fetch,
                            crs = crs, threads = threads, verbose = FALSE)
    if (length(land_ezz_data) > 1) {
      land_ezz_data <- do.call(rbind, land_ezz_data)
    } else {
      land_ezz_data <- land_ezz_data[[1]]
    }
  }
  land_data <- sf::st_union(land_ezz_data[land_ezz_data$TYPE == "LAND"])
  ## erase terrestrial areas that do not occur on land
  terrestrial_present <- FALSE
  if (any(x$MARINE == "0")) {
    terrestrial_present <- TRUE
    x_terrestrial_data <- x[x$MARINE == "0", ]
    x_terrestrial_data <- suppressWarnings(st_parallel_intersection(
                            x_terrestrial_data, land_data, threads = threads))
  }
  ## erase marine areas that occur on land
  marine_present <- FALSE
  if (any(x$MARINE == "2")) {
    marine_present <- TRUE
    x_marine_data <- x[x$MARINE == "1", ]
    x_marine_data <- suppressWarnings(st_parallel_difference(x_marine_data,
                                                        land_data,
                                                        threads = threads))
  }
  ## combine terrestrial and marine data sets
  if (terrestrial_present && marine_present) {
    x <- rbind(x[x$MARINE == "1", ], rbind(x_terrestrial_data,
                                           x_marine_data))
  } else if (terrestrial_present) {
    x <- rbind(x[x$MARINE == "1", ], x_terrestrial_data)
  } else {
    x <- rbind(x[x$MARINE == "1", ], x_marine_data)
  }
  ## dissolve data
  x <- st_union(x)
  x <- sf::st_sf(MANAGEMENT = rep("PA", length(x)), geometry = x)
  ## intersect data with land and eez data
  x_land_and_eez_areas <- suppressWarnings(st_parallel_intersection(x,
                            land_ezz_data, threads = threads))
  ## find areas in high seas
  x_high_seas <- suppressWarnings(st_parallel_difference(x, land_ezz_data,
                                                         threads = threads))
  if (nrow(x_high_seas) > 0) {
    x_high_seas$TYPE <- "HIGH SEAS"
    x_high_seas$ISO3 <- NA
  }
  ## merge data
  if ((nrow(x_land_and_eez_areas) > 0) && (nrow(x_high_seas) > 0)) {
    x <- rbind(x_land_and_eez_areas, x_high_seas)
  } else if (nrow(x_land_and_eez_areas) > 0) {
    x <- x_land_and_eez_areas
  } else {
    x <- x_high_seas
  }
  ## calculate area in square kilometers
  areas <- units::set_units(sf::st_area(x), km^2)
  x$AREA_KM2 <- as.numeric(areas)
  ## remove slivers
  x <- x[x$AREA_KM2 > 1e-10, ]
  # return cleaned data
  return(x)
}
