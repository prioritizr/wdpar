#' @include internal.R st_erase_overlaps.R
NULL

#' Clean data
#'
#' Clean data obtained from
#' [Protected Planet](https://www.protectedplanet.net/en).
#' Specifically, this function is designed to clean data obtained from
#' the World Database on Protected Areas
#' (WDPA) and the World Database on Other Effective Area-Based Conservation
#' Measures (WDOECM).
#' For recommended practices on cleaning large datasets
#' (e.g. datasets that span multiple countries or a large geographic area),
#' please see below.
#'
#' @param x [sf::sf()] object containing protected area data.
#'
#' @param crs `character` or `integer` object representing a
#'   coordinate reference system. Defaults to World Behrmann
#'  (*ESRI:54017*).
#'
#' @param exclude_unesco `logical` should UNESCO Biosphere Reserves be excluded?
#'   Defaults to `TRUE`.
#'
#' @param retain_status `character` vector containing the statuses for
#'   protected areas that should be retained during the cleaning process.
#'   Available statuses include:
#'   `"Proposed"`, `"Inscribed"`, `"Adopted"`, `"Designated"`, and
#'   `"Established"`.
#'   Additionally, a `NULL` argument can be specified to ensure that no
#'   protected areas are excluded according to their status.
#'   The default argument is a `character` vector containing `"Designated"`,
#'   `"Inscribed"`, and `"Established"`.
#'   This default argument ensures that protected areas that are not currently
#'   implemented are excluded.
#'
#' @param snap_tolerance `numeric` tolerance for snapping geometry to a
#'   grid for resolving invalid geometries. Defaults to 1 meter.
#'
#' @param simplify_tolerance `numeric` simplification tolerance.
#'   Defaults to 0 meters.
#'
#' @param geometry_precision `numeric` level of precision for processing
#'   the spatial data (used with [sf::st_set_precision()]). The
#'   default argument is 1500 (higher values indicate higher precision).
#'   This level of precision is generally suitable for analyses at the
#'   national-scale. For analyses at finer-scale resolutions, please
#'   consider using a greater value (e.g. 10000).
#'
#' @param erase_overlaps `logical` should overlapping boundaries be
#'   erased? This is useful for making comparisons between individual
#'   protected areas and understanding their "effective" geographic coverage.
#'   On the other hand, this processing step may not be needed
#'   (e.g. if the protected area boundaries are going to be rasterized), and so
#'   processing time can be substantially by skipping this step and setting
#'   the argument to `FALSE`. Defaults to `TRUE`.
#'
#' @param verbose `logical` should progress on data cleaning be reported?
#'   Defaults to `TRUE` in an interactive session, otherwise
#'   `FALSE`.
#'
#' @details This function cleans data following best practices
#'   (Butchart *et al.* 2015; Protected Planet 2021; Runge *et al.* 2015).
#'   To obtain accurate protected area coverage statistics for a country,
#'   please note that you will need to manually clip the cleaned data to
#'   the countries' coastline and its Exclusive Economic Zone (EEZ).
#'
#'   \enumerate{
#'
#'   \item Exclude protected areas according to their status (i.e.
#'     `"STATUS"` field). Specifically, protected areas that have
#'     a status not specified in the argument to `retain_status` are excluded.
#'     By default, only protected areas that have a
#'     `"Designated"`, `"Inscribed"`, or `"Established"` status are retained.
#'    This means that the default behavior is to exclude protected that
#'    are not currently implemented.
#'
#'   \item Exclude United Nations Educational, Scientific and Cultural
#'     Organization (UNESCO) Biosphere Reserves (Coetzer *et al.* 2014).
#'     This step is only performed if the argument to `exclude_unesco` is
#'     `TRUE`.
#'
#'   \item Standardize column names. This is important so that data
#'     imported as in shapefile or file geodatabase format have the
#'     same column names. Specifically, if present, the `"PARENT_ISO3"` field is
#'     renamed to "PARENT_ISO" and the "SHAPE" field is renamed to
#'     `"geometry"`.
#'
#'   \item Create a field (`"GEOMETRY_TYPE"`) indicating if areas are
#'     represented as point localities (`"POINT"`) or as polygons
#'     (`"POLYGON"`).
#'
#'   \item Exclude areas represented as point localities that do not
#'     have a reported spatial extent (i.e. missing data for the field
#      `"REP_AREA"`).
#'
#'   \item Geometries are wrapped to the dateline (using
#'     [sf::st_wrap_dateline()] with the options
#'     `"WRAPDATELINE=YES"` and `"DATELINEOFFSET=180"`).
#'
#'   \item Reproject data to coordinate system specified in argument to
#'     `crs` (using [sf::st_transform()]).
#'
#'   \item Repair any invalid geometries that have manifested
#'     (using [st_repair_geometry()]).
#'
#'   \item Buffer areas represented as point localities to circular areas
#'     using their reported spatial extent (using data in the field
#'     `"REP_AREA"` and [sf::st_buffer()]; see Visconti
#'     *et al.* 2013).
#'
#'   \item Snap the geometries to a grid to fix any remaining
#'     geometry issues (using argument to `snap_tolerance` and
#'     [lwgeom::st_snap_to_grid()]).
#'
#'   \item Repair any invalid geometries that have manifested
#'     (using [st_repair_geometry()]).
#'
#'   \item Simplify the protected area geometries to reduce computational burden
#'     (using argument to `simplify_tolerance` and
#'     [sf::st_simplify()]).
#'
#'   \item Repair any invalid geometries that have manifested
#'     (using [st_repair_geometry()]).
#'
#'   \item The `"MARINE"` field is converted from integer codes
#'     to descriptive names (i.e. `0` = `"terrestrial"`,
#'     `1` = `"partial"`, `2` = `"marine"`).
#'
#'   \item The `"PA_DEF"` field is converted from integer codes
#'   to descriptive names (i.e. `0` = `"OECM"`, and `1` = `"PA"`).
#'
#'   \item Zeros in the `"STATUS_YR"` field are replaced with
#'     missing values (i.e. `NA_real_` values).
#'
#'   \item Zeros in the `"NO_TK_AREA"` field are replaced with `NA`
#'     values for areas where such data are not reported or applicable
#'     (i.e. areas with the values `"Not Applicable"`
#'     or `"Not Reported"` in the `"NO_TK_AREA"` field).
#'
#'   \item Overlapping geometries are erased from the protected area data
#'     (discussed in Deguignet *et al.* 2017). Geometries are erased such
#'     that areas associated with more effective management
#'     categories (`"IUCN_CAT"`) or have historical precedence are retained
#'     (using [sf::st_difference()]).
#'
#'   \item Slivers are removed (geometries with areas less than 0.1 square
#'     meters).
#'
#'   \item The size of areas are calculated in square kilometers and stored in
#'     the field `"AREA_KM2"`.
#'
#'   \item Trimming extra leading or trailing white space characters
#'     from the `"MANG_PLAN"` field  (e.g., `" "`, `"\n"`, `"\r"`).
#'  }
#'
#' @section Recommended practices for large datasets:
#' This function can be used to clean large datasets assuming that
#' sufficient computational resources and time are available.
#' Indeed, it can clean data spanning large countries, multiple
#' countries, and even the full global dataset.
#' When processing the full global dataset, it is recommended to use a
#' computer system with at least 32 GB RAM available and to allow for at least
#' one full day for the data cleaning procedures to complete.
#' It is also recommended to avoid using the computer system for any other
#' tasks while the data cleaning procedures are being completed,
#' because they are very computationally intensive.
#' Additionally, when processing large datasets -- and especially
#' for the global dataset -- it is strongly recommended to disable the
#' procedure for erasing overlapping areas.
#' This is because the built-in procedure for erasing overlaps is
#' very time consuming when processing many protected areas, so that
#' information on each protected area can be output
#' (e.g. IUCN category, year established).
#' Instead, when cleaning large datasets, it is recommended to run
#' the data cleaning procedures with the procedure for erasing
#' overlapping areas disabled (i.e. with `erase_overlaps = FALSE`).
#' After the data cleaning procedures have completed,
#' the protected area data can be manually dissolved
#' to remove overlapping areas (e.g. using [wdpa_dissolve()]).
#' For an example of processing a large protected area dataset,
#' please see the vignette.
#'
#' @return A [sf::sf()] object.
#'
#' @seealso [wdpa_fetch()], [wdpa_dissolve()].
#'
#' @references
#' Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP,
#' Harfoot M, ... & Brooks TM (2015) Shortfalls and solutions for
#' meeting national and global conservation area targets.
#' *Conservation Letters*, **8**: 329--337.
#'
#' Coetzer KL, Witkowski ET, & Erasmus BF (2014) Reviewing
#' Biosphere Reserves globally: Effective conservation action or bureaucratic
#' label? *Biological Reviews*, **89**: 82--104.
#'
#' Deguignet M, Arnell A, Juffe-Bignoli D, Shi Y, Bingham H, MacSharry B &
#' Kingston N (2017) Measuring the extent of overlaps in protected area
#' designations. *PloS One*, **12**: e0188681.
#'
#' Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
#' (2015) Protected areas and global conservation of migratory birds.
#' *Science*, **350**: 1255--1258.
#'
#' Protected Planet (2021) Calculating protected and OECM area coverage.
#' Available at:
#' <https://www.protectedplanet.net/en/resources/calculating-protected-area-coverage>.
#'
#' Visconti P, Di Marco M, Alvarez-Romero JG, Januchowski-Hartley SR, Pressey,
#' RL, Weeks R & Rondinini C (2013) Effects of errors and gaps in spatial data
#' sets on assessment of conservation progress. *Conservation Biology*,
#' **27**: 1000--1010.
#'
#' @examples
#' \dontrun{
#' # fetch data for the Liechtenstein
#' lie_raw_data <- wdpa_fetch("LIE", wait = TRUE)
#'
#' # clean data
#' lie_data <- wdpa_clean(lie_raw_data)
#'
#' # plot cleaned dataset
#' plot(lie_data)
#'
#' }
#' @export
wdpa_clean <- function(x,
                       crs = paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0",
                       "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"),
                       exclude_unesco = TRUE,
                       retain_status =
                        c("Designated", "Inscribed", "Established"),
                       snap_tolerance = 1,
                       simplify_tolerance = 0,
                       geometry_precision = 1500,
                       erase_overlaps = TRUE,
                       verbose = interactive()) {
  # check arguments are valid
  ## display message
  if (isTRUE(verbose)) {
    cli::cli_progress_step("initializing")
  }
  ## simple arguments
  assertthat::assert_that(inherits(x, "sf"),
                          nrow(x) > 0,
                          all(assertthat::has_name(x, c("ISO3", "STATUS",
                                                        "DESIG_ENG", "REP_AREA",
                                                        "MARINE", "PA_DEF"))),
                          assertthat::is.string(crs) ||
                          assertthat::is.count(crs),
                          assertthat::is.number(snap_tolerance),
                          isTRUE(snap_tolerance >= 0),
                          assertthat::is.number(simplify_tolerance),
                          isTRUE(simplify_tolerance >= 0),
                          assertthat::is.count(geometry_precision),
                          assertthat::is.flag(erase_overlaps),
                          assertthat::is.flag(exclude_unesco),
                          assertthat::is.flag(verbose))
  ## retain status
  assertthat::assert_that(inherits(retain_status, c("character", "NULL")))
  if (is.character(retain_status)) {
    assertthat::assert_that(
      assertthat::noNA(retain_status),
      all(retain_status %in% c(
        "Proposed", "Inscribed", "Adopted", "Designated", "Established")))
  }
  ## check that x is in wgs1984
  assertthat::assert_that(sf::st_crs(x) == sf::st_crs(4326),
   msg = "argument to x is not longitude/latitude (i.e. EPSG:4326)")

  # clean data
  ## exclude areas based on status
  if (is.null(retain_status)) {
    if (verbose) {
      cli::cli_progress_step("retaining areas regardless of status)")
    }
  } else {
    if (verbose) {
      cli::cli_progress_step("retaining only areas with specified statuses")
    }
    x <- x[which(x$STATUS %in% retain_status), ]
  }
  ## remove UNESCO sites if needed
  if (exclude_unesco) {
    if (verbose) {
      cli::cli_progress_step("removing UNESCO Biosphere Reserves")
    }
    x <- x[x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve", ]
  } else {
    if (verbose) {
      cli::cli_progress_step("retaining UNESCO Biosphere Reserves")
    }
  }
  # standardize column names
  if (verbose) {
    cli::cli_progress_step("standardizing field names")
  }
  if ("PARENT_ISO3" %in% names(x)) {
    names(x)[names(x) == "PARENT_ISO3"] <- "PARENT_ISO"
  }
  if ("SHAPE" %in% names(x)) {
    names(x)[names(x) == "SHAPE"] <- "geometry"
    x <- sf::st_set_geometry(x, "geometry")
  }
  if (verbose) {
    cli::cli_progress_step("standardizing field names")
  }
  ## assign column indicating geometry type
  is_point <- vapply(sf::st_geometry(x), inherits, logical(1),
                     c("POINT", "MULTIPOINT"))
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## remove protected areas represented as points that do not have
  ## a reported area
  if (verbose) {
    cli::cli_progress_step("removing points with no reported area")
  }
  x <- x[!(x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA)), ]
  ## wrap dateline issues
  if (verbose) {
    cli::cli_progress_step("wrapping dateline")
  }
  x <- sf::st_set_precision(x, geometry_precision)
  x <- suppressWarnings(sf::st_wrap_dateline(x,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")))
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  x <- sf::st_set_precision(x, geometry_precision)
  ## repair geometry
  if (verbose) {
    cli::cli_progress_step("repairing geometry")
  }
  x <- st_repair_geometry(x, geometry_precision)
  ## reproject data
  if (verbose) {
    cli::cli_progress_step("reprojecting data")
  }
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_transform(x, crs)
  x <- sf::st_set_precision(x, geometry_precision)
  ## repair geometry again
  if (verbose) {
    cli::cli_progress_step("repairing geometry")
  }
  x <- st_repair_geometry(x, geometry_precision)
  ## buffer polygons by zero to fix any remaining issues
  x_polygons_pos <- which(x$GEOMETRY_TYPE == "POLYGON")
  if (length(x_polygons_pos) > 0) {
    if (verbose) {
      cli::cli_progress_step("further geometry fixes (i.e. buffering by zero)")
    }
    if (verbose) message("buffering by zero: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_polygons_data <- x[x_polygons_pos, ]
    x_polygons_data <- sf::st_set_precision(x_polygons_data, geometry_precision)
    x_polygons_data <- sf::st_buffer(x_polygons_data, 0)
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POINT"), ], x_polygons_data)
    x <- sf::st_set_precision(x, geometry_precision)
  }
  ## buffer areas represented as points
  x_points_pos <- which(x$GEOMETRY_TYPE == "POINT")
  if (length(x_points_pos) > 0) {
    if (verbose) {
      cli::cli_progress_step("buffering points to reported area")
    }
    x_points_data <- x[x_points_pos, ]
    x_points_data <- sf::st_buffer(x_points_data,
                       sqrt((x_points_data$REP_AREA * 1e6) / pi))
    if (any(x$GEOMETRY_TYPE == "POLYGON")) {
      x <- rbind(x[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
    } else {
      x <- x_points_data
    }
    x <- sf::st_set_precision(x, geometry_precision)
  }
  ## return empty dataset if no valid non-empty geometries remain
  if (all(sf::st_is_empty(x))) {
    if (verbose) {
      cli::cli_alert_warning(
        "no valid non-empty geometries remain, returning empty dataset")
    }
    return(empty_wdpa_dataset(sf::st_crs(x)))
  }
  ## simplify geometries
  if (simplify_tolerance > 0) {
    if (verbose) {
      cli::cli_progress_step("simplifying geometry")
    }
    x <- sf::st_set_precision(x, geometry_precision)
    x <- sf::st_simplify(x, TRUE, simplify_tolerance)
    x <- sf::st_set_precision(x, geometry_precision)
    x <- x[!sf::st_is_empty(x), ]
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- sf::st_set_precision(x, geometry_precision)
  }
  ## repair geometry again
  if (verbose) {
    cli::cli_progress_step("repairing geometry")
  }
  x <- st_repair_geometry(x, geometry_precision)
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    if (verbose) {
      cli::cli_progress_step("snapping geometry to tolerance")
    }
    x <- sf::st_set_precision(x, geometry_precision)
    x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
    x <- sf::st_set_precision(x, geometry_precision)
  }
  ## repair geometry again
  if (verbose) {
    cli::cli_progress_step("repairing geometry")
  }
  x <- st_repair_geometry(x, geometry_precision)
  ## format columns
  if (verbose) {
    cli::cli_progress_step("formatting attribute data")
  }
  ### MARINE field
  x$MARINE[x$MARINE == "0"] <- "terrestrial"
  x$MARINE[x$MARINE == "1"] <- "partial"
  x$MARINE[x$MARINE == "2"] <- "marine"
  ### STATUS_YR field
  x$STATUS_YR[x$STATUS_YR == 0] <- NA_real_
  ### NO_TK_AREA field
  x$NO_TK_AREA[x$NO_TAKE %in% c("Not Reported", "Not Applicable")] <- NA_real_
  ### PA_DEF field
  x$PA_DEF <- as.character(x$PA_DEF)
  x$PA_DEF[x$PA_DEF == "0"] <- "OECM"
  x$PA_DEF[x$PA_DEF == "1"] <- "PA"
  if (verbose) {
    cli::cli_progress_done()
  }
  ## remove overlaps data
  if (erase_overlaps && isTRUE(nrow(x) > 1)) {
    x$IUCN_CAT <- factor(as.character(x$IUCN_CAT),
                         levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                                    "Not Reported", "Not Applicable",
                                    "Not Assigned"))
    x <- sf::st_set_precision(x, geometry_precision)
    x <- st_erase_overlaps(x[order(x$IUCN_CAT, x$STATUS_YR), ], verbose)
    x$IUCN_CAT <- as.character(x$IUCN_CAT)
    x <- x[!sf::st_is_empty(x), ]
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- sf::st_set_precision(x, geometry_precision)
  }
  ## remove slivers
  if (verbose) {
    cli::cli_progress_step("removing slivers")
  }
  x <- x[as.numeric(sf::st_area(x)) > 0.1, ]
  ## calculate area in square kilometers
  if (verbose) {
    cli::cli_progress_step("calculating spatial statistics")
  }
  areas <- as.numeric(sf::st_area(x)) * 1e-6
  x$AREA_KM2 <- as.numeric(areas)
  ## trim white space characters
  if (verbose) {
    cli::cli_progress_step(
      "trimming extra white space characters from MANG_PLAN"
    )
  }
  x$MANG_PLAN <- trimws(x$MANG_PLAN)
  ## move geometry to last column
  if ((!"geometry" %in% names(x))) {
    geom_col <- attr(x, "sf_column")
    attr(x, "sf_column") <- "geometry"
    names(x)[names(x) == geom_col] <- "geometry"
  }
  x <- x[, c(setdiff(names(x), "geometry"), "geometry")]
  # return cleaned data
  x
}
