context("wdpa_clean")

# define data
wdpa_column_names <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME",
                       "DESIG", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT",
                       "INT_CRIT",  "MARINE", "REP_M_AREA", "REP_AREA",
                       "NO_TAKE", "NO_TK_AREA", "STATUS", "STATUS_YR",
                       "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN",
                       "VERIF", "METADATAID", "SUB_LOC", "PARENT_ISO", "ISO3",
                       "SUPP_INFO", "CONS_OBJ", "GEOMETRY_TYPE", "AREA_KM2",
                       "geometry")

# test_that("wdpa_clean (single country with eez)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_clean(suppressWarnings(wdpa_fetch("MHL", wait = TRUE,
#                                               force = TRUE)))
#   # run tests
#   expect_is(x, "sf")
#   expect_true(all(names(x) %in% wdpa_column_names))
#   expect_gt(nrow(x), 0)
#   expect_true(all(x$ISO3 == "MHL"))
#   expect_gt(sum(x$MARINE == "partial"), 0)
#   expect_gt(sum(x$MARINE == "marine"), 0)
#   expect_equal(sum(is.na(x$AREA_KM2)), 0)
#   expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
# })
#
# test_that("wdpa_clean (single country without eez)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_clean(suppressWarnings(wdpa_fetch("LIE", wait = TRUE,
#                                               force = TRUE)),
#                   geometry_precision = 1000)
#   # run tests
#   expect_gt(nrow(x), 0)
#   expect_true(all(wdpa_column_names %in% names(x)))
#   expect_gt(sum(x$ISO3 == "LIE"), 0)
#   expect_equal(sum(x$MARINE == "marine"), 0)
#   expect_gt(sum(x$MARINE == "terrestrial"), 0)
#   expect_equal(sum(is.na(x$AREA_KM2)), 0)
#   expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
# })
#
# test_that("wdpa_clean (single country with simplification)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_clean(suppressWarnings(wdpa_fetch("MHL", force = TRUE,
#                                               wait = TRUE)),
#                   simplify_tolerance = 100)
#   # run tests
#   expect_is(x, "sf")
#   expect_true(all(wdpa_column_names %in% names(x)))
#   expect_true(all(x$ISO3 == "MHL"))
#   expect_gt(sum(x$MARINE == "partial"), 0)
#   expect_gt(sum(x$MARINE == "marine"), 0)
#   expect_equal(sum(is.na(x$AREA_KM2)), 0)
#   expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
# })
#
# test_that("wdpa_clean (single country without overlap removal)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_clean(suppressWarnings(wdpa_fetch("BDI", wait = TRUE,
#                                               force = TRUE)),
#                   erase_overlaps = FALSE)
#   # run tests
#   expect_gt(nrow(x), 0)
#   expect_true(all(wdpa_column_names %in% names(x)))
#   expect_gt(sum(x$ISO3 == "BDI"), 0)
#   expect_equal(sum(x$MARINE == "marine"), 0)
#   expect_gt(sum(x$MARINE == "terrestrial"), 0)
#   expect_equal(sum(is.na(x$AREA_KM2)), 0)
#   expect_gt(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
# })
#
# test_that("wdpa_clean (country with MULTIPOINT protected areas)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_fetch("BOL", wait = TRUE, force = TRUE)
#   x_points <- vapply(x$geometry, inherits, logical(1),
#                      c("POINT", "MULTIPOINT"))
#   x <- x[c(which(x_points), 15), , drop = FALSE]
#   y <- suppressWarnings(wdpa_clean(x, erase_overlaps = FALSE))
#   expect_gt(nrow(x), 0)
# })
#
# test_that("wdpa_clean (country with MULTIPOLYGON protected area)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   # fetch data
#   x <- wdpa_fetch("BOL", wait = TRUE, force = TRUE)
#   y <- suppressWarnings(wdpa_clean(x, erase_overlaps = FALSE,
#                                    geometry_precision = 10000))
#   p1 <- x[x$WDPAID == 555592679, ]
#   p2 <- y[y$WDPAID == 555592679, ]
#   # test that polygons in multipolygon features are retained
#   expect_gt(length(sf::st_cast(p1$geometry, "POLYGON")), 1)
#   expect_gt(length(sf::st_cast(p2$geometry, "POLYGON")), 1)
# })
#
# test_that("wdpa_clean (country with super invalid MULTIPOLYGON data)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   x <- wdpa_fetch("GAB", wait = TRUE, force = TRUE)
#   y <- suppressWarnings(wdpa_clean(x, erase_overlaps = TRUE))
#   expect_is(y, "sf")
# })
#
# test_that("wdpa_clean (geometries in non-geometry column)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   x <- wdpa_fetch("GAB", wait = TRUE, force = TRUE)
#   geom_col <- attr(x, "sf_column")
#   attr(x, "sf_column") <- "shape"
#   names(x)[names(x) == geom_col] <- "shape"
#   y <- suppressWarnings(wdpa_clean(x, erase_overlaps = TRUE))
#   expect_is(y, "sf")
#   expect_equal(attr(y, "sf_column"), "geometry")
# })
#
# test_that("wdpa_clean (single country with no valid non-empty geometries)", {
#   skip_on_cran()
#   skip_if_not(curl::has_internet())
#   x <-
#     suppressWarnings(wdpa_clean(wdpa_fetch("SOM", wait = TRUE, force = TRUE)))
#   y <- wdpa_clean(wdpa_fetch("MHL", wait = TRUE, force = TRUE))
#   crs <- paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0",
#     "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
#   expect_identical(x, empty_wdpa_dataset(st_crs(crs)))
#   expect_equal(names(x), wdpa_column_names)
#   expect_equal(ncol(x), ncol(y))
#   expect_true(all(sapply(seq_along(x), function(i) {
#     class_x <- class(x[[i]])[1]
#     class_y <- class(y[[i]])[1]
#     (class_x == class_y) ||
#     (startsWith(class_x, "sf") && startsWith(class_y, "sf"))
#   })))
# })
