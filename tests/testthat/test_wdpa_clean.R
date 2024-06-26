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

default_retain_status <- c("Designated", "Inscribed", "Established")

test_that("single country with eez", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(
      wdpa_fetch("MHL", wait = TRUE, verbose = TRUE, check_version = FALSE)
    )
  )
  # run tests
  expect_is(x, "sf")
  expect_true(all(names(x) %in% wdpa_column_names))
  expect_gt(nrow(x), 0)
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_true(all(x$STATUS %in% default_retain_status))
  expect_true(all(x$PA_DEF %in% c("PA", "OECM")))
})

test_that("single country without eez", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("LIE", wait = TRUE, check_version = FALSE)),
    verbose = TRUE, geometry_precision = 1000
  )
  # run tests
  expect_gt(nrow(x), 0)
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_gt(sum(x$ISO3 == "LIE"), 0)
  expect_equal(sum(x$MARINE == "marine"), 0)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_true(all(x$STATUS %in% default_retain_status))
})

test_that("single country with simplification", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("MHL", wait = TRUE, check_version = FALSE)),
    simplify_tolerance = 100
  )
  # run tests
  expect_is(x, "sf")
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_true(all(x$STATUS %in% default_retain_status))
})

test_that("single country without overlap removal", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("BDI", wait = TRUE, check_version = FALSE)),
    erase_overlaps = FALSE
  )
  # run tests
  expect_gt(nrow(x), 0)
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_gt(sum(x$ISO3 == "BDI"), 0)
  expect_equal(sum(x$MARINE == "marine"), 0)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_gt(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_true(all(x$STATUS %in% default_retain_status))
})

test_that("country with MULTIPOINT protected areas", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_fetch("BOL", wait = TRUE, check_version = FALSE)
  x_points <- vapply(sf::st_geometry(x), inherits, logical(1),
                     c("POINT", "MULTIPOINT"))
  x <- x[c(which(x_points), 15), , drop = FALSE]
  y <- suppressWarnings(wdpa_clean(x, erase_overlaps = FALSE))
  expect_gt(nrow(x), 0)
  expect_true(all(y$STATUS %in% default_retain_status))
})

test_that("country with MULTIPOLYGON protected area", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_fetch("BOL", wait = TRUE, check_version = FALSE)
  y <- suppressWarnings(wdpa_clean(x, erase_overlaps = FALSE,
                                   geometry_precision = 10000))
  p1 <- x[x$WDPAID == 555592679, ]
  p2 <- y[y$WDPAID == 555592679, ]
  # test that polygons in multipolygon features are retained
  expect_gt(length(sf::st_cast(sf::st_geometry(p1), "POLYGON")), 1)
  expect_gt(length(sf::st_cast(sf::st_geometry(p2), "POLYGON")), 1)
  expect_true(all(y$STATUS %in% default_retain_status))
})

test_that("country with super invalid MULTIPOLYGON data", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- wdpa_fetch("GAB", wait = TRUE, check_version = FALSE)
  y <- suppressWarnings(wdpa_clean(x, erase_overlaps = TRUE))
  expect_is(y, "sf")
  expect_true(all(y$STATUS %in% default_retain_status))
})

test_that("geometries in non-geometry column", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- wdpa_fetch("GAB", wait = TRUE, check_version = FALSE)
  geom_col <- attr(x, "sf_column")
  attr(x, "sf_column") <- "shape"
  names(x)[names(x) == geom_col] <- "shape"
  y <- suppressWarnings(wdpa_clean(x, erase_overlaps = TRUE))
  expect_is(y, "sf")
  expect_equal(attr(y, "sf_column"), "geometry")
  expect_true(all(y$STATUS %in% default_retain_status))
})

test_that("single country with no valid non-empty geometries", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("SOM", wait = TRUE, check_version = FALSE))
  )
  y <- wdpa_clean(wdpa_fetch("MHL", wait = TRUE))
  crs <- paste("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0",
    "+y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
  expect_identical(x, empty_wdpa_dataset(st_crs(crs)))
  expect_equal(names(x), wdpa_column_names)
  expect_equal(ncol(x), ncol(y))
  expect_true(all(sapply(seq_along(x), function(i) {
    class_x <- class(x[[i]])[1]
    class_y <- class(y[[i]])[1]
    (class_x == class_y) ||
    (startsWith(class_x, "sf") && startsWith(class_y, "sf"))
  })))
  expect_true(all(y$STATUS %in% default_retain_status))
})

test_that("retain UNESCO Biosphere reserves", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("MHL", wait = TRUE, check_version = FALSE)),
    exclude_unesco = FALSE
  )
  # run tests
  expect_is(x, "sf")
  expect_true(all(names(x) %in% wdpa_column_names))
  expect_gt(nrow(x), 0)
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_true(all(x$STATUS %in% default_retain_status))
})

test_that("protected areas that turn into long rectangles without prepr", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_if_not_installed("prepr")
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- suppressWarnings(wdpa_fetch("USA", wait = TRUE, check_version = FALSE))
  x <- x[x$MARINE == "2", , drop = FALSE]
  y <- wdpa_clean(x, erase_overlaps = FALSE)
  # run tests
  expect_is(y, "sf")
  expect_lte(as.numeric(max(sf::st_area(y))), 1e+13)
})

test_that("protected areas that massively increase in size without prepr", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_if_not_installed("prepr")
  skip_if_not_installed("dplyr")
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  ids <- c(23177, 12352, 555705343, 555705341, 555721495)
  x <- wdpa_fetch("DZA", wait = TRUE, check_version = FALSE, datatype = "gdb")
  x <- x[x$WDPAID %in% ids, , drop = FALSE]
  # clean data
  y <- wdpa_clean(x, erase_overlaps = FALSE)
  # run tests
  expect_is(y, "sf")
})

test_that("custom retain_status", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_clean(
    suppressWarnings(wdpa_fetch("MHL", wait = TRUE, check_version = FALSE)),
    retain_status = c("Designated", "Proposed")
  )
  # run tests
  expect_is(x, "sf")
  expect_true(all(names(x) %in% wdpa_column_names))
  expect_gt(nrow(x), 0)
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
  expect_identical(sort(unique(x$STATUS)), sort(c("Designated", "Proposed")))
})

test_that("NULL retain_status", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- suppressWarnings(wdpa_fetch("MHL", wait = TRUE, check_version = FALSE))
  y <- wdpa_clean(x, retain_status = NULL, erase_overlaps = TRUE)
  # run tests
  expect_is(y, "sf")
  expect_true(all(names(y) %in% wdpa_column_names))
  expect_gt(nrow(y), 0)
  expect_true(all(y$ISO3 == "MHL"))
  expect_gt(sum(y$MARINE == "partial"), 0)
  expect_gt(sum(y$MARINE == "marine"), 0)
  expect_equal(sum(is.na(y$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(y, sparse = FALSE)), 0)
  expect_identical(sort(unique(x$STATUS)), sort(unique(y$STATUS)))
})

test_that("empty intersections", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- suppressWarnings(wdpa_fetch("FRA", wait = TRUE, check_version = FALSE))
  x <- x[seq_len(500), , drop = FALSE]
  y <- wdpa_clean(x, retain_status = NULL, erase_overlaps = TRUE)
  # run tests
  expect_is(y, "sf")
  expect_true(all(names(y) %in% wdpa_column_names))
  expect_gt(nrow(y), 0)
})


test_that("shp and gdb produce same results", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch and clean data
  x <- wdpa_clean(
    suppressWarnings(
      wdpa_fetch(
        "MHL", wait = TRUE, verbose = TRUE, datatype = "shp",
        check_version = FALSE, force = TRUE)
    )
  )
  y <- wdpa_clean(
    suppressWarnings(
      wdpa_fetch(
        "MHL", wait = TRUE, verbose = TRUE, datatype = "gdb",
        check_version = FALSE, force = TRUE)
    )
  )
  # sort data
  x <- x[order(x$WDPAID), , drop = FALSE]
  y <- y[order(y$WDPAID), , drop = FALSE]
  # run tests
  expect_equal(x, y)
})
