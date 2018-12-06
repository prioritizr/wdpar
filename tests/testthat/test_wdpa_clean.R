context("wdpa_clean")

# define data
wdpa_column_names <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME",
                       "DESIG", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT",
                       "INT_CRIT",  "MARINE", "REP_M_AREA", "REP_AREA",
                       "NO_TAKE", "NO_TK_AREA", "STATUS", "STATUS_YR",
                       "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN",
                       "VERIF", "METADATAID", "SUB_LOC", "PARENT_ISO", "ISO3",
                       "GEOMETRY_TYPE", "AREA_KM2", "geometry")

test_that("wdpa_clean (single country with eez)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(suppressWarnings(wdpa_fetch("MHL", wait = TRUE)))
  # run tests
  expect_is(x, "sf")
  expect_true(all(names(x) %in% wdpa_column_names))
  expect_gt(nrow(x), 0)
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (single country without eez)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(suppressWarnings(wdpa_fetch("LIE", wait = TRUE)))
  # run tests
  expect_gt(nrow(x), 0)
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_gt(sum(x$ISO3 == "LIE"), 0)
  expect_equal(sum(x$MARINE == "marine"), 0)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (single country with simplification)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(suppressWarnings(wdpa_fetch("MHL", wait = TRUE)),
                  simplify_tolerance = 100)
  # run tests
  expect_is(x, "sf")
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (single country without overlap removal)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(suppressWarnings(wdpa_fetch("BDI", wait = TRUE)),
                  erase_overlaps = FALSE)
  # run tests
  expect_gt(nrow(x), 0)
  expect_true(all(wdpa_column_names %in% names(x)))
  expect_gt(sum(x$ISO3 == "BDI"), 0)
  expect_equal(sum(x$MARINE == "marine"), 0)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_gt(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})
