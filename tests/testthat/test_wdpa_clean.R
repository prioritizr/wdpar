context("wdpa_clean")

# define helper functions
data_not_available <- function(x) {
  inherits(x, "try-error") &&
  attr(x, "condition")$message == paste("data is not yet available for ",
                                        "download at ",
                                        "http://protectedplanet.net")
}

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
  x <- wdpa_fetch("MHL")
  skip_if(data_not_available(x), "MHL data not available online")
  x <- wdpa_clean(x)
  # run tests
  expect_is(x, "sf")
  expect_equal(wdpa_column_names, names(x))
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
  x <- try(wdpa_fetch("BDI"), silent = TRUE)
  skip_if(data_not_available(x), "BDI data not available online")
  x <- wdpa_clean(x)
  # run tests
  expect_gt(nrow(x), 0)
  expect_equal(wdpa_column_names, names(x))
  expect_gt(sum(x$ISO3 == "BDI"), 0)
  expect_equal(sum(x$MARINE == "marine"), 0)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (single country with tolerance)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- try(wdpa_fetch("MHL"), silent = TRUE)
  skip_if(data_not_available(x), "MHL data not available online")
  x <- wdpa_clean(x, tolerance = 100)
  # run tests
  expect_is(x, "sf")
  expect_equal(wdpa_column_names, names(x))
  expect_gt(nrow(x), 0)
  expect_true(all(x$ISO3 == "MHL"))
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (global)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  skip_if(mean(pingr::ping("www.google.com", count = 10)) > 10,
          "slow internet connection detected")
  # fetch data
  x <- wdpa_clean(wdpa_fetch("global"))
  # run tests
  expect_gt(nrow(x), 0)
  expect_equal(wdpa_column_names, names(x))
  expect_gt(length(unique(x$ISO3)), 10)
  expect_gt(sum(x$MARINE == "terrestrial"), 0)
  expect_gt(sum(x$MARINE == "partial"), 0)
  expect_gt(sum(x$MARINE == "marine"), 0)
  expect_equal(sum(is.na(x$AREA_KM2)), 0)
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})
