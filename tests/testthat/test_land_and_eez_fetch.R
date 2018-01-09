context("land_and_eez_fetch")

test_that("land_and_eez_fetch (single country with eez)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- land_and_eez_fetch("MHL")
  # run tests
  expect_is(x, "sf")
  expect_equal(names(x), c("ISO3", "TYPE", "geometry"))
  expect_true(all(x$ISO3 == "MHL"))
  expect_equal(nrow(x), 2)
  expect_true(all(x$type %in% c("LAND", "EEZ")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("land_and_eez_fetch (single country without eez)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- land_and_eez_fetch("BDI")
  # run tests
  expect_is(x, "sf")
  expect_equal(names(x), c("ISO3", "TYPE", "geometry"))
  expect_true(all(x$ISO3 == "BDI"))
  expect_equal(nrow(x), 1)
  expect_true(all(x$type %in% c("LAND")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("land_and_eez_fetch (single country with simplification)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- land_and_eez_fetch("MHL", simplify_tolerance = 100)
  # run tests
  expect_is(x, "sf")
  expect_equal(names(x), c("ISO3", "TYPE", "geometry"))
  expect_true(all(x$ISO3 == "MHL"))
  expect_equal(nrow(x), 2)
  expect_true(all(x$type %in% c("LAND", "EEZ")))
  # expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("land_and_eez_fetch (multiple countries)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # fetch data
  x <- land_and_eez_fetch(c("MHL", "BDI"))
  # run tests
  expect_is(x, "sf")
  expect_equal(names(x), c("ISO3", "TYPE", "geometry"))
  expect_true(all(x$ISO3 %in% c("MHL", "BDI")))
  expect_equal(nrow(x), 3)
  expect_true(all(x$type %in% c("LAND", "EEZ")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("land_and_eez_fetch (global)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  skip_if(mean(pingr::ping("www.google.com", count = 10)) > 10,
          "slow internet connection detected")
  # fetch data
  x <- land_and_eez_fetch("global", simplify_tolerance = 1000)
  # run tests
  expect_is(x, "sf")
  expect_equal(names(x), c("ISO3", "TYPE", "geometry"))
  expect_true(all(x$ISO3 %in% countrycode::countrycode_data$iso3c))
  expect_true(all(x$type %in% c("LAND", "EEZ")))
})
