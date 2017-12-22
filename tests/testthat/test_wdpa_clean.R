context("wdpa_clean")

test_that("wdpa_clean (single country with eez)", {
  skip_on_cran()
  skip("not implemented")
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(wdpa_fetch("NZL"))
  # run tests
  expect_is(x, "sf")
  expect_true(all(c("ISO3", "TYPE", "geometry") %in% names(x)))
  expect_true(all(x$ISO3 == "NZL"))
  expect_true(all(x$type %in% c("LAND", "EEZ")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (single country without eez)", {
  skip_on_cran()
  skip("not implemented")
  skip_if_not(pingr::is_online())
  # fetch data
  x <- wdpa_clean(wdpa_fetch("LSO"))
  # run tests
  expect_is(x, "sf")
  expect_true(all(c("ISO3", "TYPE", "geometry") %in% names(x)))
  expect_true(all(x$ISO3 == "LSO"))
  expect_true(all(x$type %in% c("LAND")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})

test_that("wdpa_clean (global)", {
  skip_on_cran()
  skip("not implemented")
  skip_if_not(pingr::is_online())
  skip_if(mean(pingr::ping("www.google.com", count = 10)) > 10,
          "slow internet connection detected")
  # fetch data
  x <- wdpa_clean(wdpa_fetch("global"))
  # run tests
  expect_is(x, "sf")
  expect_true(all(c("ISO3", "TYPE", "geometry") %in% names(x)))
  expect_true(all(x$ISO3 %in% countrycode::countrycode_data$iso3c))
  expect_true(all(x$type %in% c("LAND", "EEZ")))
  expect_equal(sum(sf::st_overlaps(x, sparse = FALSE)), 0)
})
