context("wdpa_fetch")

test_that("country name", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  x <- suppressWarnings(wdpa_fetch("Liechtenstein", wait = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("ISO3", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  x <- suppressWarnings(wdpa_fetch("LIE", wait = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("global", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not(pingr::is_online())
  skip_if(mean(pingr::ping("www.google.com", count = 10)) > 10,
          "slow internet connection detected")
  x <- suppressWarnings(wdpa_fetch("global"))
  expect_is(x, "sf")
})
