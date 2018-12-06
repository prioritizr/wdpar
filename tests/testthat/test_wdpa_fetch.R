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
