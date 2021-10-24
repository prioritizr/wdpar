context("wdpa_fetch")

test_that("country name", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_on_github_workflow("Windows")
  x <- suppressWarnings(wdpa_fetch("Liechtenstein", force = TRUE, wait = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("ISO3", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_on_github_workflow("Windows")
  x <- suppressWarnings(wdpa_fetch(
    "LIE", force = TRUE, wait = TRUE, verbose = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("global", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_local_and_slow_internet()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("Mac OSX")
  x <- suppressWarnings(wdpa_fetch(
    "global", force = TRUE, wait = TRUE, n = 5, verbose = TRUE))
  expect_is(x, "sf")
})

test_that("cache", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_on_github_workflow("Windows")
  # fetch data
  x <- wdpa_fetch("MHL", wait = TRUE, force = TRUE)
  y <- wdpa_fetch("MHL", wait = TRUE, force = FALSE)
  # run tests
  expect_is(x, "sf")
  expect_equal(x, y)
})
