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
  skip_if_not_installed("pingr")
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("Mac OSX")
  skip_if(
    (mean(pingr::ping("www.google.com", count = 10)) > 10) &&
    !identical(Sys.getenv("CI"), "true"))
  x <- suppressWarnings(wdpa_fetch(
    "global", force = TRUE, wait = TRUE, n = 5, verbose = TRUE))
  expect_is(x, "sf")
})
