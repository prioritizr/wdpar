context("wdpa_fetch")

test_that("country name", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- suppressWarnings(wdpa_fetch("Liechtenstein", wait = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("ISO3", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- suppressWarnings(wdpa_fetch("LIE", wait = TRUE, verbose = TRUE))
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("global", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_local_and_slow_internet()
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- suppressWarnings(wdpa_fetch(
    "global", wait = TRUE, n = 5, verbose = TRUE))
  expect_is(x, "sf")
})

test_that("polygon and point data", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_if_local_and_slow_internet()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  x <- suppressWarnings(wdpa_fetch("USA", wait = TRUE))
  expect_is(x, "sf")
  expect_true(any(vapply(
    sf::st_geometry(x), inherits, logical(1), c("POLYGON", "MULTIPOLYGON")
  )))
  expect_true(any(vapply(
    sf::st_geometry(x), inherits, logical(1), c("POINT", "MULTIPOINT")
  )))
})

test_that("cache", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # fetch data
  x <- wdpa_fetch("MHL", wait = TRUE, force = TRUE)
  Sys.sleep(2)
  y <- wdpa_fetch("MHL", wait = TRUE, force = FALSE)
  # run tests
  expect_is(x, "sf")
  expect_equal(x, y)
})
