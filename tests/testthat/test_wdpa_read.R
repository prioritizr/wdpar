context("wdpa_read")

test_that("wdpa_read (without point data)", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_on_github_workflow("Windows")
  # try to find download url
  download_url <- wdpa_url("LIE", wait = TRUE)
  # path to save file zipfile with data
  path <- tempfile(pattern = "WDPA_", fileext = ".zip")
  # download zipfile
  result <- utils::download.file(download_url, path)
  # load data
  x <- wdpa_read(path)
  # tests
  expect_is(x, "sf")
  expect_true(nrow(x) > 0)
  expect_true(all(c("ISO3", "STATUS", "DESIG_ENG", "REP_AREA", "MARINE") %in%
                  names(x)))
})

test_that("wdpa_read (with point data)", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_on_github_workflow("Windows")
  # try to find download url
  download_url <- wdpa_url("MHL", wait = TRUE)
  # path to save file zipfile with data
  path <- tempfile(pattern = "WDPA_", fileext = ".zip")
  # download zipfile
  result <- utils::download.file(download_url, path)
  # load data
  x <- wdpa_read(path)
  # tests
  expect_is(x, "sf")
  expect_true(nrow(x) > 0)
  expect_true(all(c("ISO3", "STATUS", "DESIG_ENG", "REP_AREA", "MARINE") %in%
                  names(x)))
  is_point <- vapply(sf::st_geometry(x), inherits, logical(1),  "POINT") |
              vapply(sf::st_geometry(x), inherits, logical(1),  "MULTIPOINT")
  expect_gt(sum(is_point), 0)
  expect_gt(sum(!is_point), 0)
})
