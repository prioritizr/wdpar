context("internal")

test_that("country_code", {
  expect_equal("LIE", country_code("LIE"))
  expect_equal("LIE", country_code("Liechtenstein"))
  expect_error(country_code("Does not exist"))
  expect_error(country_code("ZZZ"))
})

test_that("wdpa_file (global - same year different month)", {
  # create temporary files
  td <- file.path(tempdir(), basename(tempfile()))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  w <- file.path(td, "WDPA_Nov2018_NZL-shapefile.zip")
  x <- file.path(td, "WDPA_Aug2017_Public.zip")
  y <- file.path(td, "WDPA_Nov2017_Public.zip")
  # run test
  cat("TEST", file = w)
  cat("TEST", file = x)
  cat("TEST", file = y)
  expect_equal(wdpa_file("global", td), y)
  # cleanup
  unlink(td)
})

test_that("wdpa_file (global - different year same month)", {
  # create temporary files
  td <- file.path(tempdir(), basename(tempfile()))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  w <- file.path(td, "WDPA_Nov2018_NZL-shapefile.zip")
  x <- file.path(td, "WDPA_Nov2016_Public.zip")
  y <- file.path(td, "WDPA_Nov2017_Public.zip")
  cat("TEST", file = w)
  cat("TEST", file = x)
  cat("TEST", file = y)
  # run test
  expect_equal(wdpa_file("global", td), y)
  # cleanup
  unlink(td)
})

test_that("wdpa_file (country - same year different month)", {
  # create temporary files
  td <- file.path(tempdir(), basename(tempfile()))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  w <- file.path(td, "WDPA_Dec2018_Public.zip")
  x <- file.path(td, "WDPA_Nov2017_NZL-shapefile.zip")
  y <- file.path(td, "WDPA_Dec2017_NZL-shapefile.zip")
  cat("TEST", file = x)
  cat("TEST", file = y)
  cat("TEST", file = w)
  # run test
  expect_equal(wdpa_file("NZL", td), y)
  # cleanup
  unlink(td)
})

test_that("wdpa_file (country - different year same month)", {
  # create temporary files
  td <- file.path(tempdir(), basename(tempfile()))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  w <- file.path(td, "WDPA_Nov2018_Public.zip")
  x <- file.path(td, "WDPA_Nov2016_NZL-shapefile.zip")
  y <- file.path(td, "WDPA_Nov2017_NZL-shapefile.zip")
  cat("TEST", file = x)
  cat("TEST", file = x)
  cat("TEST", file = y)
  # run test
  expect_equal(wdpa_file("NZL", td), y)
  # cleanup
  unlink(td)
})

test_that("wdpa_url (country)", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # verify that wdpa_url yields a result
  x <- suppressWarnings(wdpa_url("MLT", wait = TRUE))
  expect_is(x, "character")
  # verify that downloading the url yields a zipped shapefile
  f1 <- tempfile(fileext = ".zip")
  f2 <- file.path(tempdir(), basename(tempfile()))
  download_file(x, f1)
  expect_true(file.exists(f1))
  unzip(f1, exdir = f2)
  zip_path <- dir(f2, "^.*\\.zip$", recursive = TRUE, full.names = TRUE)
  if (length(zip_path) > 0)
    Map(utils::unzip, zip_path,
        exdir = gsub(".zip", "", zip_path, fixed = TRUE))
  expect_gt(length(dir(f2, "^.*\\.shp$", recursive = TRUE)), 0)
  unlink(f1, recursive = TRUE, force = TRUE)
  unlink(f2, recursive = TRUE, force = TRUE)
})

test_that("wdpa_url (global)", {
  skip_on_cran()
  skip_if_not(curl::has_internet())
  skip_if_chrome_not_available()
  skip_on_github_workflow("Windows")
  skip_on_github_workflow("macOS")
  # verify that wdpa_url yields a result
  x <- suppressWarnings(wdpa_url("global", wait = TRUE))
  expect_is(x, "character")
  # skip if on local computer with slow internet
  skip_if_local_and_slow_internet()
  # verify that downloading the url yields a zipped shapefile
  f1 <- tempfile(fileext = ".zip")
  f2 <- file.path(tempdir(), basename(tempfile()))
  download_file(x, f1)
  expect_true(file.exists(f1))
  unzip(f1, exdir = f2)
  expect_gt(length(dir(f2, "^.*\\.gdb$", include.dirs = TRUE,
                       recursive = TRUE)), 0)
  unlink(f1, recursive = TRUE, force = TRUE)
  unlink(f2, recursive = TRUE, force = TRUE)
})

test_that("citation", {
  expect_is(citation("wdpar"), "citation")
})

test_that("wdpa_version", {
  expect_equal(wdpa_version("WDPA_Jan2018_LIE-shapefile.zip"), "Jan2018")
})

test_that("convert_wdpa_version_to_POSIXct", {
  expect_equal(convert_wdpa_version_to_POSIXct("Feb2018"),
               as.POSIXct(strptime(paste0("01/Feb/2018"), "%d/%b/%Y")))
  expect_error(convert_wdpa_version_to_POSIXct("asdf2018"))
})

test_that("read_sf_n (n = NULL)", {
  path <- system.file("shape/nc.shp", package = "sf")
  x <- sf::read_sf(path)
  y <- read_sf_n(path)
  expect_identical(x, y)
})

test_that("read_sf_n (n = 5)", {
  path <- system.file("shape/nc.shp", package = "sf")
  x <- sf::read_sf(path)[seq_len(5), ]
  y <- read_sf_n(path, n = 5)
  expect_identical(x, y)
})
