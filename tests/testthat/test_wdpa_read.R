context("wdpa_read")

find_dl_url <- function(x) {
  # create potential uls
  last_few_months <- format(Sys.Date() - (seq(-2, 10) * 30), "%b%Y")
  potential_urls <- paste0("https://www.protectedplanet.net/downloads/WDPA_",
                           last_few_months, "_", x, "?type=shapefile")
  # try to find working url
  found_url <- FALSE
  for (i in seq_along(potential_urls)) {
   if (!httr::http_error(potential_urls[i])) {
     download_url <- potential_urls[i]
     found_url <- TRUE
     break()
   }
  }
  # if no working urls found, then return null
  if (!found_url)
    return(NULL)
  # otherwise return working url
  return(download_url)
}

test_that("wdpa_read (without point data)", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  # try to find download url
  download_url <- find_dl_url("LIE")
  skip_if(is.null(download_url))
  # path to save file zipfile with data
  path <- tempfile(pattern = "WDPA_", fileext = ".zip")
  # download zipfile
  result <- httr::GET(download_url, httr::write_disk(path))
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
  skip_if_not(pingr::is_online())
  # try to find download url
  download_url <- find_dl_url("MHL")
  skip_if(is.null(download_url))
  # path to save file zipfile with data
  path <- tempfile(pattern = "WDPA_", fileext = ".zip")
  # download zipfile
  result <- httr::GET(download_url, httr::write_disk(path))
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
