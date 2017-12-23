context("wdpa_fetch")

# define helper functions
data_not_available <- function(x) {
  inherits(x, "try-error") &&
  attr(x, "condition")$message == paste("data is not yet available for ",
                                        "download at ",
                                        "http://protectedplanet.net")
}

test_that("country name", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  x <- try(wdpa_fetch("Liechtenstein"), silent = TRUE)
  skip_if(data_not_available(x), "LIE data not available online")
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("ISO3", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  x <- try(wdpa_fetch("LIE"), silent = TRUE)
  skip_if(data_not_available(x), "LIE data not available online")
  expect_is(x, "sf")
  expect_true(all(x$ISO3 == "LIE"))
})

test_that("global", {
  skip_on_cran()
  skip_if_not(pingr::is_online())
  skip_if(mean(pingr::ping("www.google.com", count = 10)) > 10,
          "slow internet connection detected")
  x <- wdpa_fetch("global")
  expect_is(x, "sf")
})
