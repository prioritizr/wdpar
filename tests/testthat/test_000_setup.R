context("setup")

# initialize data
test_that("initialize_chrome", {
  skip_on_cran()
  # check if chrome installed and install if needed
  if (!isTRUE(wdpar:::is_chrome_available())) {
    webdriver::install_phantomjs()
  }
  # update variable
  is_chrome_installed <<- wdpar:::is_chrome_available()
  # tests
  expect_is(is_chrome_installed, "logical")
})
