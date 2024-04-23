context("setup")

# initialize data
test_that("initialize_chrome", {
  skip_on_cran()
  # check if chrome is installed
  is_chrome_installed <<- wdpar:::is_chrome_available()
  # tests
  expect_is(is_chrome_installed, "logical")
})
