context("setup")

# initialize data
test_that("phantomjs", {
  skip_on_cran()
  # check if phantomjs installed and install if needed
  if (!isTRUE(wdpar:::has_phantomjs())) {
    phantomjs::install_phantomjs()
  }
  # update variable
  is_phantomjs_available <<- wdpar:::has_phantomjs()
  # tests
  expect_is(is_phantomjs_available, "logical")
})
