skip_if_local_and_slow_internet <- function(x) {
  testthat::skip_if_not_installed("pingr")
  x <- (mean(pingr::ping("www.google.com", count = 10)) > 10) &&
       !identical(Sys.getenv("CI"), "true")
  testthat::skip_if(x, paste("On local and slow internet"))
}

skip_if_phantomjs_not_available <- function() {
  testthat::skip_if_not(
    is_phantomjs_available, "phantomJS not available"
  )
}
