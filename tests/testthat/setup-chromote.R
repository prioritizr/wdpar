# setup for testing with chromote
# https://github.com/rstudio/shinytest2/blob/main/tests/testthat/setup-disable-crashpad.R

# note that we only use this if not on CRAN
if (!isTRUE(is_on_cran())) {

  # #disable crash reporting
  chromote::set_chrome_args(c(
    "--disable-crash-reporter",
    chromote::default_chrome_args()
  ))

  ## make sure the temp folder is removed when testing is complete
  withr::defer({
    ### Close the browser
    try(chromote::default_chromote_object()$get_browser()$close())

    ### clean up chromote sessions
    gc()
    Sys.sleep(2)

    ### delete the Crashpad folder if it exists
    unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
  }, envir = testthat::teardown_env())

}
