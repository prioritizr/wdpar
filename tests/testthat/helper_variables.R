# set default availability for chrome
is_chrome_installed <- FALSE
if (interactive() && identical(Sys.getenv("NOT_CRAN"), "true")) {
  is_chrome_installed <- TRUE
}
