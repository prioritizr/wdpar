# set default availability for phantomJS
is_phantomjs_available <- FALSE
if (interactive() && identical(Sys.getenv("NOT_CRAN"), "true")) {
  is_phantomjs_available <- TRUE
}
