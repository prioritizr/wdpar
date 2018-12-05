.onLoad <- function(libname, pkgname) {
  # import the codelist dataset from countrycode package
  utils::data("codelist", package = "countrycode")
  codelist <- codelist
  # return success
  invisible()
}
