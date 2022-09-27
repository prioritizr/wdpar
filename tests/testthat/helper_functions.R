skip_on_github_workflow <- function(x) {
  testthat::skip_if(
    identical(Sys.getenv("GITHUB_WORKFLOW"), x),
    paste("On", x))
}

standardize_geometry <- function(x) {
  x_crs <- sf::st_crs(x)
  sf::st_crs(x) <- sf::st_crs(NA)
  x <- sf::st_buffer(x, 0)
  sf::st_crs(x) <- x_crs
  x
}
