skip_on_github_workflow <- function(x) {
  testthat::skip_if(
    identical(Sys.getenv("GITHUB_WORKFLOW"), x),
    paste("On", x))
}
