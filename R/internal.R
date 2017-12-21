#' Distribute load
#'
#' Utility function for distributing computations among a pool of workers
#' for parallel processing.
#'
#' @param x \code{integer} number of item to process.
#'
#' @param n \code{integer} number of threads.
#'
#' @details This function returns a \code{list} containing an element for
#'   each worker. Each element contains a \code{integer} \code{vector}
#'   specifying the indices that the worker should process.
#'
#' @return \code{list} object.
#'
#' @seealso \code{\link{get_number_of_threads}},
#'   \code{\link{set_number_of_threads}}, \code{\link{is.parallel}}.
#'
#' @examples
#'
#' # imagine that we have 10 jobs that need processing. For simplicity,
#' # our jobs will involve adding 1 to each element in 1:10.
#' values <- 1:10
#'
#' # we could complete this processing using the following vectorized code
#' result <- 1 + 1:10
#' print(result)
#'
#' # however, if our jobs were complex then we would be better off using
#' # functionals
#' result <- lapply(1:10, function(x) x + 1)
#' print(result)
#'
#' # we could do one better, and use the "plyr" package to handle the
#' # processing
#' result <- plyr::llply(1:10, function(x) x + 1)
#' print(result)
#'
#' # we could also use the parallel processing options available through "plyr"
#' # to use more computation resources to complete the jobs (note that since
#' # these jobs are very quick to process this is actually slower).
#' cl <- parallel::makeCluster(2, "PSOCK")
#' doParallel::registerDoParallel(cl)
#' result <- plyr::llply(1:10, function(x) x + 1, .parallel = TRUE)
#' cl <- parallel::stopCluster(cl)
#' print(result)
#'
#' # however this approach iterates over each element individually, we could
#' # use the distribute_load function to split the N jobs up into K super
#' # jobs, and evaluate each super job using vectorized code.
#' x <- 1:10
#' cl <- parallel::makeCluster(2, "PSOCK")
#' parallel::clusterExport(cl, 'x', envir = environment())
#' doParallel::registerDoParallel(cl)
#' l <- distribute_load(length(x), n = 2)
#' result <- plyr::llply(l, function(i) x[i] + 1, .parallel = TRUE)
#' cl <- parallel::stopCluster(cl)
#' print(result)
#'
#' @noRd
distribute_load <- function(x, n) {
  assertthat::assert_that(assertthat::is.count(x),
                          assertthat::is.count(n),
                          isTRUE(x > 0),
                          isTRUE(n > 0))
  if (n == 1) {
    i <- list(seq_len(x))
  } else if (x <= n) {
    i <- as.list(seq_len(x))
  } else {
    j <- as.integer(floor(seq(1, n + 1, length.out = x + 1)))
    i <- list()
    for (k in seq_len(n)) {
        i[[k]] <- which(j == k)
    }
  }
  i
}

#' Country code
#'
#' Find ISO-3 country code for a country.
#'
#' @param x \code{character} name of country or its ISO-3 code.
#'
#' @return \code{character} ISO-3 country code.
#'
#' @seealso \code{\link[countrycode]{countrycode}}.
#'
#' @noRd
country_code <- function(x) {
  if (nchar(x) == 3) {
    # check that x is valid ISO-3 code
    name <- suppressWarnings(countrycode::countrycode(x, "iso3c",
                                                      "country.name.en"))
    if (is.na(name[[1]]))
      stop("argument to x is not a valid iso3 code")
    country_code <- toupper(x)
  } else {
    # check that x is valid country name
    country_code <- suppressWarnings(countrycode::countrycode(x,
                      "country.name.en", "iso3c"))
    if (is.na(country_code[[1]]))
      stop("argument to x is not a valid country name")
    country_code <- toupper(country_code)
  }
  return(country_code)
}
