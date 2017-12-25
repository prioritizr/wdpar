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
  # validate argument
  assertthat::assert_that(assertthat::is.string(x))
  # processing
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

#' Parallel sf operation
#'
#' Execute an \code{\link[sf]{sf}} operation in parallel.
#'
#' @param x object of class \code{sfc}, \code{sfg} or \code{sf}.
#'
#' @param fun function to execute.
#'
#' @param args \code{\link[base]{list}} with additional arguments.
#'
#' @param remove_empty \code{\link[base]{logical}} should empty geometries
#'   generated during processing be removed?
#'
#' @param threads \code{integer} number of cores for processing data.
#'
#' @return Object with processing executed.
#'
#' @noRd
parallel_sf_operation <- function(x, fun, args = list(), remove_empty = FALSE,
                                    threads = 1) {
  # validate arguments
  assertthat::assert_that(inherits(x, c("sf", "sfc", "sfg")),
                          inherits(fun, "function"),
                          is.list(args), assertthat::is.count(threads),
                          assertthat::is.flag(remove_empty),
                          isTRUE(threads <= parallel::detectCores(TRUE)))
  # initialize cluster
  if (threads > 1) {
    cl <- parallel::makeCluster(threads, "PSOCK")
    parallel::clusterEvalQ(cl, library(sf))
    parallel::clusterExport(cl, c("x", "fun", "args"), envir = environment())
    doParallel::registerDoParallel(cl)
  }
  # perform processing
  result <- plyr::llply(distribute_load(ifelse(inherits(x, "sf"), nrow(x),
                                               length(x)), threads),
                        .parallel = threads > 1,
                        function(i) {
                          # perform processing
                          if (inherits(x, "sf")) {
                            out <- do.call(fun, append(list(x = x[i, ]), args))
                          } else {
                            out <- do.call(fun, append(list(x = x[i]), args))
                          }
                          # update idx if needed
                          if (!is.null(attr(out, "idx"))) {
                            idx <- attr(out, "idx")
                            idx[, 1] <- i[idx[, 1]]
                            attr(out, "idx") <- idx
                          }
                          # return result
                          return(out)
                        })
  # cleanup
  if (threads > 1) {
    cl <- parallel::stopCluster(cl)
  }
  # post-processing
  agrx <- attr(result[[1]], "agr")
  rnx_is_character <- is.character(attr(result[[1]], "row.names"))
  if (inherits(x, "sf")) {
    ## merge results
    if (length(result) > 1) {
      result <- do.call(rbind, result)
    } else {
      result <- result[[1]]
    }
    ## reorder features
    result <- result[order(nchar(attr(result, "row.names")),
                           stringi::stri_reverse(attr(result, "row.names"))), ]
    ## remove empty geometries
    if (remove_empty) {
      ## remove empty geometries
      geom_length <- vapply(result[[attr(result, "sf_column")]], length,
                            integer(1))
      result <- result[geom_length > 0, ]
    }
  } else {
    ## extract idx
    idx <- NULL
    orderx <- NULL
    if (!is.null(attr(result[[1]], "idx"))) {
      if (length(result) > 1) {
        idx <- do.call(rbind, lapply(result, attr, "idx"))
        orderx <- order(idx[, 2], idx[, 1])
        idx <- idx[orderx, ]
      } else {
        idx <- attr(result[[1]], "idx")
      }
    }
    ## merge results
    result2 <- result
    result <- result[[1]]
    if (length(result2) > 1) {
      for (i in seq_along(result2)[-1])
        result <- append(result, result2[[i]])
    }
    ## reorder results
    if (!is.null(orderx))
      result <- result[orderx]
    ## remove empty geometries
    if (remove_empty) {
      geom_length <- vapply(result, length, integer(1))
      result <- result[geom_length > 0]
    }
    ## update attributes
    attr(result, "row.names") <- NULL
    if (!is.null(idx))
      attr(result, "idx") <- idx
  }
  ## update attributes
  attr(result, "agr") <- agrx
  # return result
  return(result)
}

#' Find most recent version of WDPA data set in folder
#'
#' Find the file in a folder which has the most recent version of the WDPA data #' set in it.
#'
#'
#' @param x \code{character} Country for desired data. This argument
#'   can be the name of the country (e.g. \code{"Liechtenstein"}) or the
#'   ISO-3 code for the country (e.g. \code{"LIE"}). This argument can also
#'   be set to \code{"global"} to obtain file path for the global data set.
#'
#' @param download_dir \code{character} folder path to download the data.
#'  Defaults to a persistent data directory
#'  (\code{rappdirs::user_data_dir("wdpar")}).
#'
#' @return \code{character} file path.
#'
#' @noRd
wdpa_file <- function(x, download_dir) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(x),
                          assertthat::is.dir(download_dir))
  # convert x to country ISO3 code
  if (x != "global") {
    x <- country_code(x)
  }
  # set file and date search patterns
  if (x == "global") {
    file_pattern <- "WDPA\\_.*\\_Public.zip"
  } else {
    file_pattern <- paste0("WDPA\\_.*\\_", x, "\\-shapefile.zip")
  }
  ## search for file
  file_paths <- dir(download_dir, file_pattern, full.names = TRUE)
  if (length(file_paths) == 0)
    stop("data not found in \"download_dir\" folder")
  # parse date-times
  # note that date-times easily cannot be parsed using base::strptime due to
  # bug where date-times with month before year (e.g. Nov2011) will return NA
  # see this post on Stack Overflow
  # https://stackoverflow.com/questions/26997864/strptime-not-recognizing-b-b
  # therefore we must---unsatisfyingly---extract the month and years seperately
  # reorder them, and then parse using base::strptime
  month_year <- vapply(strsplit(basename(file_paths), "_", fixed = TRUE), `[[`,
                       character(1), 2)
  months <- gsub("[[:digit:]]", "", month_year)
  years <- gsub("[[:alpha:]]", "", month_year)
  file_dates <- as.POSIXct(strptime(paste0("01/", months, "/", years),
                           "%d/%b/%Y"))
  # set file_path as latest version
  file_path <- file_paths[which.max(file_dates)]
  # return file path
  return(file_path)
}
