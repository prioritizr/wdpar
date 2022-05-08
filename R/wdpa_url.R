#' @include internal.R
NULL

#' Download URL
#'
#' Obtain a URL to download data from
#' [Protected Planet](https://www.protectedplanet.net/en).
#' Specifically, the URL provides access to data available through
#' the World Database on Protected Areas
#' (WDPA) and the World Database on Other Effective Area-Based Conservation
#' Measures (WDOECM).
#' **Note that data are accessed assuming non-commercial use.**
#'
#' @param x `character` country for desired data. This argument
#'   can be the name of the country (e.g. `"Liechtenstein"`) or the
#'   ISO-3 code for the country (e.g. `"LIE"`). This argument can also
#'   be set to `"global"` to obtain the URL for the global dataset.
#'
#' @param wait `logical` if data is not immediately available for download
#'   should the session be paused until it is ready for download? If argument
#'   to `wait` is `FALSE` and the data is not ready then an error
#'   will be thrown. Defaults to `FALSE`.
#'
#' @param page_wait `numeric` number of seconds to wait for web pages
#'   to load when finding the download URL on
#'   [Protected Planet](https://www.protectedplanet.net/en).
#'   Defaults to 2.
#'   Since the process of finding a download URL requires
#'   navigating through multiple web pages,
#'   the default argument means that the function will take at least 8
#'   seconds to complete.
#'   Users on slow internet connections may experience issues
#'   with the default argument (e.g. resulting in an error
#'   containing the message `Error: Summary: NoSuchElement`).
#'   To avoid this, users can try specifying a greater value (e.g. 5 seconds).
#'
#' @return `character` URL to download the data.
#'
#' @seealso [wdpa_fetch()], [countrycode::countrycode()].
#'
#' @examples
#' \dontrun{
#' # obtain url for New Zealand data
#' nzl_url <- wdpa_url("New Zealand", wait = TRUE)
#' print(nzl_url)
#'
#' # obtain url for New Zealand data using its ISO3 code
#' nzl_url <- wdpa_url("NZL", wait = TRUE)
#' print(nzl_url)
#'
#' # obtain url for global data
#' global_url <- wdpa_url("global")
#' print(global_url)
#' }
#' @export
wdpa_url <- function(x, wait = FALSE, page_wait = 2) {
  # validate arguments
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::is.flag(wait),
    assertthat::is.count(page_wait),
    assertthat::noNA(page_wait),
    is_online())
  # declare hidden function
  try_and_find_url <- function(x) {
    ## initialize web driver
    result <- suppressMessages(tryCatch({
      ## initialize URL
      url <- character(0)
      ## specify port
      port <- as.integer(ceiling(sample(14415:14935, 1)))
      ## navigate to download web page
      pjs <- wdman::phantomjs(port = port, verbose = FALSE)
      rd <- RSelenium::remoteDriver(port = port, browserName = "phantomjs")
      rd$open(silent = TRUE)
      rd$maxWindowSize()
      rd$navigate(paste0("https://protectedplanet.net/country/", x))
      Sys.sleep(page_wait) # wait for page to load
      elem <- rd$findElement(using = "css", ".download__trigger")
      elem$clickElement()
      Sys.sleep(page_wait) # wait for page to load
      elem <- rd$findElement(using = "css", "li:nth-child(2) .popup__link")
      elem$clickElement()
      Sys.sleep(page_wait) # wait for dialog to open
      elem <- rd$findElement(using = "css", ".modal__link-button")
      elem$clickElement()
      Sys.sleep(page_wait) # wait for for dialog to open
      ## extract html for modal
      src <- xml2::read_html(rd$getPageSource()[[1]][[1]], encoding = "UTF-8")
      divs <- xml2::xml_find_all(src, ".//div")
      divs <- divs[which(xml2::xml_attr(divs, "class") == "modal__content")]
      ## parse download link
      attrs <- xml2::xml_attr(xml2::xml_find_all(divs, ".//a"), "href")
      url <- grep("shp.zip", attrs, fixed = TRUE, value = TRUE)
    },
    finally = {
      ## clean up web driver
      try(ph <- pjs$process)
      try(rd$close(), silent = TRUE)
      try(rd$close(), silent = TRUE)
      try(pjs$stop(), silent = TRUE)
      try(pjs$stop(), silent = TRUE)
      try(ph$kill_tree(), silent = TRUE)
      try(ph$kill_tree(), silent = TRUE)
    }))
    ## prepare output
    if (length(url) == 0)
      return(NA_character_) #nocov
    return(url)
  }
  # find url
  if (x == "global") {
    out <-
      "http://wcmc.io/wdpa_current_release"
  } else {
    ## convert x to country ISO3 code
    x <- country_code(x)
    ## initialize web driver
    ## check if data is ready for download
    attempted_url <- try_and_find_url(x)
    ## return NA if not ready and not wait
    #nocov start
    if (is.na(attempted_url) && !wait) {
      stop(paste("data is not yet available for download; try again later",
                 "or use wait=TRUE"))
    } else {
      ## otherwise check for url in 5 minute increments
      while (is.na(attempted_url)) {
        Sys.sleep(60 * 5)
        attempted_url <- try_and_find_url(x)
      }
      ## now that data is available, store the url
      out <- attempted_url
    }
    #nocov end
  }
  # return url
  return(out)
}
