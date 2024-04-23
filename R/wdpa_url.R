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
    is_online()
  )
  assertthat::assert_that(
    is_chrome_available(),
    msg = paste0(
      "couldn't find a Chromium-based browser,",
      "please try installing Google Chrome"
    )
  )
  # declare hidden function
  try_and_find_url <- function(x) {
    ## initialize web driver
    result <- suppressMessages(tryCatch({
      ## initialize URL
      url <- character(0)
      ## initialize driver
      b <- chromote::ChromoteSession$new()
      ## navigate to download web page
      p <- b$Page$loadEventFired(wait_ = FALSE)
      b$Page$navigate(
        paste0("https://www.protectedplanet.net/country/", x),
        wait_ = FALSE
      )
      b$wait_for(p)
      ## click "Download" button
      chromote_click_element(b, ".download__trigger")
      Sys.sleep(page_wait) # wait for page to load
      ## click "SHP" button
      chromote_click_element(b, "li:nth-child(2) .popup__link")
      Sys.sleep(page_wait) # wait for dialog to open
      ## click download link button
      chromote_click_element(b, ".modal__link-button")
      Sys.sleep(page_wait) # wait for for dialog to open
      ## extract html for modal
      src <- xml2::read_html(chromote_page_source(b)[[1]], encoding = "UTF-8")
      divs <- xml2::xml_find_all(src, ".//div")
      divs <- divs[which(xml2::xml_attr(divs, "class") == "modal__content")]
      ## parse download link
      attrs <- xml2::xml_attr(xml2::xml_find_all(divs, ".//a"), "href")
      url <- grep("shp.zip", attrs, fixed = TRUE, value = TRUE)
    },
    finally = {
      ## clean up web driver
      try(suppressMessages(b$parent$close()), silent = TRUE)
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

is_chrome_available <- function() {
  x <- chromote::find_chrome()
  if (is.null(x)) return(FALSE)
  if (!nzchar(x)[[1]]) return(FALSE)
  TRUE
}

chromote_page_source <- function(b) {
  # from https://stackoverflow.com/a/76347768
  b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
}

chromote_click_element <- function(b, css) {
  # from https://github.com/rstudio/chromote/issues/30
  doc <- b$DOM$getDocument()
  node <- b$DOM$querySelector(doc$root$nodeId, css)
  assertthat::assert_that(
    node$nodeId >= 1,
    msg = paste0("couldn't find element using query \", css, \"")
  )
  box <- b$DOM$getBoxModel(node$nodeId)
  br <- box$model$border
  x <- (br[[1]] + br[[5]]) / 2
  y <- (br[[2]] + br[[6]]) / 2
  b$Input$dispatchMouseEvent(
    type = "mousePressed", x = x, y = y, button = "left",
    clickCount = 1
  )
  Sys.sleep(0.01)
  b$Input$dispatchMouseEvent(
    type = "mouseReleased", x = x, y = y, button = "left"
  )
  NULL
}
