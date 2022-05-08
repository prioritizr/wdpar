#' @include internal.R wdpa_url.R wdpa_latest_version.R
NULL

#' Fetch data
#'
#' Fetch data from [Protected Planet](https://www.protectedplanet.net/en).
#' Specifically, data are downloaded from the
#' World Database on Protected Areas
#' (WDPA) and the World Database on Other Effective Area-Based Conservation
#' Measures (WDOECM).
#' **Note that data are downloaded assuming non-commercial use.**
#'
#' @inheritParams wdpa_read
#' @inheritParams wdpa_url
#'
#' @param x `character` country for which to download data. This argument
#'   can be the name of the country (e.g. `"Liechtenstein"`) or the
#'   ISO-3 code for the country (e.g. `"LIE"`). This argument can also
#'   be set to `"global"` to download all of the protected areas available
#'   in the database (approximately 1.1 GB).
#'
#' @param wait `logical` if data is not immediately available for download
#'   should the session be paused until it is ready for download? If argument
#'   to `wait` is `FALSE` and the data is not ready then `NA`
#'   will be returned. Defaults to `FALSE`.
#'
#' @param download_dir `character` folder path to download the data.
#'  Defaults to a temporary directory. To avoid downloading the
#'  same dataset multiple times, it is recommended to use a persistent
#'  directory (e.g. `rappdirs::user_data_dir("wdpar")`; see Examples below).
#'
#' @param force_download `logical` if the data has previously been
#'   downloaded and is available at argument to `download_dir`, should a
#'   fresh copy be downloaded? Defaults to `FALSE`.
#'
#' @param check_version `logical` if the data are being imported from
#'  from the argument to `download_dir`, should the data be checked to see
#'  if the version number matches the latest version available online?
#'  Defaults to `TRUE`.
#'
#' @param verbose `logical` should a progress on downloading data be
#'   reported? Defaults to `TRUE` in an interactive session, otherwise
#'   `FALSE`.
#'
#' @details
#' This function will check to see if the specified data
#' have previously been downloaded, download the data if needed,
#' and import the data.
#' After importing the data, it is strongly recommended to clean the data
#' prior to analysis (see [wdpa_clean()]).
#'
#' @section Data source:
#' The `PA_DEF` column indicates the data source for individual
#' areas and sites that comprise the imported dataset.
#' Specifically, data obtained through the World Database on Protected Areas
#' (WDPA) are indicated with a value of `1` in the `PA_DEF` column.
#' Additionally, data obtained through the World Database on Other Effective
#' Area-Based Conservation Measures (WDOECM) are indicated with a value of `0`
#' in the `PA_DEF` column.
#' For more details on data conventions, please consult the official manual
#' (UNEP-WCMC 2019).
#'
#' @section Troubleshooting:
#' This function will sometimes return the error message
#' `PhantomJS signals port = 4567 is already in use`.
#' This error message can occur when you have previously run the function and
#' it threw an error, or it terminated early.
#' It can also occur when attempting to run the the function in multiple
#' sessions on the same computer.
#' To address this issue, you will need to restart your computer.
#'
#' @return [sf::sf()] object.
#'
#' @seealso [wdpa_clean()], [wdpa_read()],
#'   [wdpa_url()], [countrycode::countrycode()].
#'
#' @references
#' UNEP-WCMC (2019). User Manual for the World Database on Protected Areas and
#' world database on other effective area-based conservation measures: 1.6.
#' UNEP-WCMC: Cambridge, UK. Available at: <https://wcmc.io/WDPA_Manual>.
#'
#' @examples
#' \dontrun{
#' # fetch data for Liechtenstein
#' lie_raw_data <- wdpa_fetch("Liechtenstein", wait = TRUE)
#'
#' # print data
#' print(lie_raw_data)
#'
#' # plot data
#' plot(lie_raw_data)
#'
#' # fetch data for Liechtenstein using the ISO3 code
#' lie_raw_data <- wdpa_fetch("LIE", wait = TRUE)
#'
#' # since data are saved in a temporary directory by default,
#' # a persistent directory can be specified to avoid having to download the
#' # same dataset every time the R session is restarted
#' lie_raw_data <- wdpa_fetch("LIE", wait = TRUE,
#'                            download_dir = rappdirs::user_data_dir("wdpar"))
#'
#' # data for multiple countries can be downloaded separately and combined,
#' # this is useful to avoid having to download the global dataset
#' ## load packages to easily merge datasets
#' library(dplyr)
#' library(tibble)
#'
#' ## define country names to download
#' country_codes <- c("LIE", "MHL")
#'
#' ## download data for each country
#' mult_data <- lapply(country_codes, wdpa_fetch, wait = TRUE)
#'
#' ## merge datasets together
#' mult_dat <- st_as_sf(as_tibble(bind_rows(mult_data)))
#'
#' ## print data
#' print(mult_dat)
#' }
#' @export
wdpa_fetch <- function(x, wait = FALSE,
                       download_dir = tempdir(),
                       force_download = FALSE,
                       check_version = TRUE,
                       n = NULL,
                       page_wait = 2,
                       verbose = interactive()) {
  # check that arguments are valid
  ## check that classes are correct
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::is.dir(download_dir),
    assertthat::is.flag(force_download),
    assertthat::is.flag(verbose),
    assertthat::is.flag(check_version),
    identical(x, "global") || assertthat::is.string(country_code(x)))
  # try to find locally on system
  file_path <- try(wdpa_file(x, download_dir = download_dir), silent = TRUE)
  # fetch data
  if (force_download || inherits(file_path, "try-error")) {
    ## check for internet connection
   if (!is_online()) {
      #nocov start
      stop(paste0("data not found in download_dir, and no internet connection",
                  "to download it."))
      #nocov end
    }
    ## find latest version of the dataset
    current_month_year <- wdpa_latest_version()
    ## find the download link and set file path to save the data
    download_url <- wdpa_url(x, wait = wait, page_wait = page_wait)
    ## note that file name conventions on protectedplanet.net have changed
    ## (detected on 8th Oct 2020) and so file names are manually changed
    ## to follow the previous convention
    if (!identical(x, "global")) {
      file_name <- paste0("WDPA_", current_month_year, "_", country_code(x),
                          "-shapefile.zip")
    } else {
      file_name <- paste0("WDPA_", current_month_year, "_Public.gdb.zip")
    }
    file_path <- file.path(download_dir, file_name)
    ## download the data
    if (!file.exists(file_path) || force_download) {
      download_file(download_url, file_path, quiet = !verbose)
      if (verbose)
        message("\n")
    }
    ## verify that the file exists
    if (!file.exists(file_path))
      stop("downloading data failed") #nocov
  } else {
    # check version of available data
    if (isTRUE(check_version)) {
      ## if internet available...
      if (is_online()) {
        ### parse month-year from input file
        input_version <- wdpa_version(file_path)
        input_file_date <- convert_wdpa_version_to_POSIXct(input_version)
        ### parse month-year from latest release
        current_version <- wdpa_latest_version()
        current_file_date <- convert_wdpa_version_to_POSIXct(current_version)
        ### throw warning if out of date
        if (input_file_date < current_file_date) {
          #nocov start
          warning(paste0("local data is out of date: ",
                         format(input_file_date, "%b %Y")))
          #nocov end
        }
      } else {
        ## if internet not available
        warning("cannot verify if version on disk is up to date.") #nocov
      }
    }
  }
  # import the data
  wdpa_read(file_path, n)
}
