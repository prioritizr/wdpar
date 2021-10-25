# Initialization
## define countries for processing data
country_names <- "global"

## define file path to save data
path <- paste0(
  "~/wdpa-data/global-", format(Sys.time(), "%Y-%m-%d"), ".gpkg"
)

## load packages
library(sf)
library(wdpar)

# Preliminary processing
## prepare folder if needed
export_dir <- suppressWarnings(normalizePath(dirname(path)))
if (!file.exists(export_dir)) {
  dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
}

## preapre user data directory
data_dir <- rappdirs::user_data_dir("wdpar")
if (!file.exists(data_dir)) {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## download data
raw_data <- wdpa_fetch(country_names, wait = TRUE, download_dir = data_dir)

## clean data
result_data <- wdpa_clean(raw_data, erase_overlaps = FALSE)

# Exports
## save result
sf::write_sf(result_data, path)
