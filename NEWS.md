# wdpar 1.3.8.3

- Update `wdpa_read()` to use the _archive_ package for unzipping files. This
  is needed to ensure compatibility with Windows operating systems, because the
  built-in `unzip()` function is not robust enough against invalid characters.
  Thanks to Aharon Fleury for bug report.

# wdpar 1.3.8.2

- Update `st_repair_geometry()` documentation to specify correct installation
  instructions for the _prepr_ package (#89). Thanks to Jason Everett
  (\@jaseeverett) for bug report.
- Update `wdpa_url()` to provide correctly formatted error message when
  it can't find matching CSS queries.

# wdpar 1.3.8.1

- Update `wdpa_read()` so that it can import country-specific data in
  shapefile format that have been manually downloaded from ProtectedPlanet
  (#87). Thanks to Florent Bédécarrats (\@fBedecarrats) for bug report.
- Minor improvements to documentation.

# wdpar 1.3.8

- CRAN release.
- Minor improvements to documentation.

# wdpar 1.3.7.4

- Fix typo in text formatting for `wdpa_clean()` documentation.

# wdpar 1.3.7.3

- Fix bugs in `wdpa_fetch()` and `wdpa_read()` on MacOS.
  Thanks to Jason Everett (\@jaseeverett) for code contribution.
- Update DESCRIPTION with _prepr_ package information.
- Update README with Chromium dependencies.
- Update global example global processing script with increased precision.
- Update vignette to be compatible with changes to GADM website.
- Fix issue with tests leaving left-over files in temporary directories.

# wdpar 1.3.7.2

- Update `wdpa_fetch()` to use the _chromote_ package to handle web scrapping
 (instead of the _webdriver_ package).
- Update `wdpa_fetch()` so that it can download country-level data in either
  shapefile or file geodatabase format (using the new `datatype` parameter).
  Since file geodatabase data appears to be more robust, `wdpa_fetch()`
  now defaults to downloading data in file geodatabase format.
- Update `wdpa_clean()` to standardize column names so that cleaning
  either shapefile or file geodatabase data results in the same output.
- Update `wdpa_clean()` so that it removes leading/trailing white space
  characters from the `"MANG_PLAN"` field.
- Fix bug in `wdpa_read()` that causes output objects to contain no columns.
- Update README and vignette to be compatible with updates to _ggmap_ package.

# wdpar 1.3.7.1

- Update tests to accommodate corrupted data from Protected Planet.

# wdpar 1.3.7

- CRAN release.

# wdpar 1.3.6

- Rejected CRAN submission.

# wdpar 1.3.5.2

- Fix issue with `st_erase_overlaps()` not correctly removing overlaps when
  a protected area are completely contained within another (#73).

# wdpar 1.3.5.1

- Fix aliasing for package manual entry (#71).

# wdpar 1.3.5

- CRAN release.
- Fix compatibility issues with Protected Planet (#69).
- Update `wdpa_clean()` to be more robust (#68).
- Remove _sp_ package as dependency.

# wdpar 1.3.4

- CRAN release.
- Update citation format.

# wdpar 1.3.3.6

- Fix broken URL in vignette.
- Fix badges in README.

# wdpar 1.3.3.5

- Update citation information with journal article.

# wdpar 1.3.3.4

- Fix tests when PhantomJS not installed.
- Fix broken URL in vignette.

# wdpar 1.3.3.3

- Improve error message for `wdpa_fetch()` when PhantomJS is not
  installed (#63).

# wdpar 1.3.3.2

- Add URLs to README for citations (#58).
- Update `wdpa_fetch()` documentation to provide information on out of date
  warnings (#59).
- Update `wdpa_fetch()` warnings to be more descriptive and use the _cli_
  package for consistent alert messages.

# wdpar 1.3.3.1

- Update paper for JOSS submission (#53, #54, #62).
- Update vignette with new section on local scale analyses (#53).
- Update `wdpa_fetch()` function to use the webdriver package for obtaining data
  (replacing Rselenium package as a dependency) (#63).
- Update `st_repair_geometry()` to be more robust.
- Fix failing tests for `st_repair_geometry()` function.
- Update documentation for `wdpa_clean()` function.
- Fix broken URL in vignette.

# wdpar 1.3.3

- CRAN release.
- Reduce test timings.

# wdpar 1.3.2.4

- Fix compatibility with GEOS (version 3.11.0) (#50, #51).
- Fix broken URLs in documentation.

# wdpar 1.3.2.3

- Fix typos in documentation, add JOSS paper, and add contributing guide.

# wdpar 1.3.2.2

- Reduce false-negative rate when checking for available internet
  connection (#10).

# wdpar 1.3.2.1

- Select a random port for the web driver (#41).
- Make web driver clean up more robust (#41).

# wdpar 1.3.2

- CRAN release.
- Update `read_sf_n` to import data faster.
- Remove _withr_ package from DESCRIPTION because it is not used.

# wdpar 1.3.1.6

- Update `wdpa_clean` to format the `PA_DEF` column to indicate
  if each area is a protected area (per IUCN and CBD protected area definitions)
  or an other effective area-based conservation measure (OECM).
- Update documentation to make it clear that data obtained from Protected Planet
  include both the World Database on Protected Areas (WDPA) and the World
  Database on Other Effective Area-Based Conservation Measures (WDOECM).
- Update citation information to follow recommended citations for the World
  Database on Protected Areas (WDPA) and the World Database on Other Effective
  Area-Based Conservation Measures (WDOECM).
- Update URLs to pass CRAN checks.

# wdpar 1.3.1.5

- Update `wdpa_fetch` with new `check_version` argument to specify if
  the version of cached data should be checked against the latest version
  available online.
- Update `wdpa_clean` to not throw an unnecessary and confusing warning message
  when attempting to clean data that do not contain any valid polygon
  geometries.

# wdpar 1.3.1.4

- Fix bug in `wdpa_read` to ensure that all data from global database.
  This is a bug previously meant that protected areas denoted with polygon
  geometry data were not imported. It is strongly recommended that users
  double check processed versions of the global database to verify correctness.
- Add example script for downloading and cleaning data
  (see `inst/scripts/global-example-script.R`)
- New `st_repair_geometry` function to repair geometry using a combination
  of `sf::st_make_valid` and `prepr::st_prepair`. This function is now
  used by the `wdpa_clean` function to augment data cleaning procedures.
- Update `wdpa_url` and `wdpa_fetch` to have a `page_wait` parameter
  to specify the wait time for loading web pages when finding the download
  URLs for datasets (#39).
- Add _dplyr_ package to Suggests because it is used in an example.

# wdpar 1.3.1.3

- Update `wdpa_clean` to provide better information when cleaning data.
- Update documentation to provide guidelines for processing global dataset.
- New `wdpa_dissolve` function to dissolve geometries together (#22).

# wdpar 1.3.1.2

- Fix tests that fail package checks given only strict dependencies.
- Update citation information in README and vignette.

# wdpar 1.3.1.1

- Update `wdpa_url` function to be compatible with changes on
  <https://www.protectedplanet.net/en> for downloading country-level datasets.

# wdpar 1.3.1

- CRAN release.
- Remove LazyData from DESCRIPTION since it is not used.
- Remove tools R package from DESCRIPTION since it is not used.

# wdpar 1.3.0.2

- Update `wdpa_fetch` function to conform with CRAN policies. Specifically,
  data are now always saved in a temporary directory by default.
- Update code in README for downloading developmental version from GitHub.
- Update internal functions to be more compatible with recent version of the sf
  R package.

# wdpar 1.3.0.1

- Update `wdpa_url` function to be compatible with changes on
  <https://www.protectedplanet.net/en>.
- Update `wdpa_read` function to be compatible with new global dataset format.
- Update `st_erase_overlaps` function to be more resilient against topology
  errors (#33).

# wdpar 1.3.0.0

- Fix URLs for CRAN.
- Increase test coverage.

# wdpar 1.2.0.0

- Update `wdpa_clean` function with new `retain_status` parameter to specify
  which protected areas should be retained during the cleaning process (#33).
  Defaults to a `character` vector containing `"Designated"`, `"Inscribed"`,
  `"Established"` to indicate that protected areas with these status
  (i.e. per `"STATUS"` column) are retained. Thus the default behavior from
  previous versions remains unchanged.

# wdpar 1.1.0.0

- Update `wdpa_clean` with new `exclude_unesco` parameter to specify if
  UNESCO Biosphere Reserves should be included (#33). Defaults to `TRUE` such
  that default behavior remains unchanged from previous versions.

# wdpar 1.0.6

- CRAN release.

# wdpar 1.0.5.2

- Update `wdpa_read` function to be compatible with changes to global
  dataset (#31).

# wdpar 1.0.5.1

- Update `wdpa_fetch` function to be compatible with changes to Protected
  Planet website for downloading global dataset.

# wdpar 1.0.5

- CRAN release.

# wdpar 1.0.4.1

- Update package to be compatible with new file name conventions on Protected
  Planet website.
- Fix bug with downloading global dataset.

# wdpar 1.0.4

- CRAN release.

# wdpar 1.0.3.1

- Add example for multiple countries to `wdpa_clean` documentation (#28).
- Add information on port error to `wdpa_clean` documentation (#29).
- Update package to work with new version of Protected Planet website (#30).
- New `wdpa_latest_version` function for determining the latest version of the
  dataset.

# wdpar 1.0.3

- CRAN release.

# wdpar 1.0.2.1

- Fix "Non-file package-anchored link(s) in documentation object" warnings
  in R-devel checks.
- Update `wdpa_read` so that it is compatible with the new data format provided
  by <https://www.protectedplanet.net/en>.
- Update examples for `st_erase_overlaps` so that overlapping geometries are
  clearly shown.
- Update examples to run with CRAN checks (i.e. `--run-donttest`).
- Update `st_erase_overlaps` so that it is more robust to geometry issues.

# wdpar 1.0.2

- CRAN release.

# wdpar 1.0.1.4

- Fix compatibility issues with new `lwgeom` package version 0.2-3 (#24).
- Increase `lwgeom` and `sf` package version requirements.

# wdpar 1.0.1.3

- The `wdpa_clean` function now returns an empty result (i.e. `sf` object with
  zero rows) for countries that do not have any protected areas represented by
  spatially valid non-empty geometries (e.g. Somalia in February 2020, #19).

# wdpar 1.0.1.2

- Increase default precision in `wdpa_clean` function.

# wdpar 1.0.1.1

- Make the `wdpa_clean` function more robust to typology issues (#20).
- Remove unnecessary internet connection check from `wdpa_clean` (#21).

# wdpar 1.0.1

- CRAN release.

# wdpar 1.0.0.4

- Update `wdpa_clean` so that it works with a single protected area.
- Tweak progress bar in `st_erase_overlaps`.

# wdpar 1.0.0.3

- Update `wdpa_clean` so that it works with shapefiles that do not have geometry
  stored in the `geometry` column (e.g. global data).

# wdpar 1.0.0.2

- Update `st_erase_overlaps` so that it has a higher success rate for
  really invalid geometries (e.g. protected areas for Gabon).

# wdpar 1.0.0.1

- Add extra data cleaning step to vignette so that the tutorial has a better
  chance at working when adapted to other countries.

# wdpar 1.0.0

- CRAN release.
- Fix links in CITATION, README, and vignette.

# wdpar 0.0.4

- Fix false reports of lack of Internet connectivity (#10).

# wdpar 0.0.3

- CRAN release.

# wdpar 0.0.2.1

- Fix compatibility issue changes to Protected Planet website (#11).

# wdpar 0.0.2

- CRAN release.

# wdpar 0.0.1.3

- Fix bug where parts of MULTIPOLYGON protected areas would be incorrectly
  erased during data cleaning (#9).

# wdpar 0.0.1.2

- Fix bug where protected areas represented as POINT and MULTIPOINT geometries
  were incorrectly omitted (#9).
- Address CRAN check NOTES by removing unused Imports, or manually calling one
  of their functions.

# wdpar 0.0.1.1

- Fix `Summary: ElementNotVisible` bug (#8).
- Citation automatically reports the correct URL (i.e. CRAN or GitHub) based on
  package version number.

# wdpar 0.0.1

- CRAN release.

# wdpar 0.0.0.4

- Fix broken link in README, add package website to DESCRIPTION.
- Update lifecycle badge to stable.
- Fix bug that threw a bogus error message in `wdpa_fetch` when `force = TRUE`.

# wdpar 0.0.0.3

- Update overview in README and vignette (#3; @rungec).
- Add list with other datasets to vignette (#4; @rungec).
- Add information about coastlines and EEZs to the README, vignette, and `wdpa_clean` documentation (#5; @rungec).
- Add Visconti _et al._ 2013 citation to `wdpa_clean` help page, regarding the
  buffering of point localities.

# wdpar 0.0.0.2

- Initial "everything works" version.

# wdpar 0.0.0.1

- Initial package version.
