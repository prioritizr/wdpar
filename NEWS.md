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
