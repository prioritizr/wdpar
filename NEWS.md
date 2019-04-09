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

# wdpar 0.0.0.4 (unreleased)

- Fix broken link in README, add package website to DESCRIPTION.
- Update lifecycle badge to stable.
- Fix bug that threw a bogus error message in `wdpa_fetch` when `force = TRUE`.

# wdpar 0.0.0.3 (unreleased)

- Update overview in README and vignette (#3; @rungec).
- Add list with other datasets to vignette (#4; @rungec).
- Add information about coastlines and EEZs to the README, vignette, and `wdpa_clean` documentation (#5; @rungec).
- Add Visconti _et al._ 2013 citation to `wdpa_clean` help page, regarding the
  buffering of point localities.

# wdpar 0.0.0.2 (unreleased)

- Initial "everything works" version.

# wdpar 0.0.0.1 (unreleased)

- Initial package version.
