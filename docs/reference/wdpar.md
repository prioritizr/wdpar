# wdpar: Interface to the World Database on Protected Areas

The wdpar R package provides an interface to data provided by [Protected
Planet](https://www.protectedplanet.net/en). Specifically, it can be
used to automatically obtain data from the World Database on Protected
Areas (WDPA) and the World Database on Other Effective Area-Based
Conservation Measures (WDOECM). It also provides methods for cleaning
data from these databases following best practices (outlined in Butchart
*et al.* 2015; Protected Planet 2021; Runge *et al.* 2015). The main
functions are [`wdpa_fetch()`](wdpa_fetch.md) for downloading data and
[`wdpa_clean()`](wdpa_clean.md) for cleaning data. For more information,
please see the package vignette. To cite this package, please see
`citation("wdpar")`.

## References

Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP, Harfoot M,
... & Brooks TM (2015) Shortfalls and solutions for meeting national and
global conservation area targets. *Conservation Letters*, **8**:
329–337.

Protected Planet (2021) Calculating protected and OECM area coverage.
Available at:
<https://www.protectedplanet.net/en/resources/calculating-protected-area-coverage>.

Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
(2015) Protected areas and global conservation of migratory birds.
*Science*, **350**: 1255–1258.

## See also

Useful links:

- <https://prioritizr.github.io/wdpar/>

- <https://github.com/prioritizr/wdpar>

- Report bugs at <https://github.com/prioritizr/wdpar/issues>

## Author

**Maintainer**: Jeffrey O Hanson <jeffrey.hanson@uqconnect.edu.au>
