# Clean data

Clean data obtained from [Protected
Planet](https://www.protectedplanet.net/en). Specifically, this function
is designed to clean data obtained from the World Database on Protected
Areas (WDPA) and the World Database on Other Effective Area-Based
Conservation Measures (WDOECM). For recommended practices on cleaning
large datasets (e.g. datasets that span multiple countries or a large
geographic area), please see below.

## Usage

``` r
wdpa_clean(
  x,
  crs = "ESRI:54017",
  exclude_unesco = TRUE,
  retain_status = c("Designated", "Inscribed", "Established"),
  snap_tolerance = 1,
  simplify_tolerance = 0,
  geometry_precision = 1500,
  erase_overlaps = TRUE,
  repair_geometries = TRUE,
  verbose = interactive()
)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  containing protected area data.

- crs:

  `character` or `integer` object representing a coordinate reference
  system. Defaults to World Behrmann (`"ESRI:54017"`).

- exclude_unesco:

  `logical` should UNESCO Biosphere Reserves be excluded? Defaults to
  `TRUE`.

- retain_status:

  `character` vector containing the statuses for protected areas that
  should be retained during the cleaning process. Available statuses
  include: `"Proposed"`, `"Inscribed"`, `"Adopted"`, `"Designated"`, and
  `"Established"`. Additionally, a `NULL` argument can be specified to
  ensure that no protected areas are excluded according to their status.
  The default argument is a `character` vector containing
  `"Designated"`, `"Inscribed"`, and `"Established"`. This default
  argument ensures that protected areas that are not currently
  implemented are excluded.

- snap_tolerance:

  `numeric` tolerance for snapping geometry to a grid for resolving
  invalid geometries. Defaults to 1 meter.

- simplify_tolerance:

  `numeric` simplification tolerance. Defaults to 0 meters.

- geometry_precision:

  `numeric` level of precision for processing the spatial data (used
  with
  [`sf::st_set_precision()`](https://r-spatial.github.io/sf/reference/st_precision.html)).
  The default argument is 1500 (higher values indicate higher
  precision). This level of precision is generally suitable for analyses
  at the national-scale. For analyses at finer-scale resolutions, please
  consider using a greater value (e.g. 10000).

- erase_overlaps:

  `logical` should overlapping boundaries be erased? This is useful for
  making comparisons between individual protected areas and
  understanding their "effective" geographic coverage. On the other
  hand, this processing step may not be needed (e.g. if the protected
  area boundaries are going to be rasterized), and so processing time
  can be substantially by skipping this step and setting the argument to
  `FALSE`. Defaults to `TRUE`.

- repair_geometries:

  `logical` indicating if the data cleaning procedure should repair
  invalid spatial geometries? Although it is often necessary to repair
  geometries when working with national or multi-national scales, it may
  introduce, spatial artifacts that cause problems at local scales.
  Defaults to `TRUE`. Also, please be aware that setting
  `repair_geometries = FALSE` may result in errors during the data
  cleaning process.

- verbose:

  `logical` should progress on data cleaning be reported? Defaults to
  `TRUE` in an interactive session, otherwise `FALSE`.

## Value

A [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Details

This function cleans data following best practices (Butchart *et al.*
2015; Protected Planet 2021; Runge *et al.* 2015). To obtain accurate
protected area coverage statistics for a country, please note that you
will need to manually clip the cleaned data to the countries' coastline
and its Exclusive Economic Zone (EEZ).

1.  Exclude protected areas according to their status (i.e. `"STATUS"`
    field). Specifically, protected areas that have a status not
    specified in the argument to `retain_status` are excluded. By
    default, only protected areas that have a `"Designated"`,
    `"Inscribed"`, or `"Established"` status are retained. This means
    that the default behavior is to exclude protected that are not
    currently implemented.

2.  Exclude United Nations Educational, Scientific and Cultural
    Organization (UNESCO) Biosphere Reserves (Coetzer *et al.* 2014).
    This step is only performed if the argument to `exclude_unesco` is
    `TRUE`.

3.  Standardize column names to ensure consistency between data stored
    in geodatabase and shapefile formats. Specifically, if present, the
    `"PARENT_ISO3"` field is renamed to "PARENT_ISO" and the "SHAPE"
    field is renamed to `"geometry"`. Note that this field was
    deprecated by Protected Planet in November 2025 and so this step is
    not performed for more recent versions of the database. This
    information is now provided by the `"PRNT_ISO3"` field.

4.  Create a field (`"GEOMETRY_TYPE"`) indicating if areas are
    represented as point localities (`"POINT"`) or as polygons
    (`"POLYGON"`).

5.  Exclude areas represented as point localities that do not have a
    reported spatial extent (i.e. missing data for the field

6.  Geometries are wrapped to the dateline (using
    [`sf::st_wrap_dateline()`](https://r-spatial.github.io/sf/reference/st_transform.html)
    with the options `"WRAPDATELINE=YES"` and `"DATELINEOFFSET=180"`).

7.  Reproject data to coordinate system specified in argument to `crs`
    (using
    [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)).

8.  Repair any invalid geometries that have manifested (using
    [`st_repair_geometry()`](st_repair_geometry.md)). Note that this
    step is not performed if `repair_geometries = FALSE`.

9.  Buffer areas represented as point localities to circular areas using
    their reported spatial extent (using data in the field `"REP_AREA"`
    and
    [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html);
    see Visconti *et al.* 2013).

10. Snap the geometries to a grid to fix any remaining geometry issues
    (using argument to `snap_tolerance` and
    [`lwgeom::st_snap_to_grid()`](https://r-spatial.github.io/lwgeom/reference/st_snap_to_grid.html)).

11. Repair any invalid geometries that have manifested (using
    [`st_repair_geometry()`](st_repair_geometry.md)). Note that this
    step is not performed if `repair_geometries = FALSE`.

12. Simplify the protected area geometries to reduce computational
    burden (using argument to `simplify_tolerance` and
    [`sf::st_simplify()`](https://r-spatial.github.io/sf/reference/geos_unary.html)).
    Note that this step is not performed if `repair_geometries = FALSE`.

13. Repair any invalid geometries that have manifested (using
    [`st_repair_geometry()`](st_repair_geometry.md)).

14. If the `"MARINE"` field is present, then it will be converted from
    integer codes to descriptive names (i.e. `0` = `"terrestrial"`, `1`
    = `"partial"`, `2` = `"marine"`). Note that this field was
    deprecated by Protected Planet in November 2025 and so this step is
    not performed for more recent versions of the database. This
    information is now provided by the `"REALM"` field, wherein sites
    with over 10% of their area on land are assigned a `"Terrestrial"`
    value, sites with over 90% of their area in marine areas are
    assigned a
    `"Marine" value, and remaining sites are assigned a `"Coastal"\`
    value.

15. If the `"PA_DEF"` field is present, then it will be converted from
    integer codes to descriptive names (i.e. `0` = `"OECM"`, and `1` =
    `"PA"`). Note that this field was deprecated by Protected Planet in
    November 2025 and so this step is not performed for more recent
    versions of the database. This information is now provided by the
    `"SITE_TYPE"` field.

16. Zeros in the `"STATUS_YR"` field are replaced with missing values
    (i.e. `NA_real_` values).

17. Zeros in the `"NO_TK_AREA"` field are replaced with `NA` values for
    areas where such data are not reported or applicable (i.e. areas
    with the values `"Not Applicable"` or `"Not Reported"` in the
    `"NO_TK_AREA"` field).

18. Overlapping geometries are erased from the protected area data
    (discussed in Deguignet *et al.* 2017). Geometries are erased such
    that areas associated with more effective management categories
    (`"IUCN_CAT"`) or have historical precedence are retained (using
    [`sf::st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)).

19. Slivers are removed (geometries with areas less than 0.1 square
    meters).

20. The size of areas are calculated in square kilometers and stored in
    the field `"AREA_KM2"`.

21. Trimming extra leading or trailing white space characters (i.e.,
    `" "`, `"\n"`, `"\r"`) from fields that contain character values.

## Recommended practices for large datasets

This function can be used to clean large datasets assuming that
sufficient computational resources and time are available. Indeed, it
can clean data spanning large countries, multiple countries, and even
the full global dataset. When processing the full global dataset, it is
recommended to use a computer system with at least 32 GB RAM available
and to allow for at least one full day for the data cleaning procedures
to complete. It is also recommended to avoid using the computer system
for any other tasks while the data cleaning procedures are being
completed, because they are very computationally intensive.
Additionally, when processing large datasets – and especially for the
global dataset – it is strongly recommended to disable the procedure for
erasing overlapping areas. This is because the built-in procedure for
erasing overlaps is very time consuming when processing many protected
areas, so that information on each protected area can be output (e.g.
IUCN category, year established). Instead, when cleaning large datasets,
it is recommended to run the data cleaning procedures with the procedure
for erasing overlapping areas disabled (i.e. with
`erase_overlaps = FALSE`). After the data cleaning procedures have
completed, the protected area data can be manually dissolved to remove
overlapping areas (e.g. using [`wdpa_dissolve()`](wdpa_dissolve.md)).
For an example of processing a large protected area dataset, please see
the vignette.

## References

Butchart SH, Clarke M, Smith RJ, Sykes RE, Scharlemann JP, Harfoot M,
... & Brooks TM (2015) Shortfalls and solutions for meeting national and
global conservation area targets. *Conservation Letters*, **8**:
329–337.

Coetzer KL, Witkowski ET, & Erasmus BF (2014) Reviewing Biosphere
Reserves globally: Effective conservation action or bureaucratic label?
*Biological Reviews*, **89**: 82–104.

Deguignet M, Arnell A, Juffe-Bignoli D, Shi Y, Bingham H, MacSharry B &
Kingston N (2017) Measuring the extent of overlaps in protected area
designations. *PloS One*, **12**: e0188681.

Runge CA, Watson JEM, Butchart HM, Hanson JO, Possingham HP & Fuller RA
(2015) Protected areas and global conservation of migratory birds.
*Science*, **350**: 1255–1258.

Protected Planet (2021) Calculating protected and OECM area coverage.
Available at:
<https://www.protectedplanet.net/en/resources/calculating-protected-area-coverage>.

Visconti P, Di Marco M, Alvarez-Romero JG, Januchowski-Hartley SR,
Pressey, RL, Weeks R & Rondinini C (2013) Effects of errors and gaps in
spatial data sets on assessment of conservation progress. *Conservation
Biology*, **27**: 1000–1010.

## See also

[`wdpa_fetch()`](wdpa_fetch.md), [`wdpa_dissolve()`](wdpa_dissolve.md).

## Examples

``` r
# \dontrun{
# fetch data for the Liechtenstein
lie_raw_data <- wdpa_fetch("LIE", wait = TRUE)

# clean data
lie_data <- wdpa_clean(lie_raw_data)

# plot cleaned dataset
plot(lie_data)
#> Warning: plotting the first 9 out of 35 attributes; use max.plot = 35 to plot all


# }
```
