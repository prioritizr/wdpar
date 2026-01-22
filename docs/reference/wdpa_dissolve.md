# Dissolve data

Create a dataset of spatial boundaries that contains no overlapping
geometries.

## Usage

``` r
wdpa_dissolve(x, geometry_precision = 1500)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

- geometry_precision:

  `numeric` level of precision for processing the spatial data (used
  with
  [`sf::st_set_precision()`](https://r-spatial.github.io/sf/reference/st_precision.html)).
  The default argument is 1500 (higher values indicate higher
  precision). This level of precision is generally suitable for analyses
  at the national-scale. For analyses at finer-scale resolutions, please
  consider using a greater value (e.g. 10000).

## Value

A [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Details

This function is basically a wrapper for
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html).
It also contains additional parameters to assist with processing large
and complex geometry data.

## See also

[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html),
[`st_erase_overlaps()`](st_erase_overlaps.md).

## Examples

``` r
# create data
pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0, 0), byrow = TRUE,
                                  ncol = 2))) * 100
pl2 <- sf::st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5),
                                  byrow = TRUE, ncol = 2))) * 100
pl3 <- sf::st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25),
                                  byrow = TRUE, ncol = 2))) * 100
x <- sf::st_sf(order = c("A", "B", "C"),
               geometry = sf::st_sfc(list(pl1, pl2, pl3), crs = 3395))

# dissolve data
y <- wdpa_dissolve(x)

# plot data for visual comparison
par(mfrow = c(1, 2))
plot(sf::st_geometry(x), xlim = c(0, 200), ylim = c(0, 250),
     main = "original", col = "transparent")
plot(sf::st_geometry(y), , xlim = c(0, 200), ylim = c(0, 250),
     main = "dissolved", col = "transparent")
```
