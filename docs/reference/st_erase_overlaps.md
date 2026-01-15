# Erase overlaps

Erase overlapping geometries in a
[`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Usage

``` r
st_erase_overlaps(x, verbose = FALSE)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

- verbose:

  `logical` should progress be reported? Defaults to `FALSE`.

## Value

A [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Details

This is a more robust – albeit slower – implementation for
[`sf::st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
when `y` is missing.

## See also

[`sf::st_difference()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html),
[`wdpa_dissolve()`](wdpa_dissolve.md).

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

# erase overlaps
y <- st_erase_overlaps(x)

# plot data for visual comparison
par(mfrow = c(1, 2))
plot(sf::st_geometry(x), xlim = c(0, 200), ylim = c(0, 250),
     main = "original", col = "transparent")
plot(sf::st_geometry(y), , xlim = c(0, 200), ylim = c(0, 250),
     main = "no overlaps", col = "transparent")
```
