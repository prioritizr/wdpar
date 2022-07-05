context("wdpa_dissolve")

test_that("works", {
  # create data
  pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0, 0), byrow = TRUE,
                                    ncol = 2))) * 100
  pl2 <- sf::st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5),
                                    byrow = TRUE, ncol = 2))) * 100
  pl3 <- sf::st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25),
                                    byrow = TRUE, ncol = 2))) * 100
  x <- sf::st_sf(order = c("A", "B", "C"),
                 geometry = sf::st_sfc(list(pl1, pl2, pl3), crs = 3395))
  # create result
  y <- wdpa_dissolve(x)
  y2 <- sf::st_set_precision(
    sf::st_sf(id = 1, geometry = sf::st_union(x)),
    1500
  )
  # tests
  expect_is(y, "sf")
  expect_equal(
    sf::st_equals(
      sf::st_geometry(y),
      sf::st_geometry(y2),
      sparse = FALSE
    )[[1]],
    TRUE
  )
  expect_equal(nrow(sf::st_cast(y, "POLYGON")), 1)
})
