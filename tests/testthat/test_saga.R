context("saga")

test_that("saga_difference", {
  # create data
  b0 <- sf::st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1),
                                  c(-1, -1))))
  b1 <- b0 + 2
  b2 <- b0 + c(-0.2, 2)
  x <- sf::st_union(sf::st_sfc(b0, b1, b2))
  a0 <- b0 * 0.8
  a1 <- a0 * 0.5 + c(2, 0.7)
  a2 <- a0 + 1
  a3 <- b0 * 0.5 + c(2, -0.5)
  y <- sf::st_union(sf::st_sfc(a0, a1, a2, a3))
  ## make data for comparison
  o1 <- sf::st_difference(x, y)
  o2 <- saga_difference(x, y)
  ## test if sf and saga yield same result
  sf::st_precision(o1) <- 1000
  sf::st_precision(o2) <- 1000
  expect_true(sf::st_equals(o1, o2, sparse = FALSE)[1])
})

test_that("saga_union", {
  # create data
  x <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  x <- sf::st_geometry(x)
  ## make data for comparison
  o1 <- sf::st_union(x)
  o2 <- saga_union(x)
  ## test if sf and saga yield same result
  sf::st_precision(o1) <- 1000
  sf::st_precision(o2) <- 1000
  expect_true(sf::st_equals(o1, o2, sparse = FALSE)[1])
})
