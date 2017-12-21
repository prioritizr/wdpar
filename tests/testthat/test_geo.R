context("geo")

test_that("st_parallel_make_valid (sf)", {
  # create data
  x <- sf::st_sf(z = 1, geomtry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0),
                          c(0.5, 0), c(0.5, 0.5), c(0.5, 0), c(1, 0),
                          c(1, 1), c(0, 1), c(0, 0))))),
                 agr = "constant")
  # repair geometry
  y1 <- lwgeom::st_make_valid(x)
  y2 <- suppressWarnings(st_parallel_make_valid(x, threads = 1))
  y3 <- suppressWarnings(st_parallel_make_valid(x, threads = 2))
  # run tests
  expect_is(y1, "sf")
  expect_is(y2, "sf")
  expect_is(y3, "sf")
  expect_equal(y1, y2)
  expect_equal(y1, y3)
})

test_that("st_parallel_make_valid (sfc)", {
  # create data
  x <- sf::st_sfc(st_polygon(list(rbind(c(0, 0), c(0.5, 0), c(0.5, 0.5),
                                        c(0.5, 0), c(1, 0), c(1, 1),
                                        c(0, 1), c(0, 0)))))
  # repair geometry
  y1 <- lwgeom::st_make_valid(x)
  y2 <- suppressWarnings(st_parallel_make_valid(x, threads = 1))
  y3 <- suppressWarnings(st_parallel_make_valid(x, threads = 2))
  # run tests
  expect_is(y1, "sfc")
  expect_is(y2, "sfc")
  expect_is(y3, "sfc")
  expect_equal(y1, y2)
  expect_equal(y1, y3)
})

test_that("st_parallel_transform (sf)", {
  # create data
  pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
                                    byrow = TRUE, ncol = 2)))
  pl2 <- sf::st_polygon(list(matrix(c(5, 5, 7, 5, 7, 7, 5, 7, 5, 5),
                                    byrow = TRUE, ncol = 2)))
  x <- sf::st_sf(z = runif(2), geometry = sf::st_sfc(list(pl1, pl2),
                                                     crs = 4326))
  # transform data
  y1 <- sf::st_transform(x, 3395)
  y2 <- suppressWarnings(st_parallel_transform(x, 3395, threads = 1))
  y3 <- suppressWarnings(st_parallel_transform(x, 3395, threads = 2))
  # run tests
  expect_is(y1, "sf")
  expect_is(y2, "sf")
  expect_is(y3, "sf")
  expect_equal(y1, y2)
  expect_equal(y1, y3)
})

test_that("st_parallel_transform (sfc)", {
  # create data
  pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
                                    byrow = TRUE, ncol = 2)))
  pl2 <- sf::st_polygon(list(matrix(c(5, 5, 7, 5, 7, 7, 5, 7, 5, 5),
                                    byrow = TRUE, ncol = 2)))
  x <- sf::st_sfc(list(pl1, pl2), crs = 4326)
  # transform data
  y1 <- sf::st_transform(x, 3395)
  y2 <- suppressWarnings(st_parallel_transform(x, 3395, threads = 1))
  y3 <- suppressWarnings(st_parallel_transform(x, 3395, threads = 2))
  # run tests
  expect_is(y1, "sfc")
  expect_is(y2, "sfc")
  expect_is(y3, "sfc")
  expect_equal(y1, y2)
  expect_equal(y1, y3)
})

test_that("st_remove_holes (sf)", {
  # create data
  outer1 <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  outer2 <- matrix(c(20, 20, 30, 25, 30, 30, 25, 30, 20, 20), ncol = 2,
                   byrow = TRUE)
  x <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                  sf::st_polygon(list(outer1, hole1, hole2)),
                  sf::st_multipolygon(list(list(outer2), list(outer1, hole1,
                                                              hole2))),
                  crs = 4326)
  x <- sf::st_sf(x = letters[1:3], geometry = x)
  # remove holes
  y1 <- st_remove_holes(x)
  y2 <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                   sf::st_polygon(list(outer1)),
                   sf::st_multipolygon(list(list(outer2), list(outer1))),
                   crs = 4326)
  y2 <- sf::st_sf(x = letters[1:3], geometry = y2)
  # run tests
  expect_equal(y1, y2)
})

test_that("st_remove_holes (sfc)", {
  # create data
  outer1 <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  outer2 <- matrix(c(20, 20, 30, 25, 30, 30, 25, 30, 20, 20), ncol = 2,
                   byrow = TRUE)
  x <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                  sf::st_polygon(list(outer1, hole1, hole2)),
                  sf::st_multipolygon(list(list(outer2), list(outer1, hole1,
                                                              hole2))),
                  crs = 4326)
  # remove holes
  y1 <- st_remove_holes(x)
  y2 <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                   sf::st_polygon(list(outer1)),
                   sf::st_multipolygon(list(list(outer2), list(outer1))),
                   crs = 4326)
  # run tests
  expect_equal(y1, y2)
})

test_that("st_remove_holes (sfg)", {
  # create data
  outer1 <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  outer2 <- matrix(c(20, 20, 30, 25, 30, 30, 25, 30, 20, 20), ncol = 2,
                   byrow = TRUE)
  x1 <- sf::st_polygon(list(outer1, hole1, hole2))
  x2 <- sf::st_multipolygon(list(list(outer2), list(outer1, hole1, hole2)))
  x3 <- sf::st_point(rbind(c(0, 0)))
  # remove holes
  y1 <- st_remove_holes(x1)
  y2 <- st_remove_holes(x2)
  y3 <- st_remove_holes(x3)
  # run tests
  expect_equal(y1, sf::st_polygon(list(outer1)))
  expect_equal(y2, sf::st_multipolygon(list(list(outer2), list(outer1))))
  expect_equal(y3, x3)
})
