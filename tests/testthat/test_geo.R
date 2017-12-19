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
