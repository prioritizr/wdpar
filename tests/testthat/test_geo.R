context("geo")

test_that("st_remove_holes (sf)", {
  # create data
  set.seed(500)
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
                   sf::st_multipolygon(list(list(outer1), list(outer2))),
                   crs = 4326)
  y2 <- sf::st_sf(x = letters[1:3], geometry = y2)
  # run tests
  expect_equal(y1, y2)
})

test_that("st_remove_holes (sfc)", {
  # create data
  set.seed(500)
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
                   sf::st_multipolygon(list(list(outer1), list(outer2))),
                   crs = 4326)
  # run tests
  expect_equal(y1, y2)
})

test_that("st_remove_holes (sfg)", {
  # create data
  set.seed(500)
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
  expect_equal(y2, sf::st_multipolygon(list(list(outer1), list(outer2))))
  expect_equal(y3, x3)
})

test_that("st_erase_overlaps (sf)", {
  # create input testing data
  pl1 <- sf::st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0, 0), byrow = TRUE,
                                    ncol = 2))) * 100
  pl2 <- sf::st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5),
                                    byrow = TRUE, ncol = 2))) * 100
  pl3 <- sf::st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25),
                                    byrow = TRUE, ncol = 2))) * 100
  x <- sf::st_sf(order = c("A", "B", "C"),
                 geometry = sf::st_sfc(list(pl1, pl2, pl3), crs = 3395))
  # erase overlaps
  y1 <- sf::st_sfc(list(
    sf::st_polygon(list(matrix(c(0, 2, 1, 0, 0, 0, 1, 0), ncol = 2))) * 100,
    sf::st_polygon(list(matrix(c(0.5, 0, 1, 2, 1.5, 1, 0.5, 0.5, 0.5, 1.5, 0.5,
                                 0.5, 1, 0.5), ncol = 2))) * 100,
    sf::st_polygon(list(matrix(c(0.75, 0, 1, 2, 1.25, 1, 0.75, 1.25, 1.25, 2.5,
                                 1.25, 1.25, 1.5, 1.25), ncol = 2))) * 100),
    crs = 3395)
  y1 <- sf::st_sf(order = c("A", "B", "C"), geometry = y1)
  y2 <- st_erase_overlaps(x) %>%
        sf::st_cast("POLYGON")
  # run tests
  expect_is(y2, "sf")
  expect_equal(y1[["order"]], y2[["order"]])
  expect_true(sf::st_equals(sf::st_set_precision(y1[["geometry"]], 1000),
                            sf::st_set_precision(y2[["geometry"]], 1000),
                            sparse = FALSE)[1])
})

test_that("st_extract_holes (sf)", {
  # create data
  set.seed(500)
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
  y1 <- st_extract_holes(x)
  y2 <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                   sf::st_multipolygon(list(list(hole1), list(hole2))),
                   sf::st_multipolygon(list(list(hole1), list(hole2))),
                   crs = 4326)
  y2 <- sf::st_sf(x = letters[1:3], geometry = y2)
  # run tests
  expect_equivalent(y1, y2)
})

test_that("st_extract_holes (sfc)", {
  # create data
  set.seed(500)
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
  y1 <- st_extract_holes(x)
  y2 <- sf::st_sfc(sf::st_point(rbind(c(0, 0))),
                   sf::st_multipolygon(list(list(hole1), list(hole2))),
                   sf::st_multipolygon(list(list(hole1), list(hole2))),
                   crs = 4326)
  # run tests
  expect_equivalent(y1, y2)
})

test_that("st_extract_holes (sfg)", {
  # create data
  set.seed(500)
  outer1 <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  outer2 <- matrix(c(20, 20, 30, 25, 30, 30, 25, 30, 20, 20), ncol = 2,
                   byrow = TRUE)
  x1 <- sf::st_polygon(list(outer1, hole1, hole2))
  x2 <- sf::st_multipolygon(list(list(outer2), list(outer1, hole1, hole2)))
  x3 <- sf::st_point(rbind(c(0, 0)))
  # remove holes
  y1 <- st_extract_holes(x1)
  y2 <- st_extract_holes(x2)
  y3 <- st_extract_holes(x3)
  # run tests
  expect_equal(y1, sf::st_multipolygon(list(list(hole1), list(hole2))))
  expect_equal(y2, sf::st_multipolygon(list(list(hole1), list(hole2))))
  expect_equal(y3, x3)
})
