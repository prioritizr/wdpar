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
  expect_equivalent(y1[["geometry"]], y2[["geometry"]])
})

test_that("st_subset_polygons", {
  # make data
  ## point
  po <- sf::st_point(c(1, 2))
  ## line
  lin <- sf::st_linestring(matrix(1:8, , 2))
  ## polygon
  outer <- matrix(c(0, 0, 8, 0, 8, 8, 0, 8, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  pl1 <- sf::st_polygon(list(outer))
  pl2 <- sf::st_polygon(list(outer + 20, hole1 + 20, hole2 + 20))
  ## multi-polygon
  mpl <- sf::st_multipolygon(list(list(outer + 30, hole1 + 30, hole2 + 30),
                                  list(outer + 40, hole1 + 40),
                                  list(outer + 50)))
  ## geometry collections
  geomcol1 <- sf::st_geometrycollection(list(pl1 + 60, po + 60, pl2 + 60,
                                             lin + 60, mpl + 60))
  geomcol2 <- sf::st_geometrycollection(list(po + 70, lin + 70))
  geomcol3 <- sf::st_geometrycollection(list(pl1 + 80))
  geomcol4 <- sf::st_geometrycollection()
  # create sf object
  x <- sf::st_sf(label = letters[1:9], geometry = sf::st_sfc(po, pl1, lin, pl2,
                                                             mpl, geomcol1,
                                                             geomcol2, geomcol3,
                                                             geomcol4))
  # subset polygons
  y <- st_subset_polygons(x)
  # run tests
  expect_equal(rownames(y), as.character(c(2, 4, 5, 6, 8)))
  expect_equal(y$label, x$label[c(2, 4, 5, 6, 8)])
  expect_equal(sf::st_crs(x), sf::st_crs(y))
  expect_equal(sf::st_geometry(y)[[1]], pl1)
  expect_equal(sf::st_geometry(y)[[2]], pl2)
  expect_equivalent(sf::st_geometry(y)[[3]], mpl)
  expect_equivalent(sf::st_geometry(y)[[4]],
                    sf::st_multipolygon(list(
                      pl1 + 60, pl2 + 60,
                      sf::st_polygon(list(outer + 30 + 60, hole1 + 30 + 60,
                                          hole2 + 30 + 60)),
                      sf::st_polygon(list(outer + 40 + 60, hole1 + 40 + 60)),
                      sf::st_polygon(list(outer + 50 + 60)))))
  expect_equal(sf::st_geometry(y)[[5]], pl1 + 80)
})
