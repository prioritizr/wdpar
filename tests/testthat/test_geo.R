context("geo")

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
