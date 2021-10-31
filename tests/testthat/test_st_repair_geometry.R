context("st_repair_geometry")

test_that("valid geometry (defined crs)", {
  skip_on_cran()
  # define data
  p1 <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  # repair geometry
  p2 <- st_repair_geometry(p1)
  p3 <- sf::st_make_valid(p1)
  # tests
  expect_is(p2, "sf")
  expect_named(p2, names(p1))
  expect_true(all(sf::st_is_valid(p2)))
  expect_equivalent(p2, p3, tolerance = 1e-5)
})

test_that("valid geometry (NA crs)", {
  skip_on_cran()
  # define data
  p1 <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  sf::st_crs(p1) <- sf::st_crs(NA)
  # repair geometry
  p2 <- st_repair_geometry(p1)
  p3 <- sf::st_make_valid(p1)
  # tests
  expect_is(p2, "sf")
  expect_named(p2, names(p1))
  expect_true(all(sf::st_is_valid(p2)))
  expect_equivalent(p2, p3, tolerance = 1e-5)
})

test_that("invalid geometry (prepair not needed)", {
  skip_on_cran()
  # define data
  p1 <- st_sf(
    id = 1,
    geometry = sf::st_as_sfc(
      "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))",
      crs = sf::st_crs(3857)
    )
  )
  # repair geometry
  p2 <- st_repair_geometry(p1)
  p3 <- sf::st_make_valid(p1)
  # tests
  expect_is(p2, "sf")
  expect_named(p2, names(p1))
  expect_true(all(sf::st_is_valid(p2)))
  expect_equal(p2, p3)
})
