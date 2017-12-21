context("internal")

test_that("distribute_load", {
  expect_equal(distribute_load(5, 1), list(seq(1, 5)))
  expect_equal(distribute_load(10, 2), list(seq(1, 5), seq(6, 10)))
  expect_equal(distribute_load(10, 3), list(seq(1, 4), seq(5, 7), seq(8, 10)))
})

test_that("country_code", {
  expect_equal("LIE", country_code("LIE"))
  expect_equal("LIE", country_code("Liechtenstein"))
  expect_error(country_code("Does not exist"))
  expect_error(country_code("ZZZ"))
})
