library(testthat)
library(afd39)

test_that("topographie.habitats existence id", {
  expect_no_error(topographie.habitats())
  expect_no_error(topographie.habitats(34))
  expect_error(topographie.habitats(0))
  expect_error(topographie.habitats(100000))
})