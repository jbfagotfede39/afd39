library(testthat)
library(afd39)

test_that("topographie.mesures existence id", {
  expect_no_error(topographie.mesures())
  expect_no_error(topographie.mesures(23))
  expect_error(topographie.mesures(0))
  expect_error(topographie.mesures(100000))
})