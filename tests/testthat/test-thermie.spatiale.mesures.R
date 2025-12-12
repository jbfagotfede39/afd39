library(testthat)
library(afd39)

test_that("thermie.spatiale.mesures existence id", {
  expect_no_error(thermie.spatiale.mesures())
  expect_no_error(thermie.spatiale.mesures(1))
  expect_error(thermie.spatiale.mesures(0))
  expect_error(thermie.spatiale.mesures(100000))
})