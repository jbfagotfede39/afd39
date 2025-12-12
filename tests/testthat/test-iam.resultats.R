library(testthat)
library(afd39)

test_that("iam.resultats existence id", {
  expect_no_error(iam.resultats())
  expect_no_error(iam.resultats(34))
  expect_error(iam.resultats(0))
  expect_error(iam.resultats(100000))
})