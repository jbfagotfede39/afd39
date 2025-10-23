library(testthat)
library(afd39)

test_that("table.recuperation format données", {
  expect_error(table.recuperation())
  expect_error(table.recuperation("fzeefzf"))
})