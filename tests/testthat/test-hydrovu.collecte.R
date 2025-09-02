library(testthat)
library(afd39)

test_that("hydrovu.collecte format données", {
  expect_error(hydrovu.collecte("5327553842264847", "BONlac"))
  expect_error(hydrovu.collecte("5327553842264847"))
  expect_error(hydrovu.collecte(chsta_coderhj = "BONlac"))
  expect_error(hydrovu.collecte(modem = "852311"))
  expect_error(hydrovu.collecte(capteur = "794849"))
})