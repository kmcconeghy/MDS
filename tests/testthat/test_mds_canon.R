context("Package Functions - mds_canon.R")

test_that("mds_as_canon executes", {
  #load data
  df <- mdsR::mds_dta

  ## test function
  expect_output(mds_as_canon(mds_dta, .report=T))
})

test_that("mds_is_canon executes", {
  #load data
  df <- mdsR::mds_dta

  ## test function
  expect_equal(mds_is_canon(mds_dta), T)
})
