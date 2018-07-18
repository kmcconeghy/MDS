context("Package Functions - mds_misc.R")

test_that("mds_des executes", {
  #load data
  df <- mdsR::mds_dta

  ## test function
  expect_is(mds_des(mds_dta), "list")

})
