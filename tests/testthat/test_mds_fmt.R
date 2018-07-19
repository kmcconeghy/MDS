context("Package Functions - mds_fmt.R")

test_that("mds_fmt executes", {
  #load data
  df <- mdsR::mds_dta

  ## test function
  expect_output(mds_fmt(mds_dta, .quietly=T))
})
