context("Package Functions - mds_rpt_xls.R")

test_that("mds_rpt_xls executes", {
  #load data
  df <- mdsR::mds_dta

  ## test function
  expect_output(mds_rpt_xls(mds_dta))
})
