context("Package Functions - mds_flagmiss.R")

#load data
  mds_dta <- mdsR::mds_dta
  test <- mds_flagmiss(mds_dta, .quietly = T, .table = T)

test_that("mds_flagmiss executes", {
  expect_output(mds_flagmiss(mds_dta, .quietly = F, .table = F))
})

test_that("mds_flagmiss output correct", {
  ## test function
  expect_equal(ncol(test), 2L)
  expect_true(all(names(test) %in% c('varlist', 'count_na')))
  expect_equal(class(test$varlist), 'factor')
  expect_equal(class(test$count_na), 'numeric')
  expect_true(max(test$count_na)<=1)
  expect_true(min(test$count_na)>=0)
})
