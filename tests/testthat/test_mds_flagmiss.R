context("Package Functions - mds_flagmiss.R")

#load data
  mds_dta <- mdsR::mds_dta
  test <- mds_flagmiss(mds_dta, .quietly = T, .table = T)

test_that("mds_flagmiss executes", {
  expect_output(mds_flagmiss(mds_dta, .quietly = F, .table = F))
})

test_that("mds_flagmiss output correct", {
  ## test function
  expect_equal(ncol(test), 3L)
  expect_true(all(names(test) %in% c('varlist', 'count_na', 'prop_na')))
  expect_equal(class(test$varlist), 'factor')
  expect_equal(class(test$count_na), 'integer')
  expect_equal(class(test$prop_na), 'numeric')
  expect_true(max(test$prop_na)<=1)
  expect_true(min(test$prop_na)>=0)
})
