context("Package Functions - mds_mkmds.R")

test_that("mk_repid() expected result", {

  test <- mk_repid()

  ## test function
  expect_equal(class(test), 'character')#is a string
  expect_equal(length(test), 1L) #is length 1
  expect_equal(nchar(test), 13L) #string is length 10
})

test_that("mk_beneid() expected result", {
  test <- mk_beneid()

  ## test function
  expect_equal(class(test), 'character')#is a string
  expect_equal(length(test), 1L) #is length 1
  expect_equal(nchar(test), 16L) #string is length 10
})

test_that("mk_accptid() expected result", {
  test <- mk_accptid()

  ## test function
  expect_equal(class(test), 'character')#is a string
  expect_equal(length(test), 1L) #is length 1
  expect_equal(nchar(test), 6L) #string is length 10
})

test_that("mk_provid() expected result", {
  test <- mk_repid()
  test <- mk_provid(test)

  ## test function
  expect_equal(class(test), 'character')#is a string
  expect_equal(length(test), 1L) #is length 1
  expect_equal(nchar(test), 13L) #string is length 10
})
