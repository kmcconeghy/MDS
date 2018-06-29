context("Package Data Files - Dictionary")

test_that("mdsR Dictionary properly formatted", {
  #load data
  df <- mdsR::mds_varlist

  df_names <- c("item",'standard', "label", "description", "class",
              "category", 'SetMissToZero',
              "default_ref", "default_label")

  #Tests
  expect_s3_class(df, "data.frame")
  expect_that(df_names, equals(names(df)))
  expect_type(df$item, "character")
  expect_type(df$label, "character")
  expect_type(df$description, "character")
  expect_type(df$class, "character")
  expect_type(df$category, "character")
  expect_type(df$SetMissToZero, 'logical')
  expect_type(df$default_ref, "character")
  expect_type(df$default_label, "character")
})
