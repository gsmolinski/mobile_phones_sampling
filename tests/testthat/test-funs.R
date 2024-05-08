library(testthat)
source("R/funs.R")

prefixes_sample <- c("Orange" = 4500, "T-mobile" = 45021, "Play Sp. z o.o." = 452)

test_that("fill_number works", {
  expect_equal(fill_number(0, prefixes_sample[[1]]), 450000000)
  expect_equal(fill_number(9, prefixes_sample[[1]]), 450099999)
  expect_equal(fill_number(0, prefixes_sample[[2]]), 450210000)
  expect_equal(fill_number(0, prefixes_sample[[3]]), 452000000)
})

test_that("generate_numbers works", {
  result <- lapply(prefixes_sample, generate_numbers)
  expect_length(result, length(prefixes_sample))
  expect_named(result, names(prefixes_sample))
  expect_type(unlist(result), "integer")
})

test_that("draw_sample works", {
  result <- draw_sample(2, prefixes_sample)
  expect_length(result, 2)
  expect_named(result)
  expect_type(result, "integer")
  prefixes_sample <- c("Orange" = 11111111)
  expect_length(draw_sample(100000, prefixes_sample), 10)
})

test_that("transform_to_df works", {
  result <- transform_to_df(prefixes_sample)
  expect_s3_class(result, "data.frame")
  expect_length(result, 2) # number of columns
})
