library(testthat)
source("/Users/tobiasmatz/Desktop/AoC 2019/Day1/day1.R")

context("Light weight baby")

test_that("Test function for calculate_initial_weight of fuel", {
  expect_equal(calculate_initial_weight(12), 2)
  expect_equal(calculate_initial_weight(14), 2)
  expect_equal(calculate_initial_weight(1969), 654)
  expect_equal(calculate_initial_weight(100756), 33583)
})

test_that("Test function for calculate_total_weight of fuel", {
  expect_equal(calculate_total_weight(14), 2)
  expect_equal(calculate_total_weight(1969), 966)
  expect_equal(calculate_total_weight(100756), 50346)
})
