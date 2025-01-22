context("exercises in 02_temperature_conversion.R")

test_that("Exercise 02: errors : temperature_conversion ", {
  exerciseTest("2.2")
  expect_error(ex02temperature_conversion(-9, unit = "CC"))
  expect_error(ex02temperature_conversion(-460))
})
