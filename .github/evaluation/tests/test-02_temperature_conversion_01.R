context("exercises in 02_temperature_conversion.R")

test_that("Exercise 02: examples : temperature_conversion", {
  exerciseTest("2.1")
  expect_vector_function_equal(ex02temperature_conversion,
                               temp = c(212, 32),
                               .expected = c(100, 0),
                               tolerance = 0.0001)
  expect_vector_function_equal(ex02temperature_conversion,
                               temp = 100,
                               unit = "C",
                               .expected = 212,
                               tolerance = 0.0001)
 })