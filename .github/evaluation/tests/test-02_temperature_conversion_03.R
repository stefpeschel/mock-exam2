context("exercises in 02_temperature_conversion.R")

test_that("Exercise 02: additional tests : temperature_conversion ", {
  exerciseTest("2.3")
  expect_vector_function_equal(ex02temperature_conversion, 
                               temp = 3,
                               .expected = -16.111111,
                               tolerance = 0.0001)
  expect_error(ex02temperature_conversion(c(4, 3, 3.1), unit = c("F", "C", "F")))
})
