context("exercises in 04_runner.R")

test_that("Exercise 04: a: check object", {
  exerciseTest("4.1")
  expect_data_frame(ex04adeck, ncols = 2L, nrows = 60)
  expect_set_equal(colnames(ex04adeck), c("colour", "number"))
  expect_set_equal(sum(ex04adeck$number), 480L)
  expect_set_equal(mean(table(ex04adeck$colour)), 15L)
})
