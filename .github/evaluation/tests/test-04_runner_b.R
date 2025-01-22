context("exercises in 04_runner.R")

test_that("Exercise 04: b: check object", {
  exerciseTest("4.2")
  expect_set_equal(ex04bdeck$card, NULL)
  expect_set_equal(ex04bdeck$history, NULL)
  expect_data_frame(ex04bdeck$deck, ncols = 2L, nrows = 60)
  expect_set_equal(colnames(ex04bdeck$deck), c("colour", "number"))
  expect_set_equal(sum(ex04bdeck$deck$number), 480L)
  expect_set_equal(mean(table(ex04bdeck$deck$colour)), 15L)
})
