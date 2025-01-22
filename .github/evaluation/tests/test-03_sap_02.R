context("exercises in 03_sap.R")

test_that("Exercise 03: b: object tests ", {
  exerciseTest("3.2")
  expect_data_frame(ex0303b, nrows = 11, ncols = 2)
  expect_set_equal(colnames(ex0303b), c("LIFNR", "NETWR"))
  expect_equal(sd(ex0303b$NETWR), 66057.89, tolerance = 0.0001)
  expect_equal(ex0303b$LIFNR[3], 52198)
})
