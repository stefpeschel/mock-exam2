context("exercises in 03_sap.R")

test_that("Exercise 03: a: object tests ", {
  exerciseTest("3.1")
  expect_data_frame(ex0303a, nrows = 1673, ncols = 8)
  expect_set_equal(colnames(ex0303a), c("MANDT", "EBELN", "EBELP", "MATGR",
                                        "WERKS", "MENGE", "NETWR", "WAERS"))
  expect_equal(sum(ex0303a$EBELP), 4410)
  expect_equal(table(ex0303a$WAERS)["GBP"], c(GBP = 31))
  expect_equal(sd(ex0303a$NETWR), 51335.21, tolerance = 0.0001)
})
