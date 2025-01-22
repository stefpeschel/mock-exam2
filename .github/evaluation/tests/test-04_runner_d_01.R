context("exercises in 04_runner.R")

test_that("Exercise 04: d: Examples work", {
  exerciseTest("4.41")
  example_deck <- list(
    card = data.frame(colour = "green", number = 12),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "smaller")

  example_deck <- list(
    card = data.frame(colour = "green", number = 4),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "larger")
})
