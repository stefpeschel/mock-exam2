context("exercises in 04_runner.R")

test_that("Exercise 04: d: Irrelevant input is passed", {
  exerciseTest("4.42")
  example_deck <- list(
    card = data.frame(colour = "green", number = 12, irrelevant = 1L),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3),
                      irrelevant = 1L),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "smaller")

  example_deck <- list(
    card = data.frame(colour = "green", number = 4, irrelevant = 1L),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3),
                      irrelevant = 1L),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "larger")
})
