context("exercises in 04_runner.R")

test_that("Exercise 04: d: Additional tests", {
  exerciseTest("4.43")
  example_deck <- list(
    card = data.frame(colour = "green", number = 7),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "larger")
  example_deck <- list(
    card = data.frame(colour = "green", number = 9),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  expect_vector_function_equal(ex04decide_prior, card = example_deck$card,
                               deck = example_deck$deck, .expected = "smaller")
  example_deck <- list(
    card = data.frame(colour = "green", number = 8),
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  expect_subset(ex04decide_prior(card = example_deck$card, deck = example_deck$deck),
                c("larger", "smaller"))
})
