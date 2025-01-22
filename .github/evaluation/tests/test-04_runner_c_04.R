context("exercises in 04_runner.R")

expect_options <- function(object, options, input, fname) {
  act <- quasi_label(rlang::enquo(object), arg = "object")
  compResults <- purrr::map_lgl(options, ~identical(act$val, .x))
  if (!any(compResults)) {
    expect(
      any(compResults),
      paste0("\n With the input deck = \n \n", printToText(input), "\n the function ",
             fname, " should produce one of the following outputs: \n \n", printToText(options),
             "\n Your function produced instead: \n \n", printToText(object))
    )
  } else {
    expect_equal(1, 1)
  }
  invisible(act$val)
  
}

test_that("Exercise 04: c: seed and randomness", {
  exerciseTest("4.34")
  example_deck <- list(
    card = NULL,
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  example_deck$deck <- example_deck$deck[order(example_deck$deck$number),]
  after_draw <- ex04draw(example_deck)
  expect_data_frame(after_draw$card, nrows = 1L, ncols = 2L)
  expect_set_equal(colnames(after_draw$card), c("colour", "number"))
  expect_data_frame(after_draw$deck, nrows = 3L, ncols = 2L)
  expect_set_equal(colnames(after_draw$deck), c("colour", "number"))
  combined_deck <- rbind(after_draw$card, after_draw$deck)
  expect_identical(combined_deck[order(combined_deck$number), ],
                   example_deck$deck)
  # random works
  after_draw2 <- ex04draw(example_deck)
  after_draw3 <- ex04draw(example_deck)
  after_draw4 <- ex04draw(example_deck)
  after_draw5 <- ex04draw(example_deck)
  identical2 <- identical(after_draw, after_draw2)
  identical3 <- identical(after_draw, after_draw3)
  identical4 <- identical(after_draw, after_draw4)
  identical5 <- identical(after_draw, after_draw5)
  if ((identical2 + identical3 + identical4 + identical5) == 4) {
    stop("Your function does not seem to produce random but deterministic draws. 5 draws in a row were identical.")
  }
  # seed works
  after_draw <- ex04draw(example_deck, seed = 1L)
  after_draw2 <- ex04draw(example_deck, seed = 1L)
  after_draw3 <- ex04draw(example_deck, seed = 1L)
  after_draw4 <- ex04draw(example_deck, seed = 1L)
  after_draw5 <- ex04draw(example_deck, seed = 1L)
  identical2 <- identical(after_draw, after_draw2)
  identical3 <- identical(after_draw, after_draw3)
  identical4 <- identical(after_draw, after_draw4)
  identical5 <- identical(after_draw, after_draw5)
  if ((identical2 + identical3 + identical4 + identical5) != 4) {
    stop("Your function does not seem to be reproducible (seed). When a seed is set your function did not produce identical when repeated.")
  }
})
