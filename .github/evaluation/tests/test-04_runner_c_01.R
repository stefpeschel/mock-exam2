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

test_that("Exercise 04: c: Tests examples, empty deck works, correcting for randomness", {
  exerciseTest("4.31")
  example_deck <- list(
    card = NULL,
    deck = data.frame(colour = c("black", "orange", "white", "black"),
                      number = c(8, 9, 1, 3)),
    history = NULL)
  after_draw <- ex04draw(example_deck)
  possible_solutions <- list(
    list(card = example_deck$deck[1, , drop = FALSE],
         deck = example_deck$deck[-1, ,],
         history = NULL),
    list(card = example_deck$deck[2, , drop = FALSE],
         deck = example_deck$deck[-2, ,],
         history = NULL),
    list(card = example_deck$deck[3, , drop = FALSE],
         deck = example_deck$deck[-3, ,],
         history = NULL),
    list(card = example_deck$deck[4, , drop = FALSE],
         deck = example_deck$deck[-4, ,],
         history = NULL)
  )
  expect_options(after_draw, possible_solutions, example_deck, "ex04draw")
})
