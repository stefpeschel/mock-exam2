### main body helper for exercise tests


# called when checking class of a result
classCheck <- function(functionResult, class, called.with) {
  test <- test_class(functionResult, class)
  if(test){
    return(NULL)
  } else {
    class_Check <- FALSE
    expect_true(class_Check,  info = sprintf("\n
    The output of your function is of class: %s\n
    but should be: %s\n
    %s", paste(class(functionResult), collapse = ", "), paste(class, collapse = ", "), called.with))
  }
}


# general wrapper fun for any 'expect_...' funs
# calls 'make.Called.with' within, so pass list of raw test arguments as 'inputs'
# handles errors during testing and returns list of student fun result, called with and expected result
expect_function_equal <- function(fun, inputs = NULL, .expected = NULL, fname = NULL, .capture_output = FALSE) {
  if(is.null(fname)) fname <- as.character(substitute(fun))
  
  called.with <- make.Called.with(fun, inputs, fname)
  
  #TODO eval fun as fun otherwise parenthesis always have to be supplied
  withCallingHandlers(functionResult <- if(.capture_output) {
    capture_output(if (!length(inputs)) eval(fun) else do.call(fun, inputs))
    } else {
      if (!length(inputs)) eval(fun) else do.call(fun, inputs)
      },
                      error = function(e) {
                        functionError(called.with, e$message)
                      },
                      warning = function(w) {
                        functionError(called.with, w$message,
                                      e.type = "warning")
                      })
  
  list(functionResult = functionResult, called.with = called.with, expected = .expected)
}


# function to be used on tests where there are multiple possible solutions
# so use as a wrapper around other funs found here
# supply a 'expect_*' like test function as use.fun
# fn as the exercise function and choices as the possible correct results
# supply arguments via ...
expect_choice_function <- function(use.fun, fun, ..., choices, hint = "") {
  
  funOut <- use.fun(fun, ..., .expected = NULL, .output.result = TRUE)
  
  for (ci in seq_along(choices)) {
    cs <- choices[[ci]]
    skip_to_next <- FALSE
    tryCatch(expect_equal(funOut, cs),
             error = function(e) skip_to_next <<- TRUE)
    if(skip_to_next) {
      if(ci == length(choices)) {
        errorOutput(funOut, "One of the below",
                    called.with = NULL,
                    info = sprintf("\n%s\nYour function produced:\n%s\n\nbut should have been one of the following:\n%s\n\n%s",
                                   make.Called.with(fun,
                                                    fname = as.character(substitute(fn)),
                                                    list(...)),
                                   printToText(funOut),
                                   printToText(choices), hint))
      } else next
    } else break
  }
}


#TODO this deffo needs an overhaul, not sure what this is used for actually
# Addinputs needs to be a named list
expect_equal_hash <- function(fun, fname, ..., Addinputs = NULL, hashRes) {
  
  inputs <- list(...)
  
  input.description <- sapply(inputs, printToText)
  if (is.null(names(input.description))) {
    names(input.description) <- sprintf("Argument %s", seq_along(input.description))
  }
  # case for not all inputs named
  names(input.description)[which(names(input.description) == "")] <-
    sprintf("Argument %s", which(names(input.description) == ""))
  
  called.with <- paste0(as.character(substitute(fname)), " was called with the following arguments:
                        \n",
                        if(length(input.description) == 0) "No Arguments passed" else { paste(
                          sprintf(ifelse(nchar(input.description) + nchar(names(input.description)) < 70 &
                                           !grepl("\n", input.description, fixed = TRUE),
                                         "%s: %s\n", "%s:\n%s\n"), names(input.description), input.description),
                          collapse = "\n")})
  
  
  fRes <- if (is.null(Addinputs)) {
    eval(fun)(fname, ..., .expected = NULL, .output.result = TRUE)
  } else {
    # maybe this dont work yet, havent tested, dont have time rn
    do.call(eval(fun)(fname, ..., .expected = NULL, .output.result = TRUE), Addinputs)
  }
  
  expect_equal(digest::digest(fRes, algo = "sha256", serialize = FALSE, ascii = TRUE),
               hashRes, label = "funtionResult",
               info = sprintf("\nYour funtion generated the output \n %s
                             \nwhich is not the expected output. \n%s",
                              printToText(fRes), called.with))
}


#TODO is this only a remnant and can be deleted?
meta_expect <- function(expect, lhs, ..., .postprocess = identity) {
  argcall <- substitute(lhs)
  fname <- as.character(argcall[[1]])
  argcall[[1]] <- as.symbol("list")
  inputs <- eval.parent(argcall)
  
  for (i.arg in seq_along(inputs)) {
    if (data.table::is.data.table(inputs[[i.arg]])) {
      inputs[[i.arg]] <- copy(inputs[[i.arg]])
    }
  }
  
  input.description <- sapply(inputs, function(i) {
    printToText(i)
  })
  if (is.null(names(input.description))) {
    names(input.description) <- sprintf("Argument %s", seq_along(input.description))
  }
  called.with <- paste0(fname, " was called with the following arguments:\n",
                        paste(
                          sprintf(ifelse(nchar(input.description) + nchar(names(input.description)) < 70 &
                                           !grepl("\n", input.description, fixed = TRUE),
                                         "%s: %s\n", "%s:\n%s\n\n"), names(input.description), input.description),
                          collapse = ""),
                        "\n")
  expect(.postprocess(lhs), ..., info = called.with)
}


#TODO i think this is simply used in 'evaluate_submission.R' and should therefore maybe not be here
exerciseTest <- function(exname) {
  assertString(exname, pattern = "^[0-9]+(\\.[0-9]+)?$")
  exgroup <- sub("\\..*", "", exname)
  requestedgroups <- sub("\\..*", "", EXERCISES)
  if (length(EXERCISES) && !exname %in% EXERCISES && !exgroup %in% EXERCISES && !exname %in% requestedgroups) {
    skip(sprintf("Skipping exercise %s", exname))
  }
}


