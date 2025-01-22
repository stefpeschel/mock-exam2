### helper file for all output class specific extensions of the expect_function_equal() fun


# function for tests where the desired exercise function output is a vector, list or an atomic
# supply the exercise fun as 'fun', the expected output and arguments to be supplied to the function via ... . 
# If no check is to be performed and you simply want the students function result, set '.expected' to NULL
# and '.output.result' to TRUE. Use this so errors are handles well and the printed output is nice.
# supply 'length' or 'class' to check for those separately. Use this for a more direct failure msg.
#TODO why remove label? what is it actually for? 
#TODO think i know what its for, not sure what removing refers to anymore
expect_vector_function_equal <- function(fun,  ..., .expected, class = "noCheck",
                                         length = FALSE,
                                         tolerance = if (edition_get() >= 3) testthat_tolerance(),
                                         .ignore.order = FALSE, add.err.info = "",
                                         .output.result = FALSE, .ignore_attr = FALSE,
                                         info = NULL, label = NULL, .capture_output = FALSE,
                                         .testthat_fun = "expect_equal") {
  inputs <- list(...)
  fname <- if (!is.null(label)) {
    label
  } else {
    as.character(substitute(fun))
  }
  params <- expect_function_equal(fun, inputs, .expected, fname, .capture_output)
  
  functionResult <- params[["functionResult"]]
  called.with <- params[["called.with"]]
  
  if(class != "noCheck") {
    classCheck(functionResult, class, called.with)
  }
  if(!isFALSE(length)) expect_equal(length(functionResult), length, info = sprintf("\n
    The output of your function is of length: %s\n
    but should be: %s\n
    %s", length(functionResult), length, called.with))
  
  addendum <- ""
  
  if(.ignore.order){
    if (!is.null(names(functionResult)) && !is.null(names(.expected))) {
      functionResult <- functionResult[order(names(functionResult))]
      .expected <- .expected[order(names(.expected))]
    }
    functionResult <- sort(functionResult, method = "shell")  # shell sorting is stable for names
    .expected <- sort(.expected, method = "shell")
    addendum <- "(outputs were sorted, as order doesnt matter here)"
  }
  
  if(addendum != "") addendum <- paste(addendum, "\n")
  if(add.err.info != "") add.err.info <- paste(add.err.info, "\n")
  
  if(.output.result) return(functionResult)
  errorOutput(functionResult, .expected = .expected, called.with = called.with, addendum = addendum, label = label,
              add.err.info = add.err.info, tolerance = tolerance, .ignore_attr = .ignore_attr, info = info,
              .testthat_fun = .testthat_fun)
}


# function for tests where the desired exercise function output is a matrix. also works for data.frame.
# supply the exercise fun as 'fun', the expected output and arguments to be supplied to the function via ... . 
# If no check is to be performed and you simply want the students function result, set 'expected' to NULL
# and '.output.result' to TRUE. Use this so errors are handles well and the printed output is nice.
# supply or 'class' to check for that separately. Use this for a more direct failure msg.
#TODO why remove label and tolerance?
#TODO first part of code is duplicate from vector fun
expect_matrix_function_equal <- function(fun, ..., .expected, class = "noCheck",
                                         .ignore.row.order = FALSE,
                                         .ignore.col.order = FALSE,
                                         add.err.info = "",
                                         .output.result = FALSE, label = NULL,
                                         .ignore_attr = FALSE,
                                         tolerance = if (edition_get() >= 3) testthat_tolerance()) {
  inputs <- list(...)
  fname <- if (!is.null(label)) {
    label
  } else {
    as.character(substitute(fun))
  }
  params <- expect_function_equal(fun, inputs, .expected, fname)
  
  functionResult <- params[["functionResult"]]
  called.with <- params[["called.with"]]
  
  if(class != "noCheck") {
    classCheck(functionResult, class, called.with)
  }
  
  #handling of ignore row or col order
  addendum <- ""
  if(is.matrix(.expected) && is.matrix(functionResult) ||
     inherits(.expected, "data.frame") && inherits(functionResult, "data.frame")){
    if(ncol(.expected) == ncol(functionResult) && !is.null(colnames(functionResult))){
      if(.ignore.col.order){
        .expected <- .expected[, sort(colnames(.expected))]
        functionResult <- functionResult[, sort(colnames(functionResult))]
        addendum <- "(columns were sorted by name because the exercise ignores column order)\n"
      }
      if(.ignore.row.order){
        .expected <- .expected[do.call(order, as.data.frame(.expected)), ]
        rownames(.expected) <- NULL
        functionResult <- functionResult[do.call(order, as.data.frame(functionResult)), ]
        rownames(functionResult) <- NULL
        # handling for different column order shouldnt matter here, as it should ultimately
        # throw an error if col order is different or it was sorted above if not important
        if (addendum != "") {
          addendum <- "(rows and cols were sorted by content because the exercise ignores row and column order)\n"
        } else {
          addendum <- "(rows were sorted by name because the exercise ignores row order)\n"
        }
      }
    }
  }
  if(addendum != "") addendum <- paste(addendum, "\n")
  if(add.err.info != "") add.err.info <- paste(add.err.info, "\n")
  
  if(.output.result) return(functionResult)
  errorOutput(functionResult, .expected = .expected, called.with = called.with,
              addendum = addendum, add.err.info = add.err.info, tolerance = tolerance,
              iggnore_attr = ignore_attr)
}


# test fun check for function output exercises
# checks that output is correct and is a function
# returns ouput function if '.output.result' is given
#TODO seems sorta not quite right, checks fun_equal and hen class (does that make sense?)
#TODO naming scheme of .output things is different, make this same for all
expect_fn_function_equal <- function(fun, ..., .expected = NULL,
                                     .output.result = FALSE, add.err.info = "",
                                     info = NULL) {
  
  inputs <- list(...)
  fname <- as.character(substitute(fun))
  params <- expect_function_equal(fun, inputs, .expected, fname)
  
  functionResult <- params[["functionResult"]]
  called.with <- params[["called.with"]]
  
  classCheck(functionResult, "function", called.with)
  if(.output.result) return(functionResult)
  errorOutput(functionResult, .expected = .expected, called.with = called.with,
              add.err.info = add.err.info)
}

# wrapper function for tests that expect a data.table as output
# supply the exercise fun as 'fun', the expected output and arguments to be supplied to the function via ... . 
# If no check is to be performed and you simply want the students function result, set '.expected' to NULL
# and '.output.result' to TRUE. Use this so errors are handles well and the printed output is nice.
# There are additional arguments to check or ignore other things like order, colnames and attributes.
# ignore.row.order may be 'true': ignore all, or character: listing of cols to ignore order of
expect_dt_function_result <- function(fun, ..., .expected, tolerance = if (edition_get() >= 3) testthat_tolerance(),
                                      .ignore.row.order = FALSE, .ignore.col.order = FALSE, add.err.info = "",
                                      .ignore_attr = FALSE, .output.result = FALSE,
                                      .expect.colNames = character(0)) {
  inputs <- list(...)
  for (i.arg in seq_along(inputs)) {
    if (data.table::is.data.table(inputs[[i.arg]])) {
      inputs[[i.arg]] <- copy(inputs[[i.arg]])
    }
  }
  fname <- as.character(substitute(fun))
  
  params <- expect_function_equal(fun, inputs, .expected, fname)
  
  fresult <- params[["functionResult"]]
  called.with <- params[["called.with"]]
  
  classCheck(fresult, "data.table", called.with)
  
  if (length(.expect.colNames)) {
    expect_vector_function_equal(function(x) all.equal(as.character(colnames(x)),
                                                       as.character(.expect.colNames)),
                                 fresult,
                                 .expected = TRUE,
                                 info =
                                   paste("\nThe colnames produced by your function are not correct.\n",
                                         sprintf("Your colnames: \n%s", printToText(colnames(fresult))),
                                         sprintf("The expected colnames: \n%s", printToText(.expect.colNames)),
                                         "\n", called.with, sep = "\n"))
  }
  
  fresult <- setindex(setkey(copy(fresult), NULL), NULL)
  .expected <- setindex(setkey(copy(.expected), NULL), NULL)
  
  addendum <- ""
  if (data.table::is.data.table(fresult) && data.table::is.data.table(.expected)) {
    
    #TODO why delete those next lines?
    # ignore list columns (and hope that non-list columns are enough..... TODO should be solved better.
    # fresult.orderable <- fresult[, !sapply(fresult, is.list), with = FALSE]
    # expected.orderable <- expected[, !sapply(expected, is.list), with = FALSE]
    
    if (.ignore.col.order) {
      addendum <- "(columns were sorted by name because the exercise ignores column order)\n"
      .co <- order(colnames(fresult))
      fresult <- fresult[, .co, with = FALSE]
      .co <- order(colnames(.expected))
      .expected <- .expected[, .co, with = FALSE]
    }
    if (!isFALSE(.ignore.row.order)) {
      if (addendum != "") {
        addendum <- "(rows and cols were sorted by content because the exercise ignores row and column order)\n"
      } else {
        addendum <- "(rows were sorted by name because the exercise ignores row order)\n"
      }
      if (isTRUE(.ignore.row.order)) {
        .od <- do.call(order, fresult)
        fresult <- fresult[.od]
        .od <- do.call(order, .expected)
        .expected <- .expected[.od]
      } else {
        .expected <- .expected[, {
          .od <- do.call(order, .SD)
          .SD[.od]
        }, by = setdiff(colnames(.expected), .ignore.row.order)][, colnames(.expected), with = FALSE]
        fresult <- fresult[, {
          .od <- do.call(order, .SD)
          .SD[.od]
        }, by = setdiff(colnames(fresult), .ignore.row.order)][, colnames(fresult), with = FALSE]
      }
    }
  }
  
  #TODO i think these two lines are simply so it prints better names.
  # could also be done by renaming them throughout in the first place
  functionResult <- copy(fresult)
  expectedResult <- copy(.expected)
  
  #TODO 'check_attributes' was removed here and '.ignore_attr' put back.
  # Not sure what 'check_attributes' was for. was changed on 20.07.2023
  # UPDATE: now causes problems in dt1 ex 2.2
  # check_atrributes seems to be deprecated but 'ignore_attr' doen not do the same thing ...
  #TODO this is super annoying, perhaps just use testthat v2?
  # solved in individual test for now
  if(.output.result) return(functionResult)
  errorOutput(functionResult, .expected = expectedResult, tolerance = tolerance,
              called.with = called.with, addendum = addendum, add.err.info = add.err.info,
              .ignore_attr = .ignore_attr)
}