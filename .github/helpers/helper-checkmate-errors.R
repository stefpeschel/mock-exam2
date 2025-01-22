### this file is to house all checkmate assertions or error checks a students function should provide
### also includes all unwanted error handling helper funs


# function to test if student's function asserts correctly as the exercise requires
# supply exercise functin name as 'fun', 'working.args' should be a named list of complete and correct arguments,
# 'tryouts' should be a list (of lists) for different combinations of arguments that should invoke checkmate assertions
#TODO write a fun to automatically create the basic checks somehow
expect_checkmate <- function(fun, working.args, tryouts) {
  docopy <- function(l) {
    lapply(l, function(x) if (data.table::is.data.table(x)) copy(x) else x)
  }
  
  fname <- as.character(substitute(fun))
  #TODO why was 'fun' just 'fn' in the next line?!?
  called.with <- make.Called.with(fun, working.args, fname = fname)
  
  for (tr in seq_along(tryouts)) {
    new.args <- working.args
    #TODO this is a fix for calling exercises with the value NULL, as that would delete the element otherwise
    # perhaps not wanted as a fix or maybe causes problems elsewhere
    if (is.null(tryouts[[tr]])) {
      new.args[names(tryouts)[[tr]]] <- list(NULL)
    } else {
      new.args[[names(tryouts)[[tr]]]] <- tryouts[[tr]]  
    }
    
    input.description <- sapply(new.args, printToText)
    
    # change printing style dependent on class of arguments
    info <- if (any(inherits(new.args, c("matrix", "data.frame", "data.table"))) ||
                nchar(deparseCall(fname, new.args)) > 120) {
      paste0("\nCalling the following should have generated a checkmate assertion error but didn't:\n"
             ,  paste0(fname, " was called with the following arguments:
                        \n",
                       if(length(input.description) == 0) "No Arguments passed" else { paste(
                         sprintf(ifelse(nchar(input.description) + nchar(names(input.description)) < 70 &
                                          !grepl("\n", input.description, fixed = TRUE),
                                        "%s: %s\n", "%s:\n%s"), names(input.description), input.description),
                         collapse = "\n")}),
             #TODO remnant?
             #deparseCall(fname, new.args)),
             "\n\n You should use checkmate assertXxx()-functions!
              \n If not obvious why, make sure to read the function description carefully!")
    }
    else {
      paste0(sprintf("\nCalling the following should have generated a checkmate assertion error but didn't:\n
                            \n %s \n",
                     deparseCall(fname, new.args)),
             "\n You should use checkmate assertXxx()-functions!
             \nIf not obvious why, make sure to read the function description carefully!")
    }
    
    #TODO remnant?
    # expect_error(withCallingHandlers(do.call(fun, docopy(new.args)),
    #                                  warning = function(w) {
    #                                    stop("Your function produced a warning!")
    #                                  }),
    #   regexp = "Assertion.*failed",
    #   label = "function Call",
    #   info = info
    # )
    
    expect_error(withCallingHandlers({
      do.call(fun, docopy(new.args))
    }, error = function(e) {
      function.name <- sapply(sys.calls(), function(x) {
        x <- x[[1]]
        if (is.symbol(x)) as.character(x) else ""
      })
      stop(errorCondition(conditionMessage(e),
                          class = if (any(grepl("^assert|^makeAssertion", function.name))) {
                            "thrown.by.checkmate"
                          } else {
                            "simpleError"
                          }
      ))
    }, warning = function(w) {
      stop("Your function produced a warning!")
    }),
    regexp = "Assertion.*failed",
    class = "thrown.by.checkmate",
    label = "your function",
    info = info
    )
  }
}


# function the test if the student's exercise function throws the correct error messages for specific inputs.
# use this if the exercise demands errors for specific inputs to the function.
# pass the exercise function as 'fun', arguments via ... .
# class of error can be checked with 'e.class' and a label (for the printed error output) passed with 'e.type'.
expect_specific_error <- function(fun, ..., e.type = ".*", e.class = NULL, regexp = NULL,
                                  hint = "If not obvious why, make sure to read the function description carefully!") {
  args <- list(...)
  fname <- as.character(substitute(fun))
  input.description <- sapply(args, function(i) {
    printToText(i)
  })
  if (is.null(names(input.description))) {
    names(input.description) <- sprintf("Argument %s", seq_along(input.description))
  }
  # case for not all inputs named
  names(input.description)[which(names(input.description) == "")] <-
    sprintf("Argument %s", which(names(input.description) == ""))
  
  #TODO just use the 'make.Called.with' function
  called.with <- paste0(fname, " was called with the following arguments:
                        \n",
                        if(length(input.description) == 0) "No Arguments passed" else { paste(
                          sprintf(ifelse(nchar(input.description) + nchar(names(input.description)) < 70 &
                                           !grepl("\n", input.description, fixed = TRUE),
                                         "%s: %s\n", "%s:\n%s\n"), names(input.description), input.description),
                          collapse = "\n")})
  
  # change printing style dependent on class of arguments
  info <- if (suppressWarnings(any(lapply(args, function(x) inherits(x, c("matrix", "data.frame", "data.table"))))) ||
              nchar(deparseCall(fname, args)) > 120) {
    paste0(sprintf("\nCalling the following should have generated an %s error but didn't:\n\n%s\n",
                   if (e.type != ".*") paste0("'", e.type, "'") else "",
                   called.with),
           "\n", hint)
  } else {
    paste0(sprintf("\nCalling the following should have generated an %s error but didn't:\n\n%s\n",
                   if (e.type != ".*") paste0("'", e.type, "'") else "",
                   deparseCall(fname, args)),
           "\n", hint)
  }
  
  if(is.null(regexp)) regexp <- "^((?!warning).)*$"
  
  # includes a negative lookahead regex matching for "Error doesnt include warning"
  expect_error(withCallingHandlers(do.call(fun, args),
                                   warning = function(w) {
                                     stop(sprintf("warning produced by your function: %s", w$message))
                                   }),
               info = info,
               label = "function Call",
               regexp = regexp, perl = TRUE,
               class = e.class
  )
  #TODO has the arg name changed? regex vs regexp
}


# to be called, when function of student throws an error
# includes Error message
#TODO perhaps this could be improved with pinting the traceback or smth
functionError <- function(called.with, errorMes, e.type = "error") {
  stop(sprintf("%s produced by your function. \n %s \n %s message:\n %s", e.type,
               paste(called.with, collapse = "\n"), e.type, paste(errorMes, collapse = "\n")))
}


# function to capture function messages etc
# from https://www.r-bloggers.com/2020/10/capture-message-warnings-and-errors-from-a-r-function/
#TODO find out where this is used and why
capture_log1 <- function(f) {
  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(timestamp = format(Sys.time(), tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                      type = type,
                      message =  message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error=function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning=function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    list(res, logs = logs)
  }
}