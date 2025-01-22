### helper file for printers and output string creating functions


# function to simply print the call of the exercise function for the test ouput
# without printing the arguments seperately
#TODO problem with R6 handling? 
deparseCall <- function(fname, args) {
  
  out <- paste0(deparse(as.call(c(list(as.symbol(fname)), args)),
                        control = c("niceNames", "showAttributes"), width.cutoff = 80),
                collapse = "\n")
  
  gsub("<environment>", "<Random R6 Object>", out)
}


deparseOne <- function(x) {
  paste(deparse(x, control = c("niceNames", "showAttributes")), collapse = "\n")
}


# function to print various input arguments or outputs nicely within the Rstudio markdown output
printToText <- function(x) {
  max <-  50
  if (inherits(x, "data.table")) {
    printer = data.table:::print.data.table
  } else if (inherits(x, "data.frame")) {
    printer = base:::print.data.frame
    max <- 20 * ncol(x)
  } else if (inherits(x, "list") && length(x) > 0 && inherits(x[[1]], "R6")) {
    printer = function(x, ...) lapply(x, function(y) capture.output(print(y)))
    #TODO keeping this in for R6 stuff, think that this doesnt work perfectly yet
  } else if (inherits(x, "function")) {
    if (is.primitive(x)) {
      printer = function(x, ...) noquote(capture.output(print(x, ...))[[1]])
    } else {
      printer = function(x,...) {
        environment(x)<-globalenv();
        base::print.function(x)
      }
    }
  } else {
    printer = print
    max <- getOption("max.print")
  }
  paste(capture.output(printer(x, nrows = 100, max = max)), collapse = "\n")
  #TODO 'nrow' was renamed to 'nrows' (for data.table printer)
  # not sure why or for what 'nrow' was used previously
  # so perhaps this is the issue, if printer smth ever exceeds 100 rows
}


# function to make test input arguments printable and nice
make.Called.with <- function(fun, inputs, fname = NULL) {
  if (is.null(fname)) fname <- as.character(substitute(fun))
  #TODO not sure why all the checks are required
  input.description <- sapply(inputs,
                              function(x) if (inherits(x, "list") &&
                                              (length(x) == 0 || (is.atomic(x[[1]]) && !is.matrix(x[[1]]) &&
                                                                  !is.array(x[[1]])))) deparseOne(x) else printToText(x))
  if (is.null(names(input.description))) {
    names(input.description) <- sprintf("Argument %s", seq_along(input.description))
  }
  # case for not all inputs named
  names(input.description)[which(names(input.description) == "")] <-
    sprintf("Argument %s", which(names(input.description) == ""))
  
  paste0(fname, " was called with the following arguments:
                        \n",
         if(length(input.description) == 0) "No Arguments passed" else { paste(
           sprintf(ifelse(nchar(input.description) + nchar(names(input.description)) < 70 &
                            !grepl("\n", input.description, fixed = TRUE),
                          "%s: %s\n", "%s:\n%s"), names(input.description), input.description),
           collapse = "\n")})
}


# function to create nice output and detailed error msg
# input students fun result of test, expected results and input arguments in the form that 'make.Called.with' creates
# additional specific error information can also be passed
# as well as additional testthat arguments that impact 'expect_equal()'
#TODO do we really need both 'addendum' and 'add.err.info' (also 'info' ?)
errorOutput <- function(functionResult, .expected, called.with, ...,
                        addendum = "", add.err.info = "",
                        tolerance = if (edition_get() >= 3) testthat_tolerance(),
                        .ignore_attr = FALSE, info = NULL, .testthat_fun = "expect_equal") {
  if(is.null(info)) info <- sprintf("\n %s\n
    Your function produced:\n%s\n
    The expected result was:\n%s\n
    %s %s
    Check the top of this error message for more
    info if the difference is not obvious on sight.", paste(called.with, collapse = "\n"),
                                    printToText(functionResult), printToText(.expected), addendum, add.err.info)
  get(.testthat_fun)(functionResult, .expected, ..., tolerance = tolerance, ignore_attr = .ignore_attr,
               info = info)
}