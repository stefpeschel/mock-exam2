### helper funs to check exercises where students write testthat tests


copyEnv <- function(env) {
  assertEnvironment(env)
  ne <- new.env(parent = parent.env(env))
  for (n in names(env)) {
    ne[[n]] <- env[[n]]
  }
  ne
}

testenvWithFunctionReplaced <- function(fname, newfun, swallow.errors = FALSE) {
  assertFunction(newfun, null.ok = TRUE)
  assertString(fname)
  assertFlag(swallow.errors)
  #TODO self
  testenv <- copyEnv(SELF)
  if (!fname %in% names(testenv)) {
    stop(sprintf("%s not found", fname))
  }
  #TODO self cant access self
  origfun <- SELF[[fname]]
  
  if (is.null(newfun)) {
    
    if (swallow.errors) {
      # swallowing errors should not pass tests
      testenv[[fname]] <- function(...) tryCatch(origfun(...), error = function(x) FALSE, warning = function(x) FALSE)
    } else {
      # test with orig fun to make sure this *does* pass tests if errors not swallowed
      testenv[[fname]] <- function(...) origfun(...)
    }
  } else {
    # must make sure errors from origfun get passed through.
    testenv[[fname]] <- function(...) { origfun(...) ; newfun(...) }
  }
  testenv
}

#TODO i believe this is used to check student written testthat tests might be in the wrong place
#TODO maybe should also be folded in with other test scheme
expect_tests_fail_with <- function(fname, flist) {
  origdir <- getwd()
  on.exit(setwd(origdir))
  setwd(WORKINGPATH) ######################
  
  assertString(fname)
  assertList(flist, any.missing = FALSE)
  
  
  tryCatch({
    result <- testthat::test_dir("tests/", reporter = testthat::FailReporter, filter = paste0("^", fname),
                                 env = testenvWithFunctionReplaced(fname, newfun = NULL))
  }, error = function(cond) {
    #stop("test")
    summErr <- testthat::test_dir("tests/", reporter = testthat::SummaryReporter, filter = paste0("^", "ex01IsPositive"),
                                  env = testenvWithFunctionReplaced(fname, newfun = NULL),
                                  stop_on_failure = FALSE, stop_on_warning = FALSE)
    
    msg <- "Failures: "
    for (testA in summErr) {
      failed.result <- Filter(function(x) !"expectation_success" %in% class(x), testA$results)
      for (fr in failed.result) {
        msg <- paste(msg, fr, sep = "\n")
      }
    }
    stop(sprintf("Running tests/test-%s*.R tests generated at least one failure:\n%s",
                 fname, paste(msg, sep = "\n")))
    
  }
  )
  
  
  expect_error(
    testthat::test_dir("tests/", reporter = testthat::FailReporter, filter = paste0("^", fname),
                       env = testenvWithFunctionReplaced(fname, newfun = NULL, swallow.errors = TRUE)),
    "Failures detected\\.",
    info = sprintf(
      "Tests for %s appear to not check that the function should throw errors under certain conditions.",
      fname)
  )
  
  lapply(flist, function(newfun) {
    testenv <- testenvWithFunctionReplaced(fname, newfun)
    expect_error(
      testthat::test_dir("tests/", reporter = testthat::FailReporter, filter = paste0("^", fname), env = testenv),
      "Failures detected\\.",
      info = sprintf("Replacing %s with the following function should have created a test error but didn't\n%s",
                     fname,
                     paste0(deparse(newfun, control = c("niceNames", "showAttributes"), width.cutoff = 80), collapse = "\n"))
    )
  })
  
}
