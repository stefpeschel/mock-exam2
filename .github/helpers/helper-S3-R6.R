### helper file for all S3 and R6 related test exersices


# function to test correct initialization of expected R6 classes
# checks for existence, ability to create instance with given arguments (via ...)
# and if Values are all there and stored correctly
# returns a test instance if '.output' is set to true
# input name of R6 class and named attributes to run initialize() with
expect_R6_function_equal <- function(className, ..., .output = FALSE, .check_attr = FALSE) {
  # get class generator
  tryCatch(className <- get(className),
           error = function(cond) {
             expect_true(FALSE, info = sprintf("\nclass '%s' not found!",
                                               className))
           })
  # check that it is an R6 class
  classCheck(className, "R6ClassGenerator")
  
  called.with <- make.Called.with(className, list(...), paste("class", className$classname))
  # try to create an instance with the inputs
  withCallingHandlers({testObject <- className$new(...)
                      if (!(exists("testObject") && inherits(testObject, c(className$classname, "R6")))) {
                        stop(sprintf("Cannot correctly create an instance of class %s.
                             \nFix this to run the remaining tests of this Exercise.", className)) 
                      }},
                      error = function(e) {
                        functionError(called.with, e$message)
                      },
                      warning = function(w) {
                        functionError(called.with, w$message,
                                      e.type = "warning")
                      })
  
  # check that attributes present
  if (.check_attr) {
    attrNames <- names(list(...))
    for (val in attrNames) {
      withCallingHandlers(attrVal <- as.list(testObject)[[val]],
                          error = function(e) {
                            functionError(called.with, e$message)
                          },
                          warning = function(w) {
                            functionError(called.with, w$message,
                                          e.type = "warning")
                          })
      errorOutput(attrVal, list(...)[[val]], called.with,
                  add.err.info = sprintf("An instance was created with the inputs, but the returned value of %s are not the same.\n\n",
                                         val))
    }
  }
  
  # return object if required
  if (.output) return(testObject)
}


#TODO not sure why this is needed yet
r6_function_creator <- function(inst, fname) {
  withCallingHandlers(funOut <- as.list(inst)[[fname]],
                      error = function(e) {
                        functionError("error", e$message)
                      },
                      warning = function(w) {
                        functionError("error", w$message,
                                      e.type = "warning")
                      })
  funOut
}


# function to check if required created instances are formed correctly by student
# checks for existence and correct class values
# supply the required instance name (or the instance itself), class name as well as the required values (via ...)
check_r6_object <- function(objectName = NULL, className, ..., object = NULL) {
  #check if exists
  if (!is.null(objectName)) {
    tryCatch(object <- get(objectName),
             error = function(cond) {
               expect_true(FALSE, info = sprintf("\nobject '%s' not found!",
                                                 objectName))
             })
  }
  if (is.null(object) && is.null(objectName)) stop("Supply either object or objectName.")
  
  # check that it is an object of correct class
  expect_true(inherits(object, c(className, "R6")),
              info = sprintf("\nYour object %s is not of the correct class.", objectName))
  # check that attributes present and correct
  attrNames <- names(list(...))
  for (val in attrNames) {
    withCallingHandlers(attrVal <- as.list(object)[[val]],
                        error = function(e) {
                          functionError("Check task description", e$message)
                        },
                        warning = function(w) {
                          functionError("Check task description", w$message,
                                        e.type = "warning")
                        })
    errorOutput(attrVal, list(...)[[val]], sprintf("\nObject %s not initialized correctly", objectName),
                add.err.info = sprintf("An instance was created with the inputs, but the returned value of %s is not correct.\n\n",
                                       val))
  }
}
