#!/usr/bin/env Rscript
library("checkmate")  # idiot renv doesn't load this *even though* it is in the lockfile otherwise.
library("dplyr")
library("tidyr")
library("readr")
library("lubridate")
library("ggplot2")
library("testthat")

testenv <- new.env(parent = .GlobalEnv)
# .R files
allfiles <- list.files("R", pattern="\\.[rR]$", recursive=TRUE, full.names=TRUE)
# all relevant helper functions in the helper folder
helper.files = list.files(c(".github/helpers"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)
# source both into test environment
invisible(
  lapply(c(allfiles, helper.files), source, local = testenv,
         echo = FALSE, verbose = FALSE, prompt.echo = FALSE)
)
testenv$EXERCISES <- test
testenv$SELF <- testenv
testenv$WORKINGPATH <- getwd()

test_no <- c("NA", 
             "2.1", "2.2", "2.3", 
             "3.1", "3.2", 
             "4.1", "4.2", "4.31", "4.32", "4.33", "4.34", "4.41", "4.42", "4.43")
test_path <- ".github/evaluation/tests/"
test_files <- list.files(test_path)
test_df <- data.frame(test_no = test_no, test_file = test_files)
test_df

file_name <- test_df[test_df$test_no == test, "test_file"]
source(paste0(test_path, file_name), local = testenv)
