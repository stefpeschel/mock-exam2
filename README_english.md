# Mock exam 2

The mock exam corresponds in scope and difficulty to the actual exam of this semester. 

There are 4 tasks that you have to solve.

In contrast to the first mock exam, there is no longer an “R” folder containing the tasks. 
Instead, the file **`exam.Rmd`** contains all tasks and thus the entire exam. 
Please solve the tasks directly in this file by adding the missing code.

In the folder `other_formats` you will also find the tasks in other formats (.pdf and .md) 
and in German or English. This should make it easier to read/work with the tasks. 
**However, only the file `exam.Rmd` in the main folder is to be edited.

### Editing the tasks

Take your time to look at the tasks. They are sorted by 
increasing difficulty and divided into different topics, which will 
give you hints as to where you should look for support in the course material. 
For each task, you will find empty functions labeled with headings such as 
*"Write a function that does X ”*. Fill in the 
function body (i.e. the part between the curly brackets `{ }`) with 
code that does what is required. Example:

```r
# Write a function that divides x by y
ex01Divide <- function(x, y) {
# TODO
}
```

should look like this:

```r
# Write a function that divides x by y
ex01Divide <- function(x, y) {
x / y
}
```

(Remember that a function always returns the result of the last executed command).

Only R standard packages and the packages covered in the course should be used in the exam.

**The libraries are loaded automatically, so you do not need to use them with 
with `library()` in your code **. 

Please do not use `require(`, `library(` or `::` / `:::` to load other libraries 
and do not install any other packages for this task. 
Furthermore, this is an `R` course, so do not run any external software from your code. 

### Automated tests

The tests are now located directly below each task. To check whether a test 
passed, you can either run the corresponding chunk by clicking on the green “Play” 
icon in the top right-hand corner of the chunk, or select the line of code for the test 
and execute it. 

If a test has been passed, “Test passed” appears in green under the code chunk. 
Otherwise, the test is not passed and does not give the full score. 

Task 01 has no tests.  

Task 03 is only tested with regard to the statistics of the objects to be created.  
This means that these tests are partially blind. 

At the very end of the `exam.Rmd` file is the code to run all the tests for the entire exam. 
You can enter the following command in the console at any time to run all tests:

```sh
EXERCISES <- character(0); source(“evaluate_submission.R”)
```

Note: To ensure that you are using the same evaluation tool 
as we do, **it is important that you do not change the files `convert_submission`, 
`evaluate_submission.R` and `evaluate_test.R` and that you do NOT change the contents 
of the `.github` folder!** 
If you edit these files, it is possible that your submission contains errors 
that the script does not display. 