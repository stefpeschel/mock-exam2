library(knitr)

# Extract R code from the R Markdown file
input_file <- "exam.Rmd"
output_file <- "submission.R"

# Extract chunks
purl(input_file, output_file)