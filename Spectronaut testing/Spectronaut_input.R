# Short script to test SpectronautToMSstatsFormat

# Libraries and data import
library(vroom)
library(MSstats)
library(data.table)
tinytest_dat <- vroom("input/Spectronaut/spectronaut_input.csv")
test_dat <- fread("input/Spectronaut/240312 Alex AC-8056_Report.tsv")

# Wrangling data
# Index of problem columns
problem_column_index <- grep(colnames(test_dat), pattern = ".*\\.raw\\.PG\\.IBAQ$", value = TRUE)

# Function to extract the first number before semicolon
extract_first_number <- function(x) {
  parts <- strsplit(as.character(x), ";")
  sapply(parts, "[[", 1)
}

# Take the first instance on problem columns with multiple values separated by ";"
for(column_name in problem_column_index) {
  test_dat[[column_name]] <- test_dat[[column_name]] %>%
    extract_first_number() %>%
    as.numeric()
}

# Format for MSstats
formatted_data <- SpectronauttoMSstatsFormat(
  input = tinytest_dat,
  annotation = NULL,
  use_log_file = FALSE)

processed_data <- dataProcess(formatted_data)
