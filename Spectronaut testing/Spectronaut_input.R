# Short script to test SpectronautToMSstatsFormat

tinytest_dat <- vroom("input/Spectronaut/spectronaut_input.csv")
test_dat <- vroom("input/Spectronaut/240312 Alex AC-8056_Report.tsv")

formatted_data <- SpectronauttoMSstatsFormat(
  input = test_dat,
  annotation = NULL,
  use_log_file = FALSE)

processed_data <- dataProcess(formatted_data)
