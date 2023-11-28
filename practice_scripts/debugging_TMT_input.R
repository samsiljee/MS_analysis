# Read data
library(vroom)
library(MSstatsTMT)

# Read in data
raw_data <- vroom("input/TMT/PD_raw.txt")
annotations <- vroom('input/TMT/PD_annotations.txt')

# Make input
MSstats_input <- PDtoMSstatsTMTFormat(raw_data, annotation = annotations)

# Process
MStats_processed <- proteinSummarization(MSstats_input)
