# Script to test DIANN input manually
# Sam Siljee
# 20th November 2023

# Libraries
library(dplyr)
library(vroom)
library(MSstats)

# Load data
data <- vroom("input/DIANN/report.tsv")
annotations <- vroom("input/DIANN/annotations.tsv")

# Create input
MSstatsInput <- DIANNtoMSstatsFormat(input = data, annotation = annotations)

# Process data
MSstatsProcessed <- dataProcess(MSstatsInput)
