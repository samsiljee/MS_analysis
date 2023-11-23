# Script to test DIANN input manually
# Sam Siljee
# 20th November 2023

# Libraries
library(dplyr)
library(vroom)
library(MSstats)
library(janitor)
library(stringr)

# Load data
data <- vroom("input/DIANN/report_truncated.tsv")

# Read in and edit annotations to match that of the shiny application
annot_col <- vroom("input/DIANN/annotations.tsv") %>%
    clean_names(case = "upper_camel")

# Change back "BioReplicate" column if required
colnames(annot_col)[grep("BioReplicate", colnames(annot_col), ignore.case = TRUE)] <- "BioReplicate"

# Create column for PCA plot and heatmap
annot_col$PcaRef <- str_trim(as.character(annot_col$Run))
annot_col$PcaRef <- gsub(".", "", annot_col$PcaRef, fixed = TRUE)
annot_col$PcaRef <- gsub(" ", "", annot_col$PcaRef, fixed = TRUE)

# Add "Experiment' column for PCA and heatmap labeling
annot_col <- annot_col %>%
    group_by(Condition) %>%
    mutate(Experiment = paste0(Condition, "_", row_number())) %>%
    ungroup()

# Change condition to factor for correct labelling in heatmap and PCA
annot_col$Condition <- as.factor(annot_col$Condition)

# Create input
MSstatsInput <- DIANNtoMSstatsFormat(input = data, annotation = annot_col, use_log_file = FALSE)

# Process data
MSstats_processed <- dataProcess(MSstatsInput, use_log_file = FALSE)

# Make protein matrix
annotations_small <- annot_col %>%
    mutate(originalRUN = PcaRef) %>%
    select(originalRUN, Experiment)
data_small <- MSstats_processed$ProteinLevelData %>%
    mutate(originalRUN = as.character(originalRUN)) %>%
    select(originalRUN, LogIntensities, Protein)

prot_mat <- merge(
    x = data_small,
    y = annotations_small,
    by = "originalRUN",
    all.x = TRUE
) %>%
    dplyr::select(
        Protein, Experiment, LogIntensities
    ) %>%
    pivot_wider(
        id_cols = Protein,
        names_from = Experiment,
        values_from = LogIntensities
    )
