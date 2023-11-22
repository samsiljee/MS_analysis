# Script to abbreviate datasets, making them faster for testing
# Sam Siljee - 21 November 2023

# Libraries
library(vroom)
library(dplyr)

# Load the dataset
dat <- vroom("input/DIANN/cleaned_report.tsv")

# Get number of runs
distinct_runs <- length(unique(dat$Run))

# Identify proteins present in all runs
summary <- dat %>%
    group_by(Protein.Group) %>%
    summarise(Protein = Protein.Group, Number = length(unique(Run)))
eligible_proteins <- summary %>%
    filter(Number == distinct_runs) %>%
    .$Protein.Group %>%
    unique()
filtered_dat <- dat %>%
    filter(Protein.Group %in% eligible_proteins)

# Identify list of most abundant proteins - for each run
proteins <- NULL
for(i in unique(filtered_dat$Run)) {
    new_proteins <- filtered_dat %>%
        filter(Run == i) %>%
        arrange(Genes.MaxLFQ.Unique) %>%
        .$Protein.Group %>%
        unique() %>%
        head(10)
    proteins <- c(proteins, new_proteins)
}


# Filter data
dat <- dat %>%
    filter(Protein.Group %in% proteins)

# Write table
vroom_write(dat, "input/DIANN/report_cleaned_truncated.tsv")

