# Sam Siljee
# Written 17/11/23

# Libraries
library(vroom)
library(tidyverse)
library(MSstats)

# Load raw data
raw_data <- vroom("input/DIANN/report.tsv")

# load annotations
annotations <- vroom("input/DIANN/annotations.tsv")


# Function ----
# Copied from the function by MiguelCos

diann_data1 <- raw_data %>% mutate(File.Name = str_replace(raw_data[[1]], ".*\\\\", ""))
diann_data1 <- mutate(diann_data1, File.Name = str_replace(diann_data1[[1]], ".raw.mzml$", ""))

#select relevant columns -> choose which parameter to use for quantitation

for_msstats_prot <- diann_data1 %>% 
  group_by(Stripped.Sequence, 
           Protein.Group, 
           Precursor.Charge, 
           File.Name, 
           Genes.MaxLFQ) %>% 
  summarize()

for_msstats_prot1 <- mutate(for_msstats_prot,
                            File.Name = str_remove(File.Name, ".raw$"))

#merge with MSstats annotation file
annotation <- annotations
colnames(annotation)[colnames(annotation) == "Run"] <- "File.Name"
for_msstats_prot2 <- left_join(for_msstats_prot1, annotation, by = "File.Name")

#change column names to MSstats format
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "BioReplicate"] <- "BioReplicate"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "Stripped.Sequence"] <- "PeptideSequence"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "Protein.Group"] <- "ProteinName"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "Precursor.Charge"] <- "PrecursorCharge"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "File.Name"] <- "Run"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "Precursor.Quantity"] <- "Intensity"
colnames(for_msstats_prot2)[colnames(for_msstats_prot2) == "Genes.MaxLFQ"] <- "Intensity"

#add missing columns
for_msstats_prot2$IsotopeLabelType <- "L"
for_msstats_prot2$ProductCharge <- NA
for_msstats_prot2$FragmentIon <- NA

#reorder to MSstats format
for_msstats_prot3 <- for_msstats_prot2[, c("ProteinName", "PeptideSequence", "PrecursorCharge", 
                                           "FragmentIon", "ProductCharge", "IsotopeLabelType", 
                                           "Condition", "BioReplicate", "Run", "Intensity")]

return(for_msstats_prot3)