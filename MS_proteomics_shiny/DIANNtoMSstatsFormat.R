# Function adapted from MiguelCos; found at: https://github.com/MiguelCos/MSstats_labelfree_preprocessing/blob/master/R/diann2msstats.R
# Original code under the GPL-3.0 licence
# changes made include adding some comments, re-naming some variables, and changing the column name from "FileName" to "Run" in annotations

DIANNtoMSstatsFormat <- function(raw_data, annotations) {
#select relevant columns -> choose which parameter to use for quantitation

for_msstats_prot <- raw_data %>% 
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
# Return formatted data
return(for_msstats_prot3)

}
