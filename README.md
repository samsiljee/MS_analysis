# MS_analysis
Repository to set up R workflows for my MS data

Initial code is based off of the tutorial by Dr Laurent Gatto, see my tutrial repo for details https://github.com/samsiljee/MS_for_proteomics_tutorial

First section is to have a basic look at the raw MS data, note that it will need to be converted to mzML format first; I have made a PD workflow for this.

## Overall workflow of analysis

### RAW DATA
  - Import raw data
  - Quality control; import raw data in mzML format to look at chromatograms using R. I'm sure there are many more quality control things that can be
  - Note that this was part of the initial workflow - analysing the raw data is not part of the current workflow, I may build this into a separate workflow at some point.

### PROTEOMICS
  - Import results from Proteome Discoverer or Max Quant - as PSMs
  - Review potential contaminants
  - QC plots; chromatograms
  - Normalisation in `MSstats`
  - Search IDs against basal cell markers, and ciliated markers as a control
  - Perform differential abundance analysis with `DESeq2` - note that this was developed for transcriptomics, it's likely more appropriate to perform this using `MSstats`
  - Histogram of log2 foldchanges
  - Perform PCA analysis, and plot clusters
  - Create a heatmap of samples vs proteins
  - Volcano plots of differentially abundant proteins
  - GO and KEGG term analysis
  - Nework analysis of some kind? String?
  - Use COPF analysis to infer proteoform differences: https://www.nature.com/articles/s41467-021-24030-x#code-availability

### PHOSPHOPROTEOMICS
  - Import results from Proteome Discoverer or Max Quant
  - Quantify phosphopeptide enrichment
  - Review potential contaminants
  - Normalise in `MSstats`
  - Search IDs against basal cell markers, and ciliated markers as a control
  
### PRESENTATION
  - Present QC data in graphs suitable for supplementary materials
  - Present data tables in formats such as crosstalk (can be done with R markdown, see https://www.youtube.com/watch?v=WkF7nqEYF1E https://github.com/jthomasmock/penguin-project)
  
### FURTHER ANALYSIS
  - GO annotations
  - Enrichment analysis
  - Biological network analysis

## Issues and to-do
  - Currently working on fine-tuning some of the MSstats I/O options - global standards still to be done, otherwise I just need to check that the default values are sensible
  - Adding heatmap and PCA plot function
  - Will start on section for QC after visualisation
  - Import .rda files as opposed to calculating from scratch again
  - Better progress bars for slow processes
  - Export log files of the MSstats functions, and export a txt document describing the settings used
  
## Completed issues
  - Add copyright to repository/scripts
  - Levels in the results of the group comparison is currently a number, this should be the name of the comparison - solved by changing from data frame to a named matrix
   - Current issues include getting reasonable results from the comparison output - solved by changing from data frame to a named matrix
  - Pairwise comparison option added
  - Plots now downloadable with options for DPI, dimensions, and with selected theme

# License
Feel free to use this code as you wish under the MIT license, however an anknowledgement would be nice. Thanks!
