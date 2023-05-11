# MS_analysis
A shiny app to analyse mass spectromtetry for proteomics data - from PSMs to visualisation.

## How to use this app.
1. Please download and install R and R studio
2. Download the "MS_proteomics_shiny" folder, open in Rstudio, and install all required packages
3. Click "Run app" in the top right corner of the pane displaying either ui.R, or server.R
4. Work your way through the app

### Notes:
  - The current stable version should be in the main branch
  - I plan to add a dev branch for adding new features
  - Please email samsiljee@gmail.com for help or suggestions, or submit through Github
  - This is currently under development, so it may not always be functional

## Overall workflow of analysis

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
  - Adding MaxQuant input capability
  - Will need to add option to disable automatic removal of potential contaminants from MQ data, do this by setting all values of the potential contaminant column to -ve before inputting into MaxQtoMSstatsFormat
  - Will start on section for QC after visualisation
  - Better progress bars for slow processes
  - Export log files of the MSstats functions, and export a txt document describing the settings used
  - Add reset to comparison matrix
  
## Completed issues
  - Add copyright to repository/scripts
  - Levels in the results of the group comparison is currently a number, this should be the name of the comparison - solved by changing from data frame to a named matrix
   - Current issues include getting reasonable results from the comparison output - solved by changing from data frame to a named matrix
  - Pairwise comparison option added
  - Plots now downloadable with options for DPI, dimensions, and with selected theme
  - PCA plot added, initially not working because `row.names(pca()$x)` and `annot_col()$Run` were different. Fixed by adding a pca_ref column to annot_col and adjusting as needed to match the names of the pca output.
  - Heatmap added and saving properly
  - Add options to set FC and pvalue cutoffs where desired
  
# License
Feel free to use this code as you wish under the MIT license, however an anknowledgement would be nice. Thanks!
