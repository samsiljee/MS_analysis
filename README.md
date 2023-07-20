# MS_analysis
# (c) Sam Siljee
# Created July 18th 2022

A shiny app to analyse mass spectromtetry for proteomics data - from PSMs to visualisation.

## How to use this app.
1. Please download and install R and R studio
2. Download the "MS_proteomics_shiny" folder, open in Rstudio, and install all required packages
3. Click "Run app" in the top right corner of the pane displaying either ui.R, or server.R
4. Work your way through the app

### Notes:
  - The current stable version should be in the main branch
  - The dev branch is what I'm currently working on, and may not be functional
  - Please email samsiljee@gmail.com for help or suggestions, or submit through Github
  - This is currently under development, so it may not always be functional

## Overall workflow of analysis

### PROTEOMICS
  - Import results from Proteome Discoverer or Max Quant - as PSMs, for both DDA or TMT methodologies
  - Review potential contaminants
  - QC plots; chromatograms
  - summarisation, normalisation, and quantification in `MSstats`
  - Perform differential abundance analysis with `MSstats`, more appropriate as designed specifically for MS proteomics
  - Histogram of log2 foldchanges
  - Perform PCA analysis, and plot clusters
  - Create a heatmap of samples vs proteins
  - Volcano plots of differentially abundant proteins
  - GO and KEGG term analysis
  - Network analysis through STRING?
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
  
## Resources
  - Good overview of different types of analyses to try here: http://bioinformatics.sdstate.edu/go/
  - LFQ Analyst: https://analyst-suite.monash-proteomics.cloud.edu.au/apps/lfq-analyst/
  - Good resource on visualising enrichment analysis: https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html

## Issues and to-do
  - Currently defaults are set to MSstats defaults, will need to discuss with bio-infomatician to check apprpriateness
  - QC section
  - Better progress bars for slow processes
  - Downloadable document describing the settings used (MSstats logs now available however)
  - Add reset to comparison matrix
  - Go term analysis with complete tabular output in the analysis tab, and plots intergrated into the visualisation tab
  - Pathview analysis
  - Change GO term analysis to use aGOtool via API: https://agotool.org/API_Help
  - Add in link to STRING website to view pathway, or embed the interactive network
  - Refine selection methods for including string nodes, currently it just takes the top n most significant nodes, regardless of actual significance.
  - Add WGCNA to analysis section
  - Add logical way to input TMT annotations, may need a channel annotations file alongside others. Look up vignette for MSstatsTMT for more details
  - Correct PcaRef for TMT data
  - Add option to use/see example data, could use MSstats test data for this perhaps
  - Help text for various settings
  - Hide some options under "advanced settings", otherwise use defaults
  - Make a wizard to create annotation files - Use DT for interactive table editing and autofilling some columns from the PSMs files
  - Create a manual?
  - Add basic summary report
  - Fix issue with modelQC not downloading (for TMT at least)
  - Fix Heatmap and PCA plot for TMT
  
# License
Feel free to use this code as you wish under the MIT license, however an acknowledgement would be nice. Thanks!
