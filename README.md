# Shiny app to analyse mass spectromtetry for proteomics data - from PSMs to visualisation.
(c) Sam Siljee
Created July 18th 2022

## How to use this app.
1. Please download and install R and R studio
2. Open the file "Launch_application_here.R" in RStudio
3. Click on "Run App" at top right of the editor in RStudio
4. Work your way through the app
5. Enjoy!

### Notes:
  - The first time you launch the app it may take some time to install all of the required packages
  - The current stable version should be in the main branch
  - The dev branch is what I'm currently working on, and may not be functional
  - Please email samsiljee@gmail.com for help or suggestions, or submit through Github
  - This is currently under development

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
  - ShinyGo: http://bioinformatics.sdstate.edu/go/

## Issues and to-do
  - QC section
    - sample correlation
    - Sample CVs
    - Protein numbers
    - Sample coverage
    - Missing values heatmap
    - Imputation
    - See https://github.com/samsiljee/Graphing/blob/main/MS_QC/PD_QC.Rmd for other potential plots
  - Better progress bars for slow processes
  - Add reset to comparison matrix
  - Go term analysis with complete tabular output in the analysis tab, and plots intergrated into the visualisation tab
  - Pathview analysis
  - Add in link to STRING website to view pathway, or embed the interactive network
  - Refine selection methods for including string nodes, currently it just takes the top n most significant nodes, regardless of actual significance.
  - Add WGCNA to analysis section
  - Add option to use/see example data, could use MSstats test data for this perhaps
  - Help text for various settings
  - Create a manual?
  - Fix issue with modelQC not downloading (for TMT at least) - Currently this is only a placeholder for MSstatsTMT and will not be available
  - filter out infinite fold change broken
  - Add ability to upload annotations and raw PSMs in multiple files
  - Spell out direction A vs B say in comparison
  - Fix error with update required when changing to heatmap
  - Fix GO analysis and STRING
  - Question mark button for description of settings
  
# License
Feel free to use this code as you wish under the MIT license, however an acknowledgement would be nice. Thanks!
