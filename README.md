# MS_analysis
Repository to set up R workflows for my MS data

Initial code is based off of the tutorial by Dr Laurent Gatto, see my tutrial repo for details https://github.com/samsiljee/MS_for_proteomics_tutorial

First section is to have a basic look at the raw MS data, note that it will need to be converted to mzML format first; I have made a PD workflow for this.

##Overall workflow of analysis

###RAW DATA
  - Import raw data
  - Quality control; import raw data in mzML format to look at chromatograms using R. I'm sure there are many more quality control things that can be 

###PROTEOMICS
  - Import results from Proteome Discoverer or Max Quant
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
  - Use COPF analysis to infer proteoform differences

###PHOSPHOPROTEOMICS
  - Import results from Proteome Discoverer or Max Quant
  - Quantify phosphopeptide enrichment
  - Review potential contaminants
  - Normalise in `MSstats`
  - Search IDs against basal cell markers, and ciliated markers as a control
  
###PRESENTATION
  - Present QC data in graphs suitable for supplementary materials
  - Present data tables in formats such as crosstalk (can be done with R markdown, see https://www.youtube.com/watch?v=WkF7nqEYF1E https://github.com/jthomasmock/penguin-project)

##Issues and to-do
  - Get code up and running for all sections, currently working on `MSstats` analysis
  - Add copyright to repository/scripts
