# Snippet of code from webpage explaining how to convert from Uniprot ID to gene symbol
# Taken from: https://support.bioconductor.org/p/112768/

library(httr)
my_protein_ids <- c('Q8N4C6', 'Q9UM73')

results <- POST(url = "https://www.uniprot.org/uploadlists/",
                body = list(from = 'ID',
                            to = 'GENENAME',
                            format = 'tab',
                            query = paste(my_protein_ids, collapse = ' ')))

uniprot_results <- content(results, type = 'text/tab-separated-values', 
                           col_names = TRUE, 
                           col_types = NULL, 
                           encoding = "UTF-8")
