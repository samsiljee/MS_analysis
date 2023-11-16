# Sam Siljee
# 7 June 2023
# Based on the following Youtube tutorial:
# https://www.youtube.com/watch?v=JEaNhUq_8rA

# Libraries
suppressMessages({
    library(ggplot2)
    library(ggpubr)
    library(data.table)
    library(grid)
    library(gridExtra)
    library(cowplot)
    library(dplyr)
    library(igraph)
    library(STRINGdb)
    library(readxl)
    library(stringr)
  library(vroom)
  library(MSstats)
})

# Get sup material from paper
download.file("https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-022-04434-5/MediaObjects/41586_2022_4434_MOESM11_ESM.zip",
              destfile = "./STRING_test_data/pgc3_supp.zip")
unzip(zipfile = "./STRING_test_data/pgc3_supp.zip",
      files = "2020-08-14908C-s11/Supplementary Table 12.xlsx",
      exdir = "./STRING_test_data")
df <- read_xlsx("./STRING_test_data/2020-08-14908C-s11/Supplementary Table 12.xlsx", sheet = 2)
df

df <- read_xlsx("./STRING_test_data/2020-08-14908C-s11/Supplementary Table 12.xlsx", sheet = 2) %>%
    filter(Prioritised == 1) %>%
    dplyr::select(Ensembl.ID, Symbol.ID, gene_biotype) %>%
    as.data.frame
dim(df)
head(df)

table(df$gene_biotype)

# Load test data from Watt et al
annotations <- vroom("input/Watt_et_al/annotations.tsv")
evidence <- vroom("input/Watt_et_al/evidence.txt")
protein_groups <- vroom("input/Watt_et_al/proteinGroups.txt")

# Create MSstats input
MSstats_input <- MaxQtoMSstatsFormat(annotation = annotations,
                                     evidence = evidence,
                                     proteinGroups = protein_groups)

# MSstats process
MSstats_processed <- dataProcess(MSstats_input,
                                 normalization = "none",
                                 MBimpute = FALSE)

# Make comparisons
MSstats_results <- groupComparison(contrast.matrix = "pairwise",
                                   data = MSstats_processed,
                                   save_fitted_models = FALSE,
                                   use_log_file = FALSE)

# Create vecotr of differential genes
df <- MSstats_results$ComparisonResult %>%
      mutate(Dif = ifelse(
        log2FC > 0.58 & adj.pvalue < 0.05,
        "Upregulated",
        ifelse(
          log2FC < -0.58 & adj.pvalue < 0.05,
          "Downregulated",
          "Not significant"
        )
      )) %>%
  filter(Dif != "Not significant") %>%
  mutate(Symbol.ID = Protein)
  

##----------------------------------------------------------------------
# intialise STRINGdb
string_db <- STRINGdb$new(version = "11.5",
                          species = 9606,
                          score_threshold = 400,
                          input_directory = "")

STRINGdb$help("map")

# Map genes

genes_mapped <- string_db$map(df, "Symbol.ID", removeUnmappedRows = TRUE)

dim(genes_mapped)
head(genes_mapped)
tail(genes_mapped)

genes_mapped

# Get interactions
interactions <- string_db$get_interactions(genes_mapped$STRING_id)
networks <- string_db$plot_network(genes_mapped$STRING_id)

# Clustering
# Different methods can be used for this
clusterList <- string_db$get_clusters(genes_mapped$STRING_id, algorithm = "fastgreedy")
clusterList

# Remove isolated nodes
min_len <- sapply(clusterList, function(x) length(x) > 1)
clusterList <- clusterList[min_len]
clusterList

# Plot individual clusters
string_db$plot_network(clusterList[[1]])
string_db$plot_network(clusterList[[2]])

# This cleans the network, removes any nodes with no connection
string_db$plot_network(melt(clusterList)$value)

# Run enrichment analysis
enrichment <- string_db$get_enrichment(genes_mapped$STRING_id)
head(enrichment)
tail(enrichment)

# Returns multiple categories of enrichment
table(enrichment$category)

# Plot overview of GO "CC" - like normal GO analysis
ggplot(subset(enrichment, category == "Component"),
       aes(number_of_genes, description, fill = p_value)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = number_of_genes), colour = "black", hjust = -0.5) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

# Uncertain of this one!
head(subset(enrichment, category == "RCTM"))

# Isolate the synapse path
go_path <- subset(enrichment, description == "Synapse")
go_path

go_path <- subset(enrichment, description == "Synapse" & category == "Component")
go_path

# all gene lists are entered as csv
str_split(go_path$inputGenes, pattern = ",")
str_split(go_path$preferredNames, pattern = ",")

# Reshape into data.frame
string_id <- str_split(go_path$inputGenes, pattern = ",")
genesymbol = str_split(go_path$preferredNames, pattern = ",")

go_path <- data.frame(nodes = string_id, genesymbol = genesymbol)
colnames(go_path) <- c("nodes", "genesymbol")

go_path

# Plot only the pathway network, however there are many nodes missing - not included in enriched genes
string_db$plot_network(go_path$nodes)

# get all the edges to fill missing nodes
all_edges <- string_db$get_interactions(go_path$nodes)
all_edges

# Add back in the gene symbols
all_edges <- left_join(all_edges, go_path, by = c("from" = "nodes"))
all_edges <- left_join(all_edges, go_path, by = c("to" = "nodes"))
all_edges

# Extract unique edges
all_edges <- unique(all_edges)
all_edges

# prepare data.frame for igraph
all_edges <- all_edges[,4:5]
colnames(all_edges) <- c("from", "to")
head(all_edges)
head(go_path)

# Use igraph to create an igraph object
g <- graph_from_data_frame(all_edges,
                           directed = FALSE,
                           vertices = go_path$genesymbol)
# check number of nodes (vertexes)
vcount(g)

# Print vertexes
V(g)

# find top proteins with the highest degree
degree(g)

# Calculate edges
E(g)
ecount(g)

# Sort by most connected elements
sort(degree(g), decreasing = TRUE)

# can use base R to plot. Use set.seed to produce the same shaped network every time.
set.seed(1235)
plot(g)

# Now colouring the top 3 most connected genes
go_path$color <- ifelse(grepl("GRIN2A|GRM1|GPM6A", go_path$genesymbol), "gold", "grey")
# and add to the igraph object. Note that it must be American spelling of 'color'
V(g)$color <- go_path$color

#Plot again
set.seed(1235)
plot(g)

# Can also change the size
go_path$size <- ifelse(grepl("GRIN2A|GRM1|GPM6A", go_path$genesymbol), 20, 15)
V(g)$size <- go_path$size

#Plot again
set.seed(1235)
plot(g)

# Can also adjust the width of the edges
E(g)$width <- "6"

#Plot again
set.seed(1235)
plot(g)

# Change the edge colour (in base R)
set.seed(1235)
plot(g, edge.color = "orange")

# Can use a variety of different topologies
plot(g, layout = layout_in_circle)
plot(g, layout = layout_on_sphere)
plot(g, layout = layout_with_fr)
plot(g, layout = layout_with_fr, vertex.shape = "none")
plot(g, layout = layout_with_fr, vertex.shape = "none", vertex.label.color = V(g)$color, edge.color = "orange")

# Can work out some other information also
transitivity(as.undirected(g, mode = "collapse"))
# Number of steps from one side to another
diameter(g, directed = FALSE, weights = NA)
deg <- degree(g, mode = "all")

# Plot size as relative to number of connections
plot(g, vertex.size = deg*2.2)

# Degree distribution of the network
hist(deg, breaks = 1:vcount(g) - 1, main = "Histogram of node degree")

# Cluster to find modules
ceb <- cluster_edge_betweenness(g)
dendPlot(ceb, mode = "hclust")

# And plotting the network again with the modules coloured.
# The betweeness algorithm used to colour the edges means that the edges in red
# are the edges that connect distinct modules. These are biologically important
plot(ceb, g)

# Number of communities
length(ceb)

# How modular the graph partitioning is
modularity(ceb)

# Delete isolated nodes
new_g <- delete.vertices(g, which(degree(g) == 0))
plot(new_g)

# Make modules again
new_ceb <- cluster_edge_betweenness(new_g)
plot(new_ceb, new_g)

# Extract nodes in a specific cluster
ceb[1]

# Notes - good igraph tutorial
# https://kateto.net/netscix2016.html
