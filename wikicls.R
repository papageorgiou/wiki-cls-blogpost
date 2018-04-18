
# libs --------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)


# Read data ---------------------------------------------------------------


enwiki <- read.delim(
    "C:/Users/Alex/Desktop/clickstream-enwiki-2017-11.tsv/clickstream-enwiki-2017-11.tsv",
    sep = "\t",
    col.names = c("source", "target", "type", "occurrences"))




enwiki2 <- read.delim(
    "C:/Users/Alex/Desktop/clickstream-enwiki-2018-02.tsv/clickstream-enwiki-2018-02.tsv",
    sep = "\t",
    col.names = c("source", "target", "type", "occurrences"))

g <- graph_from_data_frame(subset(enwiki2, type == "link"), directed = TRUE)


# unzipped <- unzip("C:/Users/Alex/Desktop/clickstream-enwiki-2018-02.tsv.gz", files = NULL, list = FALSE, overwrite = TRUE,
#                   junkpaths = FALSE, exdir = ".", unzip = "internal",
#                   setTimes = FALSE)
# 
# 
# gz <-  gzfile(description, open = "", encoding = getOption("encoding"),
#        compression = 6)
# 
# z <- gzcon(url("C:/Users/Alex/Desktop/clickstream-enwiki-2018-02.tsv.gz"))


# Create graph structure --------------------------------------------------


g <- graph_from_data_frame(subset(enwiki, type == "link"), directed = TRUE)


# Terms -------------------------------------------------------------------

# Artificial_general_intelligence, Artificial_intelligence   Machine_learning   Data_science   Data_analysis  Data_mining Statistics R_(programming_language) Python_(programming_language)




sg <- make_ego_graph(g, order = 1, mode = "out", V(g)["Artificial_intelligence"])[[1]]


# Number of neighbors:
V(sg)$edges <- igraph::degree(sg)

# Labels where the spaces aren't underscores:
V(sg)$label <- gsub("_", " ", V(sg)$name, fixed = TRUE)

ssg <- induced_subgraph(sg, V(sg)[edges > 0])

vertex_attr(ssg) %>%  as_tibble() %>% View("AI2")



# Generate graph ----------------------------------------------------------

s

set.seed(42) # for reproducibility
ggraph(ssg, layout = "circle" ) +  # "dh"    "auto"
    geom_edge_diagonal(aes(alpha = log10(occurrences))) +
    scale_edge_alpha_continuous("Clicks", labels = function(x) { return(ceiling(10 ^ x)) }) +
    geom_node_label(aes(label = label, size = edges)) +
    scale_size_continuous(guide = FALSE) +
    
    theme_graph() +
    theme(legend.position = "bottom")