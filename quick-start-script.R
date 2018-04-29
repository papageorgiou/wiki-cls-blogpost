

# libs  --------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)
library(readr)


set.seed(12)


# functions ---------------------------------------------------------------


# for graph structure 

create_graph_str  <- function(df_graph, term, edges_nbr) {
    sg <- make_ego_graph(g, order = 1, mode = "out", V(g)[term])[[1]]
    V(sg)$edges <- igraph::degree(sg)
    V(sg)$label <- gsub("_", " ", V(sg)$name, fixed = TRUE)
    ssg <- induced_subgraph(sg, V(sg)[edges > edges_nbr])
    ssg
}



# for graph plot 

create_ggraph <- function(graph_str, net_algo, gg_title) {
    ggraph(graph_str, layout = net_algo ) +  # alternatives: "circle"  or  "auto"
        geom_edge_diagonal(aes(alpha = traffic)) + 
        geom_node_label(aes(label = label, size = edges, fill=edges)) +
        scale_fill_gradient(high = "blue", low = "lightblue") +
        scale_size_continuous(guide = FALSE) +
        theme_graph(background = "beige") + 
        theme(legend.position = "bottom") + labs(title=gg_title)
    
}


# Read data ---------------------------------------------------------------


wiki_url <- "https://dumps.wikimedia.org/other/clickstream/2018-03/clickstream-enwiki-2018-03.tsv.gz"

march18 <- read_tsv(wiki_url, col_names = c("source", "target", "type", "traffic"))

g <- graph_from_data_frame(subset(march18, type == "link"), directed = TRUE)


# Terms -------------------------------------------------------------------

# some examples of terms to test: Artificial_general_intelligence, Artificial_intelligence   Machine_learning   Data_science   Data_analysis  Data_mining Statistics R_(programming_language) Python_(programming_language) Big_data, Analytics


# Data science --------------------------------------------------

ds18 <- create_graph_str(march18, "Data_science", 10) # replace with your terms and define min number of neighbours.


vertex_attr(ds18) %>%  as_tibble() # show in tabular format


create_ggraph(ds18, "auto", "ds18") # network graph



# Data mining ----------------------------------------------------------

dm18 <- create_graph_str(march18, "Data_mining", 10) 


vertex_attr(dm18) %>%  as_tibble() 


create_ggraph(dm18, "auto", "dm18")












