

# libs  --------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)
library(readr)
library(grid)
library(extrafont)

loadfonts()


# Read data ---------------------------------------------------------------

source("graph-functions.R")

march18 <- read_rds("march18.rds")

names(march18) <- c("source", "target", "type", "traffic")

g <- graph_from_data_frame(subset(march18, type == "link"), directed = TRUE)


# Seed --------------------------------------------------------------------

set.seed(14)

# DM  ---------------------------------------------------------------------

dm18 <- create_subgraph(march18, "Data_mining", 10)  

vertex_attr(dm18) %>%  as_tibble() %>% View("dm18")

DM <- read_csv("DM.csv")

V(dm18)$groupings <- DM$group

create_ggraph(dm18, "auto", "Data mining")

create_ggraph_b(dm18, "auto", "Data mining")


ggsave("DM.png",  width=5.5, height=4.5,  dpi = 300, pointsize=9.5)



# DS ----------------------------------------------------------


ds18 <- create_subgraph(march18, "Data_science", 10)

vertex_attr(ds18) %>%  as_tibble() %>% View("ds18")

DS <- read_csv("DS.csv")


V(ds18)$groupings <- DS$group

create_ggraph(ds18, "auto", "Data Science") 

create_ggraph_b(ds18, "auto", "Data Science") 

ggsave("DS.png",  width=5.5, height=4.5,  dpi = 300, pointsize=8)



# ML ----------------------------------------------------------------------

ml18 <- create_subgraph(march18, "Machine_learning", 25)


ML <- read_csv("ML.csv")


V(ml18)$groupings <- ML$group


create_ggraph(ml18, "auto", "Machine learning")


create_ggraph_b(ml18, "auto", "Machine learning") 


vertex_attr(ml18) %>%  as_tibble() %>% View("ml18")

ggsave("ML.png",  width=5.3, height=4,  dpi = 300, pointsize=5)


# AI ----------------------------------------------------------------------


# ai18 <- create_subgraph(march18, "Artificial_intelligence", 15) 

ai18 <- create_subgraph(march18, "Artificial_intelligence", 34) 


AI <- read_csv("AI2.csv") 


V(ai18)$groupings <- AI$group[1:32]


create_ggraph(ai18, "auto", "Artificial_intelligence") 

create_ggraph_b(ai18, "auto", "Artificial_intelligence") 


vertex_attr(ai18) %>%  as_tibble() %>% View("ai18")

ggsave("AI.png",  width=5, height=4.1,  dpi = 300, pointsize=8)

 
# AGI ---------------------------------------------------------------------


agi18 <- create_subgraph(march18, "Artificial_general_intelligence", 19) # around 20 terms with 15 edges

AGI <- read_csv("AGI.csv")

create_ggraph(agi18, "auto", "Artificial general intelligence")

vertex_attr(agi18) %>%  as_tibble() %>% View("agi18")

ggsave("AGI.png",  width=5, height=4,  dpi = 300, pointsize=8)



# Big data ----------------------------------------------------------------

# bd18 <- create_subgraph(march18, "Big_data", 10) # around 20 terms with 15 edges
# 
# 
# create_ggraph(bd18, "auto", "Big_data")
# 
# vertex_attr(bd18) %>%  as_tibble() %>% View("bd18")


# Analytics ---------------------------------------------------------------

# a18 <- create_subgraph(march18, "Analytics", 10) # with 10 is just perfect.
# 
# V(a18)$groupings <- c("big_data", "small_data", "med_data", "key_concept", "big_data", "small_data")
# 
# create_ggraph(a18, "auto", "Analytics")
# 
# vertex_attr(a18) %>%  as_tibble() %>% View("a18")


