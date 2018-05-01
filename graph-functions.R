library(hexbin)
library(ggrepel)



create_subgraph  <- function(df_graph, term, edges_nbr) {
    sg <- make_ego_graph(g, order = 1, mode = "out", V(g)[term])[[1]]
    V(sg)$edges <- igraph::degree(sg)
    V(sg)$label <- gsub("_", " ", V(sg)$name, fixed = TRUE)
    ssg <- induced_subgraph(sg, V(sg)[edges > edges_nbr])
    names(vertex_attr(ssg)) <- c("name" , "number_of_neighbours" , "label")
    ssg
}



# Network graph variants --------------------------------------------------
#         scale_fill_gradient(high = "blue", low = "white") +


# no groupings
create_ggraph <- function(graph_str, net_algo, gg_title) {
    ggraph(graph_str, layout = net_algo ) +  # "dh"    "auto"
        geom_edge_diagonal(aes(alpha = traffic)) + 
        geom_node_label(aes(label = label, size = number_of_neighbours, fill=number_of_neighbours), repel=T) +
        scale_fill_gradient(high = "lightblue", low = "white") +
        scale_size_continuous(guide = FALSE) +
        theme_graph(background = "beige", base_family = "Source Sans Pro") + 
        theme(legend.position = "bottom", plot.caption=element_text(size=5),legend.title = element_text(size=8)) + labs(title=gg_title, 
                                                 subtitle=paste0("en.wikipedia.org/", 
                                                                 str_replace(gg_title, pattern = " ", "_")), caption="based on Wikipedia clickstream data March 18")
    }


#  "a" text from legend removed. 
create_ggraph_b <- function(graph_str, net_algo, gg_title) {
    
    GeomLabel$draw_key <- function (data, params, size) { draw_key_rect(data) }
    GeomLabelRepel$draw_key <- GeomLabel$draw_key
    

    ggraph(graph_str, layout = net_algo ) +  # "dh"    "auto"
        geom_edge_diagonal(aes(alpha = traffic)) + 
        geom_node_label(aes(label = label, size=number_of_neighbours, fill=groupings), repel = T, show.legend = F) +
        scale_size_continuous(guide = FALSE) + 
        theme_graph(background = "white", base_family = "Source Sans Pro") + 
        theme(legend.position = "bottom", legend.text=element_text(size=6), legend.title = element_text(size=8),
              plot.caption=element_text(size=5) ) + 
        scale_fill_brewer(palette="Set1", guide_legend(title = "", ncol=2)) 
        #guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
        #labs(title=gg_title, subtitle= paste0("en.wikipedia.org/", str_replace(gg_title, pattern = " ", "_")), 
            # caption="based on Wikipedia clickstream data March 18") 
    
} 

# with geom_point and geom_text

create_ggraph_c <- function(graph_str, net_algo, gg_title) {
    
    ggraph(graph_str, layout = net_algo ) +  # "dh"    "auto"
        geom_edge_diagonal(aes(alpha = clickstream_traffic)) + 
        geom_node_point(aes( size = number_of_neighbours, colour=groupings)  ) +
        geom_node_text(aes(label=label, size=number_of_neighbours)) + 
        #scale_fill_gradient(high = "blue", low = "white") +
        scale_size_continuous(guide = FALSE) +
        theme_graph(background = "beige") + 
        theme(legend.position = "bottom") + labs(title=gg_title, 
                                                 subtitle= paste0("en.wikipedia.org/", 
                                                                  str_replace(gg_title, pattern = " ", "_")), caption="based on Wikipedia clickstream data March 18") 
    
}


# no labs. 

create_ggraph_d <- function(graph_str, net_algo, gg_title) {
    
    GeomLabel$draw_key <- function (data, params, size) { draw_key_rect(data) }
    ggraph(graph_str, layout = net_algo ) +  # "dh"    "auto"
        geom_edge_diagonal(aes(alpha = clickstream_traffic), show.legend = F) + 
        geom_node_label(aes(label = label, size=number_of_neighbours, fill=groupings), show.legend = F) + theme_graph(background = "beige", base_family = "Source Sans Pro") +
        scale_size_continuous(guide = FALSE) + 
        theme_graph(background = "beige") + 
        scale_fill_brewer(palette="Set1", guide_legend(title = "")) +
        theme(legend.position = "bottom" )# + 
        #labs(title=gg_title, subtitle= paste0("en.wikipedia.org/", str_replace(gg_title, #pattern = " ", "_")), 
           #  caption="based on Wikipedia clickstream data March 18") 
}





