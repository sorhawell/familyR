rm(list=ls())
library(data.table)



#load all data associated with metaboard
tabledata = readRDS("./tabledata.rds")

source("./source/DL_netork_functions.R")  #import algo for ranking
source("./source/forestFloor_fcol.R") #import fcol() a lightweight auto-color function


#start up lines for testing
dt <-copy(tabledata$DATA_LINEAGE[, .(from = SOURCE_NAME, to = TARGET_NAME, relation = MAPPING_NAME)])

#new algo to search a tree
source_node = "some name"

#the function to compute longest and shortest path
ranklist = exploreDirectedNetwork(source_node,dt,verbose=F,only_nodes = FALSE)

#and the results...
View(ranklist$ls_table)

#... the function also return any possible non-circular pathways, for this network there are 2754
#ls_table describes for each node which path from this list is the longest and shortest and the length
str(ranklist$all_paths)


#we dont need the actual paths anymore only the lengths is used for ranking
head(ranklist$ls_table$longest_length)
head(ranklist$ls_table$shortest_length)

#noVisNetwork do not like opacity in hexdecimal colors "#12345678" , noOpacity drops opacity part of color
noOpacity = function(hexcol) substr(hexcol,1,7)

#compile smaller table of nodes connected to source
nodes <- data.table(
  id=ranklist$ls_table$node_name,
  name=ranklist$ls_table$node_name,
  level = ranklist$ls_table$longest_length, #visNetwork reads a level column and use for ranking
  color = noOpacity(fcol(ranklist$ls_table$longest_length)) #colors are nice, auto color by rank
)
nodes$color[nodes$level==1] = "#333333" #color source_node dark grey


#compile smaller table of relevant edges
edges <- dt[from%in%nodes$id,]
edges[,flevel :=nodes$level[match(from,nodes$id)]]#flevel is level of from-node
edges[,tlevel :=nodes$level[match(to  ,nodes$id)]]#tlevel is level of to-node

# we like to throw away all edges pointing backwards
# that is edges pointing from a node to another node with lower rank/level

#most edges point forward, some to same level, and few backwards
table(edges$tlevel-edges$flevel)
nrow(edges)

#in this case all edges not point forward are dropped
#that is any edge where tlevel is not grater than flevel, equals are dropped too
ranked_edges <- edges[edges$tlevel>edges$flevel,]
nrow(ranked_edges) # ~300 edges were dropped

graphdata=list()
graphdata$nodes = nodes
#graphdata$nodes$level = NULL
graphdata$edges = ranked_edges
graphdata$nodes$label = substr(graphdata$nodes$name,1,15)


library(visNetwork)
visNetwork::visNetwork(nodes = graphdata$nodes, edges = graphdata$edges) %>%
  visNetwork::visHierarchicalLayout(enabled = TRUE,
                                    levelSeparation = 800,
                                    direction = "UP",
                                    sortMethod = "directed") %>%
  visNetwork::visPhysics(enabled = FALSE
                         ,barnesHut=list(centralGravity=0.0001,gravitationalConstant=-5000000,springConstant=-0.00005,avoidOverlap=1,springLength=950)
  ) %>%
  visNetwork::visNodes(shape = "box",
                       font = list(multi = TRUE, align = "center",
                                   face = "FontAwesome, arial",
                                   bold = list(size = 16)),
                       heightConstraint = list(minimum = 40),
                       widthConstraint = list(minimum = 180)) %>%
  visNetwork::visEdges(arrows = list(to = list(enabled = TRUE)),
                       arrowStrikethrough = FALSE,
                       smooth = FALSE) %>% visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                                                          algorithm="hierarchical" , degree =  list(from = 10, to = 0), hover = TRUE))

layouts = c("layout_nicely", "hierarchical", "layout_in_circle", "layout_with_fr", "layout_on_grid")

visNetwork::visNetwork(nodes = graphdata$nodes, edges = graphdata$edges) %>%
  visNetwork::visIgraphLayout(layout = "layout_in_circle",type = "full",physics = FALSE) %>%
  visNetwork::visEdges(arrows = list(to = list(enabled = TRUE)),
                       arrowStrikethrough = FALSE,
                       smooth = FALSE) %>%
  visNetwork::visNodes(shape = "box",
                       font = list(multi = TRUE, align = "center",
                                   face = "FontAwesome, arial",
                                   bold = list(size = 6)),
                       heightConstraint = list(minimum = 10),
                       widthConstraint = list(minimum = 60))%>% visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                                                                               algorithm="hierarchical" , degree =  list(from = 10, to = 0), hover = TRUE))



