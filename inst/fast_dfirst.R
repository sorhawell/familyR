library(familyR)
rm(list=ls())
# set.seed(1)
# nodes = make_nodes(list("a",0:9,0:9,0:9,0:9))
# edges = sample_edges(nodes,n=10000,cluster_size = 6000)
# source_node = 1
# dt = edges

# set.seed(1)
# nodes = make_nodes(list("a",0:7))
# edges = sample_edges(nodes,n=50)
# source_node = 1

set.seed(1)
nodes = make_nodes(list("a",0:9,0:9))
edges = sample_edges(nodes,n=180)
source_node = 1
dt = edges
dt = unique(edges)


#define recursice function
system.time({print(get_children(dt,2))})
system.time({o2 = dfirst_fastR(2,dt,max_depth=100,verbose=FALSE,only_nodes=FALSE,allocateMemory=1E5)})
sort(sapply(o2,min))
sort(sapply(o2,max))


