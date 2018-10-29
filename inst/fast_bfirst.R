library(familyR)
rm(list=ls())
# set.seed(1)
# nodes = make_nodes(list("a",0:9,0:9,0:9,0:9))
# edges = sample_edges(nodes,n=10000,cluster_size = 6000)
# source_node = 1
# dt = edges

set.seed(1)
nodes = make_nodes(list("a",0:9))
edges = sample_edges(nodes,n=20)
source_node = 1
dt = edges

library(data.table)

#' Title
#'
#' @param graph_dt
#' @param get_node
#' @param return_paths
#' @param direction
#' @param algorithm
#'
#' @return
#' @export
#'
#' @examples
getNetwork = function(
  graph_dt, get_node, return_paths=TRUE,
  direction=c("children","parents","both"),
  algorithm=c("cpp_bfirst","cpp_dfirst")) {

  out = bfirst(get_node,graph_dt)

}

bfirst_rcpp2 = function(source_node,dt,max_depth=Inf,verbose=FALSE,only_nodes=FALSE,allocateMemory=1E5) {
  # get all unique names and index to source_node
  all_names = sort(unique(c(dt$from,dt$to))) #all names in all networks
  source_node_no = match(source_node,all_names) #find source node

  #prepare memomry for cpp function

  #following declared variables will be treated as mutables by Cpp function
  from = match(dt$from,all_names) #edge is indexed a casted as integer type
  to   = match(dt$to  ,all_names) #
  paths_stream        = rep(0L , allocateMemory) #0 indicate blank space
  seen_nodes_stream   = rep(0L , length(from)  )

  familyR:::rcpp_bfirst2(source_node_no,from,to,n_epochs=9999,paths_stream,seen_nodes_stream)
  bfirst_test(1,from,to,100,paths_stream,seen_nodes_stream)
  print(mean(paths_stream!=0))
  print(mean(seen_nodes_stream!=0))
  return(list(
    paths=paths_stream[paths_stream!=0],
    nodes=seen_nodes_stream[seen_nodes_stream!=0]))
}

bfirst_rcpp = function(source_node,dt,max_depth=Inf,verbose=FALSE,only_nodes=FALSE,allocateMemory=1E5) {
  # get all unique names and index to source_node
  all_names = sort(unique(c(dt$from,dt$to))) #all names in all networks
  source_node_no = match(source_node,all_names) #find source node

  #prepare memomry for cpp function

  #following declared variables will be treated as mutables by Cpp function
  from = match(dt$from,all_names) #edge is indexed a casted as integer type
  to   = match(dt$to  ,all_names) #
  paths_stream        = rep(0L , allocateMemory) #0 indicate blank space
  seen_nodes_stream   = rep(0L , length(from)  )

  familyR:::rcpp_bfirst(source_node_no,from,to,n_epochs=9999,paths_stream,seen_nodes_stream)
  bfirst_test(1,from,to,100,paths_stream,seen_nodes_stream)
  print(mean(paths_stream!=0))
  print(mean(seen_nodes_stream!=0))
  return(list(
    paths=paths_stream[paths_stream!=0],
    nodes=seen_nodes_stream[seen_nodes_stream!=0]))
}


#define recursice function
system.time({o1 = get_children(dt,1)})
r1 = sort(o1$nodes$id)
system.time({o2a=bfirst_rcpp(1,dt,100,paths_stream,seen_nodes_stream)})
system.time({o2b=bfirst_rcpp2(1,dt,100,paths_stream,seen_nodes_stream)})
system.time({out = bfirst4(dt,1)})
r3 = c(1,sort(unlist(lapply(out,function(x) x[-1]))))
r2a = sort(o2a$nodes)
r2b = sort(o2b$nodes)

View(data.table(r1,r2a,r2b,r3))

  #return(from)


#}


