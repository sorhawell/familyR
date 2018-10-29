#' fast all paths
#'
#' @param source_node
#' @param dt
#' @param max_depth
#' @param verbose
#' @param only_nodes
#' @param allocateMemory
#'
#' @return
#' @export
#'
#' @examples
dfirst_fastR = function(source_node,dt,max_depth=511,verbose=FALSE,only_nodes=FALSE,allocateMemory=1E5) {
  # get all unique names and index to source_node
  all_names = sort(unique(c(dt$from,dt$to))) #all names in all networks
  source_node_no = match(source_node,all_names) #find source node

  #prepare memomry for cpp function

  #following declared variables will be treated as mutables by Cpp function
  from = match(dt$from,all_names) #edge is indexed a casted as integer type
  to   = match(dt$to  ,all_names) #
  paths_stream        = rep(0L , allocateMemory) #0 indicate blank space
  seen_nodes_stream   = rep(0L , length(from)  )

  memUsed = familyR:::rcpp_dfirst(source_node,dt$from,dt$to,max_depth,verbose,only_nodes,paths_stream)
  cat("mem used:",memUsed,"\n")
  if(memUsed<0) {print("mem overflow"); return(NULL)}
  m = t(matrix(paths_stream,ncol=length(paths_stream)/2))
  m = m[-which(m==0)[1]:-nrow(m),]
  uniNodes = sort(unique(c(source_node,m[,2])))
  out = lapply(uniNodes,function(a_node) sort(unique(-m[a_node==m[,2],1])))
  names(out) = uniNodes
  return(out)
}
