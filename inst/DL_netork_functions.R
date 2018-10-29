#' Title
#'
#' @param all_paths 
#' @param all_names 
#'
#' @return
#' @export
#'
#' @examples
get_longshorttable = function(all_paths,all_names) {
  
  #write table of any traversed
  out = do.call(rbind,lapply(1:length(all_paths),function(i) {
    this_path = all_paths[[i]]
    data.frame(nodes = this_path,len = 1:length(this_path),path = rep(i,length(this_path)))
  }))
  
  
  longshort_table = do.call(rbind,by(out,INDICES = out$node,function(x) {
    x = cbind(x[which.max(x$len),], x[which.min(x$len),-1])
    x[,1] = all_names[x[,1]]
    names(x) = c("node_name","longest_length","longest_path","shortest_length","shortest_path")
    return(x)
  },simplify = FALSE))
  
  longshort_table = longshort_table[order(longshort_table$longest_length,longshort_table$node_name),]
  return(longshort_table)
}


#' get all unique non-circular pathways in a directed network starting from a source node
#'
#'
#' @param source_node what node to start path search from
#' @param dt directed network table with 'from' and 'to' column
#' @param verbose 
#' @param max_depth 
#' @param only_nodes only discover all nodes (much faster/fewer paths).
#' 
#' Cannot gurantee to find shortest and longest path.
#' 
#' @return 0
#' @export
#'
#' @examples
exploreDirectedNetwork = function(source_node,dt,max_depth=Inf,verbose=FALSE,only_nodes=FALSE) {
  
  #index node names to replace chars
  all_names = sort(unique(c(dt$from,dt$to))) #all names in all networks
  source_node_no = match(source_node,all_names) #find source node
  graph_dt = copy(dt[,.(from=match(from,all_names),to=match(to,all_names))]) #index from/to)
  
  #initialize list to push discovered unique paths into
  #all paths will in practize be used as mutable
  all_paths = list()
  #  discovered_node is in practize a mutable record, of which nodes visited so far.
  seen_nodes = c()
  
  
  #define recursice function
  #' Recursive sub-function to discover all non-circular paths
  #' @param this_node name of node, char or integer (integer probably faster)
  #' @param graph_dt  reference to graph, table with from and to columns
  #' @param this_level current level and path length, do not change
  #' @param this_path  current path of nodes traversed, do not change
  #' @param verbose  print node and level names
  recursiveTree = function(this_node,graph_dt,this_level=1,this_path=c(),max_depth=Inf,verbose=FALSE,only_nodes=FALSE) {
    if(this_level >= max_depth) return(max_depth)               # optional early stop
    if(only_nodes) seen_nodes <<- c(seen_nodes,this_node) # add this node to global charvec of nodes no to visit again from any path
    if(verbose) cat("level:",this_level,"- node:",this_node ,"- found paths:",length(all_paths),
                    "- seen nodes", length(seen_nodes),  "\n")
    this_path[this_level] = this_node                           # add this node to this path
    this_table   = graph_dt[from==this_node,]                   # lookup child nodes for this node
    child_nodes  = unique(this_table$to)                        # reduce to unique child nodes
    child_nodes  = child_nodes[!child_nodes %in% this_path]     # drop child_nodes alrady in this_path to avoid circles
    if(only_nodes) {                                            # skip nodes already visited by another path
      child_nodes = child_nodes[!child_nodes %in% seen_nodes]    # drop nodes all ready visited
    }
    
    #go to all children if any
    for(this_child_node in child_nodes) {
      recursiveTree(
        this_node=this_child_node, #next node is this child
        graph_dt=graph_dt,         #reference to global table
        this_level=this_level+1,   #a child node is one level deeper
        this_path=this_path,       #child path inherits this_path
        max_depth = max_depth,
        verbose=verbose,           #optional printing  
        only_nodes = only_nodes)   
    }
    
    #if this_node is the terminal of this_path, then push this_path to global all_paths
    if(length(child_nodes)<1) {
      all_paths[[length(all_paths)+1]] <<- this_path #pushes to all_paths in closure of exploreDirectedNetwork
    }
    
    return(0)#hurray no more children, return to parent
  }

  #run recursive function
  out = recursiveTree(source_node_no,graph_dt,max_depth=max_depth,verbose=verbose,only_nodes = only_nodes)
  
  ls_table = get_longshorttable(all_paths,all_names)
  
  return(list(ls_table=ls_table,all_paths=all_paths))
}


