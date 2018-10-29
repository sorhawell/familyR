#' sample persons from a namelist
#'
#' @param namelist named list like list(first=c("Alice","Bob"),adj=c("Affirming","Brave"),latter=c("Allister","Buckingham"))
#' @param n number of persons to sample
#'
#' @return a char vector of person names
#' @export
#'
#' @examples
sample_persons = function(namelist,n=5) {
  do.call(paste,lapply(namelist,sample,size=n,replace=TRUE))
}

#' generate all persons from a name list
#'
#' @param namelist named list like list(first=c("Alice","Bob"),adj=c("Affirming","Brave"),latter=c("Allister","Buckingham"))
#'
#' @return a char vector of person names
#' @export
#'
#' @examples
expand_persons = function(namelist) {
  apply(do.call(expand.grid,namelist),1,paste,collapse=" ")
}


#' generate random edges in a network
#'
#' @importFrom data.table data.table
#'
#' @param nodes a data.frame or data.table with
#' @param n sample size
#' @param rep TRUE/FALSE repeated sampling?
#' @param cluster_size integer, subset nrow to sample from. From 1 to cluster_size rows.
#' @param Class return as data.table or data.frame
#' @param ... not used
#'
#' @return
#' @export
#'
#' @examples
sample_edges = function(nodes,n=5,rep=T,cluster_size=NULL,Class=c("data.table","data.frame"),...) {
  ids = nodes$id
  if(!is.null(cluster_size)) ids = ids[1:cluster_size]
  if(Class[1] == "data.table") dtdf = data.table::data.table else dtdf = data.frame
  dtdf(from = sample(ids,size=n,rep=rep),to=sample(ids,size=n,rep=rep))
}

#' Generate a nodes data.frame or data.table
#'
#' importFrom data.table data.table
#'
#' @param namelist named list like list(first=c("Alice","Bob"),adj=c("Affirming","Brave"),latter=c("Allister","Buckingham"))
#' @param method char either "expand" or "sample". Expand generates any possible combination. Sample samples combinations
#' @param n  integer, only for method="sample", size of sample
#' @param Class wrap in data.frame or data.table
#'
#' @return a nodes data.table or data.frame
#' @export
#'
#' @examples
make_nodes = function(namelist,method=c("expand","sample"),n=NULL,Class = c("data.table","data.frame")) {
  if(method[1]=="expand") {
    personas = expand_persons(namelist)
  } else {
    if(is.null(n) || !is.numeric(n) || n>=1) stop("for method sample, an n larger or equal to one must be provided")
    personas = sample_persons(namelist,n=n)
  }
  if(Class[1] == "data.table") {
    nodes = data.table::data.table(personas)
    nodes[,id := 1:nrow(nodes)]
  } else {
    nodes = data.frame(personas)
    nodes$id = 1:nrow(nodes)
  }
  return(nodes)
}

#' Exhaustive breath-first search of directional network
#'
#' @param edges directional graph represented by data.table or data.frame with columns from and to, containing node ids
#' @param get_node node id(s) to start from, or index into edges$from or nodes$id(if supplied) to start from
#' @param source_as_index bool default FALSE, is get_node the id of node or a index to input edges or nodes
#' @param nodes data.frame or data.table of nodes need one column of id
#'
#' @details noddes$id, edges$from and edges$to should preferably be numeric: integer or double. Char could be slower.
#' The breadfirst algoritm will search through the network generating pathways. Pathway will be generated in parallel,
#' that all non-terminated paths will be of the same length. The algorithm computes as this:
#' Get latest node in all non-terminated paths. Lookup and gather lsit of edges emitting from these nodes. Drop edges that: (1)
#' are leading to already visited nodes in previous epocs to avoid circular paths. (2) Drop edges that converge to same node,
#' keep only one edge to each unique node. (3) Some nodes may implicitly have no emitting edges. Due to (1), (2) and (3) some path
#' will be terminated as latest node has no valid emitting edge. Terminated paths are saved for last. Non terminated paths are
#' extended by their respective emitting edges. If one node has e.g. two emitting edges the path will be cloned in two version
#' where only their new latest node differ. A new epoch starts with list of active paths. When there is no more active paths,
#' then all paths has been terminated. Then, the function return a list of all terminated paths.
#'
#' Breadth first search provides shortest paths from source to any connected nodes.
#' Conversely depth first search provides (shortest and/or longest or some in between) paths from source to any connected
#' nodes.
#' See x function to any path (shortest, longest, etc.)
#'
#' @return list of all non-circular non-converged paths
#' @export
#'
#' @examples
bfirst = function(edges, get_node,source_as_index = FALSE, nodes=NULL) {
  if(source_as_index) {
    #set first node by edges or nodes
    if(!is.null(nodes)) id_source = nodes$id[get_node] else id_source = edges$from[get_node]
  } else {
    #set first node as get_node
    id_source = get_node
  }

  #inits
  paths = list(id_source) #add source node as first node in first active path
  places_visited = c()    #init vector of nodes visited
  closed_paths = list()   #init list of terminated paths

  #serach network (break condition below: no remaining active paths)
  repeat {
    #get lastest nodes in active paths and add to places_visited
    active_ends = sapply(paths,tail,1)
    places_visited = unique(c(places_visited,active_ends))

    #Lookup edges from lastest nodes in active paths
    active_edges = edges[which(edges$from %in% active_ends),]

    #keep only edges going to destinations of nodes not already visited
    active_edges = active_edges[!active_edges$to %in% places_visited,]

    #keep only one edge for each unique edges$to destiation, converging paths thereby reduced to single path
    includeRedundants = unique(match(active_edges$to,active_edges$to))
    active_edges = active_edges[includeRedundants,]

    #define ended paths as terminals;  because no edges or only leading to places already visited
    #... or because several paths converged to one node, and only one these paths traversing the node is needed.
    terminals = !active_ends %in% active_edges$from

    #append terminated paths
    if(sum(terminals)) {
      closed_paths[
        (length(closed_paths) + 1) :
        (length(closed_paths) + sum(terminals))
        ] = paths[terminals]
    }

    #follow extend non terminated paths
    if(nrow(active_edges)==0) break()

    #for all non-terminated paths and their active ends
    new_paths = mapply(paths[!terminals],active_ends[!terminals],FUN=function(this_path,this_end) {
      #get their child edges
      child_edges = active_edges[this_end==active_edges$from,]$to
      #... and for all their child edges, clone a path and extend
      lapply(child_edges,function(this_child_edge) c(this_path,this_child_edge))
    },SIMPLIFY = FALSE)

    #update new active paths list.
    flatten_paths = unlist(new_paths,rec=F)
    paths = flatten_paths
  }

  #loop breaked as no active paths, return all terminated paths
  return(closed_paths)
}

#' Exhaustive breath-first search of directional network, for-loop and append version
#'
#' @param edges directional graph represented by data.table or data.frame with columns from and to, containing node ids
#' @param get_node node id(s) to start from, or index into edges$from or nodes$id(if supplied) to start from
#' @param source_as_index bool default FALSE, is get_node the id of node or a index to input edges or nodes
#' @param nodes data.frame or data.table of nodes need one column of id
#'
#' @details
#'
#' same as bfirst but slightly less cloning and more appending. Uses for-loop instead of mapply/lapply. Order of  the
#' same returned paths differ. In bfirst ordered by first terminated. In bfirst2 by first initiated.
#'
#' noddes$id, edges$from and edges$to should preferably be numeric: integer or double. Char could be slower.
#' The breadfirst algoritm will search through the network generating pathways. Pathway will be generated in parallel,
#' that all non-terminated paths will be of the same length. The algorithm computes as this:
#' Get latest node in all non-terminated paths. Lookup and gather lsit of edges emitting from these nodes. Drop edges that: (1)
#' are leading to already visited nodes in previous epocs to avoid circular paths. (2) Drop edges that converge to same node,
#' keep only one edge to each unique node. (3) Some nodes may implicitly have no emitting edges. Due to (1), (2) and (3) some path
#' will be terminated as latest node has no valid emitting edge. Terminated paths are saved for last. Non terminated paths are
#' extended by their respective emitting edges. If one node has e.g. two emitting edges the path will be cloned in two version
#' where only their new latest node differ. A new epoch starts with list of active paths. When there is no more active paths,
#' then all paths has been terminated. Then, the function return a list of all terminated paths.
#'
#' Breadth first search provides shortest paths from source to any connected nodes.
#' Conversely depth first search provides (shortest and/or longest or some in between) paths from source to any connected
#' nodes.
#' See x function to any path (shortest, longest, etc.)
#'
#' @return list of all non-circular non-converged paths
#' @export
#'
#' @examples
bfirst2 = function(edges, get_node,source_as_index = FALSE, nodes=NULL) {
  if(source_as_index) {
    #set first node by edges or nodes
    if(!is.null(nodes)) id_source = nodes$id[get_node] else id_source = edges$from[get_node]
  } else {
    #set first node as get_node
    id_source = get_node
  }

  #inits
  paths = list(id_source) #add source node as first node in first active path
  paths_active = c(TRUE)
  places_visited = c()    #init vector of nodes visited
  closed_paths = list()   #init list of terminated paths

  #serach network (break condition below: no remaining active paths)
  repeat {
    #get lastest nodes in active paths and add to places_visited
    active_ends = sapply(paths[paths_active],tail,1)
    places_visited = unique(c(places_visited,active_ends))

    #Lookup edges from lastest nodes in active paths
    active_edges = edges[which(edges$from %in% active_ends),]

    #keep only edges going to destinations of nodes not already visited
    active_edges = active_edges[!active_edges$to %in% places_visited,]

    #keep only one edge for each unique edges$to destiation, converging paths thereby reduced to single path
    includeRedundants = unique(match(active_edges$to,active_edges$to))
    active_edges = active_edges[includeRedundants,]

    #define ended paths as terminals;  because no edges or only leading to places already visited
    #... or because several paths converged to one node, and only one these paths traversing the node is needed.
    terminals = !active_ends %in% active_edges$from

    #de-activate terminal paths
    if(sum(terminals)) paths_active[paths_active][terminals] = FALSE

    #follow extend non terminated paths
    if(nrow(active_edges)==0) break()

    #for all non-terminated paths append child edges
    for(this_path in which(paths_active)) {
      this_end = tail(paths[[this_path]],1)
      child_edges = active_edges[this_end==active_edges$from,]$to
      n_childs = length(child_edges)
      if(n_childs>1) paths_active[length(paths_active)+(1:(n_childs-1))] = TRUE #set cloned paths as active
      for(this_child_edge in child_edges) {
        #append all but last child_edges to cloned paths, appended last in paths list
        if(n_childs >1) {

          #this is the reason it is slow
          paths[[length(paths)+1]] = c(paths[[this_path]],this_child_edge)
        }
        #append last child_edge to this active path
        if(n_childs==1) paths[[this_path]] [length(paths[[this_path]])+1] = this_child_edge
        #update remaining child_edges to append
        n_childs = n_childs - 1
      }}

  }

  #loop breaked as no active paths, return all terminated paths
  return(paths)
}


#' Exhaustive breath-first search of directional network, for-loop and append version
#'
#' @param edges directional graph represented by data.table or data.frame with columns from and to, containing node ids
#' @param get_node node id(s) to start from, or index into edges$from or nodes$id(if supplied) to start from
#' @param source_as_index bool default FALSE, is get_node the id of node or a index to input edges or nodes
#' @param nodes data.frame or data.table of nodes need one column of id
#'
#' @details
#'
#' same as bfirst2 but no cloning at all. New paths refer to parent path by index multiplied with -1
#' same returned paths differ. In bfirst ordered by first terminated. In bfirst2 by first initiated.
#'
#' noddes$id, edges$from and edges$to should preferably be numeric: integer or double. Char could be slower.
#' The breadfirst algoritm will search through the network generating pathways. Pathway will be generated in parallel,
#' that all non-terminated paths will be of the same length. The algorithm computes as this:
#' Get latest node in all non-terminated paths. Lookup and gather lsit of edges emitting from these nodes. Drop edges that: (1)
#' are leading to already visited nodes in previous epocs to avoid circular paths. (2) Drop edges that converge to same node,
#' keep only one edge to each unique node. (3) Some nodes may implicitly have no emitting edges. Due to (1), (2) and (3) some path
#' will be terminated as latest node has no valid emitting edge. Terminated paths are saved for last. Non terminated paths are
#' extended by their respective emitting edges. If one node has e.g. two emitting edges the path will be cloned in two version
#' where only their new latest node differ. A new epoch starts with list of active paths. When there is no more active paths,
#' then all paths has been terminated. Then, the function return a list of all terminated paths.
#'
#' Breadth first search provides shortest paths from source to any connected nodes.
#' Conversely depth first search provides (shortest and/or longest or some in between) paths from source to any connected
#' nodes.
#' See x function to any path (shortest, longest, etc.)
#'
#' @return list of all non-circular non-converged paths
#' @export
#'
#' @examples
bfirst3 = function(edges, get_node,source_as_index = FALSE, nodes=NULL) {
  if(source_as_index) {
    #set first node by edges or nodes
    if(!is.null(nodes)) id_source = nodes$id[get_node] else id_source = edges$from[get_node]
  } else {
    #set first node as get_node
    id_source = get_node
  }

  #inits
  paths = list(id_source) #add source node as first node in first active path
  paths_active = c(TRUE)
  places_visited = c()    #init vector of nodes visited
  closed_paths = list()   #init list of terminated paths

  #serach network (break condition below: no remaining active paths)
  repeat {
    #get lastest nodes in active paths and add to places_visited
    active_ends = sapply(paths[paths_active],tail,1)
    places_visited = unique(c(places_visited,active_ends))

    #Lookup edges from lastest nodes in active paths
    active_edges = edges[which(edges$from %in% active_ends),]

    #keep only edges going to destinations of nodes not already visited
    active_edges = active_edges[!active_edges$to %in% places_visited,]

    #keep only one edge for each unique edges$to destiation, converging paths thereby reduced to single path
    includeRedundants = unique(match(active_edges$to,active_edges$to))
    active_edges = active_edges[includeRedundants,]

    #define ended paths as terminals;  because no edges or only leading to places already visited
    #... or because several paths converged to one node, and only one these paths traversing the node is needed.
    terminals = !active_ends %in% active_edges$from

    #de-activate terminal paths
    if(sum(terminals)) paths_active[paths_active][terminals] = FALSE

    #follow extend non terminated paths
    if(nrow(active_edges)==0) break()

    #for all non-terminated paths append child edges
    for(this_path in which(paths_active)) {
      this_end = tail(paths[[this_path]],1)
      child_edges = active_edges[this_end==active_edges$from,]$to
      n_childs = length(child_edges)
      if(n_childs>1) paths_active[length(paths_active)+(1:(n_childs-1))] = TRUE #set cloned paths as active
      for(this_child_edge in child_edges) {
        #append all but last child_edges to cloned paths, appended last in paths list
        if(n_childs >1) {
          #this is the reason it is slow
          paths[[length(paths)+1]] = c(
            -this_path,#index of parent path,this_child_edge)
            this_child_edge)
        }
        #append last child_edge to this active path
        if(n_childs==1) paths[[this_path]] [length(paths[[this_path]])+1] = this_child_edge
        #update remaining child_edges to append
        n_childs = n_childs - 1
      }}

  }

  #loop breaked as no active paths, return all terminated paths
  return(paths)
}



#' Exhaustive breath-first search of directional network, for-loop and append version
#'
#' @param edges directional graph represented by data.table or data.frame with columns from and to, containing node ids
#' @param get_node node id(s) to start from, or index into edges$from or nodes$id(if supplied) to start from
#' @param source_as_index bool default FALSE, is get_node the id of node or a index to input edges or nodes
#' @param nodes data.frame or data.table of nodes need one column of id
#' @param allocate_length vector are preallocated tothis length, to reduce cloning
#'
#' @details
#'
#' same as bfirst3 but no cloning at all. achieved with preallocation.
#' same returned paths differ. In bfirst ordered by first terminated. In bfirst2 by first initiated.
#'
#' noddes$id, edges$from and edges$to should preferably be numeric: integer or double. Char could be slower.
#' The breadfirst algoritm will search through the network generating pathways. Pathway will be generated in parallel,
#' that all non-terminated paths will be of the same length. The algorithm computes as this:
#' Get latest node in all non-terminated paths. Lookup and gather lsit of edges emitting from these nodes. Drop edges that: (1)
#' are leading to already visited nodes in previous epocs to avoid circular paths. (2) Drop edges that converge to same node,
#' keep only one edge to each unique node. (3) Some nodes may implicitly have no emitting edges. Due to (1), (2) and (3) some path
#' will be terminated as latest node has no valid emitting edge. Terminated paths are saved for last. Non terminated paths are
#' extended by their respective emitting edges. If one node has e.g. two emitting edges the path will be cloned in two version
#' where only their new latest node differ. A new epoch starts with list of active paths. When there is no more active paths,
#' then all paths has been terminated. Then, the function return a list of all terminated paths.
#'
#' Breadth first search provides shortest paths from source to any connected nodes.
#' Conversely depth first search provides (shortest and/or longest or some in between) paths from source to any connected
#' nodes.
#' See x function to any path (shortest, longest, etc.)
#'
#' @return list of all non-circular non-converged paths
#' @export
#'
#' @examples
bfirst4 = function(edges, get_node,source_as_index = FALSE, nodes=NULL,allocate_length=5) {
  if(source_as_index) {
    #set first node by edges or nodes
    if(!is.null(nodes)) id_source = nodes$id[get_node] else id_source = edges$from[get_node]
  } else {
    #set first node as get_node
    id_source = get_node
  }

  #inits
  paths = list(c(id_source,rep(NA_integer_,allocate_length)))  #add source node as first node in first active path
  paths_active = c(TRUE)
  places_visited = c()    #init vector of nodes visited
  closed_paths = list()   #init list of terminated paths

  #serach network (break condition below: no remaining active paths)
  # epoch=0
  repeat {
    # epoch = epoch + 1
    # print(epoch)
    #get lastest nodes in active paths and add to places_visited
    active_ends = sapply(paths[paths_active],function(this_path) this_path[rev(which(!is.na(this_path)))[1]])
    places_visited = unique(c(places_visited,active_ends))

    #Lookup edges from lastest nodes in active paths
    active_edges = edges[which(edges$from %in% active_ends),]

    #keep only edges going to destinations of nodes not already visited
    active_edges = active_edges[!active_edges$to %in% places_visited,]

    #keep only one edge for each unique edges$to destiation, converging paths thereby reduced to single path
    includeRedundants = unique(match(active_edges$to,active_edges$to))
    active_edges = active_edges[includeRedundants,]

    #define ended paths as terminals;  because no edges or only leading to places already visited
    #... or because several paths converged to one node, and only one these paths traversing the node is needed.
    terminals = !active_ends %in% active_edges$from

    #de-activate terminal paths
    if(sum(terminals)) paths_active[paths_active][terminals] = FALSE

    #follow extend non terminated paths
    if(nrow(active_edges)==0) break()

    #for all non-terminated paths append child edges
    for(this_path in which(paths_active)) {
      that_path = paths[[this_path]]
      that_end_ind = rev(which(!is.na(that_path)))[1]
      this_end = that_path[that_end_ind]
      child_edges = active_edges[this_end==active_edges$from,]$to
      n_childs = length(child_edges)
      if(n_childs>1) paths_active[length(paths_active)+(1:(n_childs-1))] = TRUE #set cloned paths as active
      for(this_child_edge in child_edges) {
        #append all but last child_edges to cloned paths, appended last in paths list
        if(n_childs >1) {
          paths[[length(paths)+1]] = c(-this_path,this_child_edge,rep(NA_integer_,allocate_length))
        }
        #append last child_edge to this active path
        if(n_childs==1) {
          paths[[this_path]][that_end_ind+1] = this_child_edge
        }
        #update remaining child_edges to append
        n_childs = n_childs - 1
    }}

  }

  #loop breaked as no active paths, return all terminated paths
  return(paths)
}
