#These wrappers are only a minimal shells around Rcpp functions.
#The Rcpp-functions could also be wrapped directly in other R functions in this library.


#' #' Hello Hello
#' #'
#' #' @return
#' #' @export
#' #' @useDynLib familyR
#' #' @examples hello_hello()
#' hello_hello = function() {
#'   familyR:::rcpp_hello_world()
#' }


#' simple test
#'
#' @param n_epochs
#' @param source_node_no
#' @param from
#' @param to
#' @param paths_stream
#' @param seen_nodes_stream
#'
#' @import Rcpp
#' @return
#' @export
#' @useDynLib familyR
#' @examples #blop
bfirst_test = function(source_node_no,from,to,n_epochs,paths_stream,seen_nodes_stream) {
  familyR:::rcpp_bfirst(source_node_no,from,to,n_epochs,paths_stream,seen_nodes_stream)
}


#' simple test
#'
#' @param source_node
#' @param from
#' @param to
#' @param verbose
#' @param only_nodes
#' @param paths_stream
#'
#' @import Rcpp
#' @return
#' @export
#' @useDynLib familyR
#' @examples #blop
dfirst_test = function(source_node,from,to,max_level,verbose,only_nodes,paths_stream){
  familyR:::rcpp_dfirst(source_node,from,to,max_level,verbose,only_nodes,paths_stream)
}
