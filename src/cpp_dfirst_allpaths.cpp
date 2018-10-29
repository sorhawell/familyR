#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


void follow_tree(
  int this_node,
  IntegerVector from,
  IntegerVector to,
  int this_level,
  IntegerVector this_path,
  int max_depth,
  bool verbose,
  bool only_nodes,
  IntegerVector paths_stream,
  IntegerVector paths_stream_pp) {

  //force return if this_level>max_depth
  if(this_level>max_depth) return;

  //some local variables
  int a_child_node;
  int any_match;
  int n_children_found = 0;
  int a_node_in_a_path;
  //int circle_counter;

  //some constants
  int from_size      = from.size();


  //extend this_path with this_node
  this_path[this_level-1] = this_node;


  //iterate all edges...
  for(int i_node = 0; i_node<from_size;i_node++) {

    //find if any next child node emitting from this_node
    if(this_node==from[i_node]) {
      a_child_node = to[i_node];

      //check child is not already in path, to avoid creating a circle
      circle_counter = 0;
      for(int i_path = 0; i_path<this_level;i_path++) {
        a_node_in_a_path = this_path[i_path];
        if(a_node_in_a_path == 0) break; //this_path is not longer, everything beyond here is zeros anyway
        if(a_child_node==this_path[i_path]) circle_counter++; //already in this path, that would make a circle
      }

      //if child is approved, visit child recursively
      if(circle_counter == 0) {
        n_children_found++;//count found children for this node
        //goto child
        follow_tree(a_child_node,from,to,this_level+1,this_path,max_depth,
                    verbose,only_nodes,paths_stream,paths_stream_pp);
        //... and now returning from child
        //... go look for more children
      }
    }
    //if no valid children was found, then this node is a terminal.
    if(n_children_found==0) {
      //push this_path to paths_stream
      for(int i_path = 0; i_path<this_level;i_path++) {
        paths_stream_pp[0] = paths_stream_pp[0] + 1;
        paths_stream[i_path+paths_stream_pp] = this_path[i_path];
      }

    }



  }

}

// [[Rcpp::export]]
int rcpp_dfirst(
    int source_node,
    IntegerVector from,
    IntegerVector to,
    int max_level,
    bool verbose,
    bool only_nodes,
    IntegerVector paths_stream
    ) {

  //int paths_stream_p = &paths_stream;
  if(only_nodes) IntegerVector global_seen_nodes(from.size());
  IntegerVector empty_path(max_level);
  IntegerVector paths_stream_pp(1);

  //follow_tree( this_node,from,to,this_level  this_path,max_depth, verbose,only_nodes,path_stream
  follow_tree(source_node ,from,to,1        ,  empty_path,25      , verbose,
              only_nodes,paths_stream,paths_stream_pp);

  int b = 0;

  return b;
}

