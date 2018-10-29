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

// [[Rcpp::export]]
NumericVector rcpp_bfirst(
    int source_node,
    IntegerVector from,
    IntegerVector to,
    int n_epochs,
    IntegerVector paths_stream,
    IntegerVector nodes_stream) {

  //global counters
  int depth = 0;
  int nblocks = 0;
  int path_stream_p = 0;
  int node_stream_p = 0;
  int node_active_p = 0;
  int node_active_end_p = 1;
  int any_match;

  //temp counters for active block
  nodes_stream[node_stream_p++] = source_node;


  //make block more
  int while_count = 0;
  while(++while_count < n_epochs) {
    ++depth; //increase depth
    //Rprintf(" \n -%i  ",depth);
    //start from active nodes and read to end of stream
    for (int node_read_p = node_active_p ; node_read_p<node_active_end_p; node_read_p++) {

      int this_node = nodes_stream[node_read_p];
      //Rprintf("-%i",this_node);
      paths_stream[path_stream_p++] = --nblocks;      //name this block
      paths_stream[path_stream_p++] =  -depth;        //mark depth
      paths_stream[path_stream_p++] =  -this_node;    //mark depth
      IntegerVector block_children  =   to[from == this_node];
      for (int i = 0; i<block_children.size(); i++) {
        //add node to node stream if note seen before
        any_match=0;
        for(int j = 0; j<node_stream_p;j++) {
          if(block_children[i]==nodes_stream[j]) any_match++;
        }
        if(any_match==0) {
          nodes_stream[node_stream_p++] = block_children[i];
          paths_stream[path_stream_p++] = block_children[i];
        }
      }


    }
    node_active_p = node_active_end_p;
    node_active_end_p = node_stream_p;

  }

  return 0;
}

// [[Rcpp::export]]
NumericVector rcpp_bfirst2(
    int source_node,
    IntegerVector from,
    IntegerVector to,
    int n_epochs,
    IntegerVector paths_stream,
    IntegerVector nodes_stream) {

  //global counters
  int depth = 0;
  int nblocks = 0;
  int path_stream_p = 0;
  int node_stream_p = 0;
  int node_active_p = 0;
  int node_active_end_p = 1;
  int any_match;
  int a_child_node;
  int this_node;

  //temp counters for active block
  nodes_stream[node_stream_p++] = source_node;


  //make block more
  int while_count = 0;
  while(++while_count < n_epochs) {
    ++depth; //increase depth
    //Rprintf(" \n -%i  ",depth);
    //start from active nodes and read to end of stream
    for (int node_read_p = node_active_p ; node_read_p<node_active_end_p; node_read_p++) {

      this_node = nodes_stream[node_read_p];
      //Rprintf("-%i",this_node);
      paths_stream[path_stream_p++] = --nblocks;      //name this block
      paths_stream[path_stream_p++] =  -depth;        //mark depth
      paths_stream[path_stream_p++] =  -this_node;    //mark depth
      //IntegerVector block_children  =   to[from == this_node];
      for(int k = 0; k<from.size();k++) {
        if(this_node==from[k]) {
          a_child_node = to[k];
          any_match=0;
          for(int j = 0; j<node_stream_p;j++) {
            if(a_child_node==nodes_stream[j]) any_match++;
          }
          if(any_match==0) {
            nodes_stream[node_stream_p++] = a_child_node;
            paths_stream[path_stream_p++] = a_child_node;
          }

        }

      }


    }
    node_active_p = node_active_end_p;
    node_active_end_p = node_stream_p;

  }

  return 0;
}

