rm(list=ls())

library(data.table)
library(familyR)

namelist = list(
  first = c("Affirming","Brave","Creepy","Delightful","Entertaining","Flattering","Grave","Helpful"),
  adj = c("Alice","Bob","clifford","Dennis","Erik","Ferdinand","George","Hansi"),
  latter = c("Augustinus","Brady","Crawford","Dawnson","Euler","Fonseca","Gralls","Hubertus")
)



set.seed(1)
#make node set of 100x512 persona
nodes = do.call(rbind,replicate(50,make_nodes(namelist,Class="data.table"),simplify = FALSE))
nodes$id = 1:nrow(nodes)

#sample edges into network 100x512x5 random edges
edges = sample_edges(nodes,n=nrow(nodes)*5,Class = "data.table")
edges.df = as.data.frame(edges) #make a data.frame copy for


# #try different implementations
system.time({paths1 = bfirst (edges.df,get_node=1)})
system.time({paths2 = bfirst2(edges.df,get_node=1)}) # with less cloning , however for loops
system.time({paths3 = bfirst (edges,get_node=1)}) #for this algo data.frame is actually faster
system.time({paths4 = bfirst3 (edges.df,get_node=1)}) #no cloning, return nested paths, also using loops

# #comparison with children, which 3 times faster
system.time({children = get_children(edges,1)})


#
# #hmm use extra trick in sleave before going Rcpp on it...
library(compiler)
cmp_bfirst3 = cmpfun(bfirst3)
cmp_bfirst = cmpfun(bfirst)

#turned bytecompilation for entire package. get_chindren got at 2x speed up again, damnit :)
#because all functions are implicitedly bytecode compiled there is no extra speed by explicitly bytecompiling here

system.time({paths5 = cmp_bfirst  (edges.df,get_node=1)}) #nah, only a very small free lunch this time
system.time({paths6 = cmp_bfirst3 (edges.df,get_node=1)})

#try vector pre-allocation, but source for algoritm is getting very ugly now, slightly faster
system.time({paths7 = bfirst4 (edges.df,get_node=1,allocate_length = 5)})
cmp_bfirst4 = cmpfun(bfirst4)
system.time({paths8 = cmp_bfirst4 (edges.df,get_node=1,allocate_length = 5)})

# #comparison with children, which is now only 2 times faster
system.time({children = get_children(edges,1)})
gc()
system.time({children = get_children(edges,1)})


#ok - only a Rcpp implementation can potentially win now...

