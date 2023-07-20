library(igraph)

m = 3 #number of levels
z = 3 #branching factor
n = 0 #number of nodes
for(i in 0:m){
  n = n + z^i
}

g = make_tree(n, children = z, mode = "undirected")
V(g)$name = 1:n
plot(g, vertex.label=NA, vertex.size=0.2)

save(g, file="tree.RData")