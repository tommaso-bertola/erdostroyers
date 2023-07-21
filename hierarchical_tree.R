library(igraph)

m = 3 #number of levels
z = 5 #branching factor
n = 0 #number of nodes
for(i in 0:m){
  n = n + z^i
}

pc = sqrt(z)/(z*(z^(m-1)-1)^2/(z^m - 1) + 1) #critical probability before congestion

g = make_tree(n, children = z, mode = "undirected")
V(g)$name = 1:n
plot(g, vertex.label=NA, vertex.size=0.2)

d = distances(g)
d_avg = average.path.length(g)

save(g, file="tree.RData")

#degree distribution obvious
#clustering coefficient basically zero