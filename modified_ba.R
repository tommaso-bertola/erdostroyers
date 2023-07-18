library(igraph)

#create BA network
m0 = 10 #nodes of initial starting network (in paper: 3)
m = 3 #in paper number of outgoing connections m = 3
mu = 0.5 #affinity tolerance, BA limit corresponds to mu = 1
t = 100 #total size = t + m0

g = barabasi.game(n = m0, m = m, directed = FALSE) #starting network
V(g)$label = runif(m0, min = 0, max = 1) #affinity
V(g)$name = 1:m0

for(i in 1:t){
  affinity = runif(1, min = 0, max =1)
  allowed_verts = V(g)[(V(g)$label > affinity - mu) & (V(g)$label < affinity + mu)] #implement tolerance condition
  #if number of allowed vertices is smaller than m we only create as many edges as allowed:
  k = ifelse(m>length(allowed_verts$name), length(allowed_verts$name), m)
  #degree list of allowed vertices:
  allowed_degs = degree(g)[V(g)$label%in%allowed_verts$label]
  deg_norm = sum(allowed_degs)
  #sample according to degree:
  targets = sample(allowed_verts$name, k, prob = allowed_degs/deg_norm) 
  g = g + vertex(label = affinity, name = i + m0)
  for(j in 1:k){
    g = g + edge(i+m0, targets[j])
  }
}

#pdf("modified_ba_graph.pdf")
plot(g, vertex.label=NA, vertex.size=0.2)
#dev.off()
