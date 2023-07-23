library(igraph)

#create BA network
m0 = 10 #nodes of initial starting network (in paper: 3)
m = 3 #in paper number of outgoing connections: m = 3
mu = 0.23 #affinity tolerance, BA limit corresponds to mu = 1
t = 490 #total size = t + m0

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
#plot(g, vertex.label=NA, vertex.size=0.2)
#dev.off()

save(g, file="modbagraph.RData")

hist(degree(g), breaks=100)

deg_distro = degree_distribution(g)
x = 0:(length(deg_distro)-1)

plot(log(x), log(deg_distro))

#now we want to find out the degree exponent
#we fit a straight line to the data point with a frequency across threshold
#threshold: cut off everything from the first time there is a frequency of zero
bound = which.min(deg_distro[(m+1):length(deg_distro)])
deg_distro_fit = deg_distro[(m+1):(bound-1)]
x_fit = (m+1):(bound-1)

data_fit = data.frame(y = log(deg_distro_fit), x = log(x_fit))
deg_exp = lm(y~x, data_fit)
summary(deg_exp)

degree_exponent = -coef(deg_exp)[2] #corresponds to value mentioned in paper where the tolerance model is explained more in detail
