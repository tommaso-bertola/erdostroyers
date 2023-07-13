library(igraph)
source("sandpile.R")

g <- sample_fitness_pl(
  no.of.nodes = 1000,
  no.of.edges = 5000,
  exponent.out = 2.5
)

plot(
  g, layout = layout_with_fr(g),
  vertex.color = "firebrick", edge.color = "grey80",
  vertex.label = NA, vertex.size = sqrt(degree(g))
)
