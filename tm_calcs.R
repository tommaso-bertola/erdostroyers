library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/graph_utils.R")

repeat {
  g <- sample_fitness_pl(
    no.of.nodes = 5000,
    no.of.edges = 15000,
    exponent.out = 2.5
  )
  if (clusters(g)$no == 1) break
}

sp <- sandpile(g, n_iters = 30000, sink_frac = 1e-4)
tm <- get_top_matrix(sp, vcount(g))

deg <- degree(g)
probs <- purrr::map_dbl(
  sort(unique(deg)),
  \(d) sum(colSums(tm[, which(deg == d), drop = FALSE]))
)
