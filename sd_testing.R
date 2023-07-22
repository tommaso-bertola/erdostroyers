library(igraph)
library(data.table)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/size_distribution_sf.R")
source("source/utils.R")

g <- sample_fitness_pl(
  no.of.nodes = 1e4,
  no.of.edges = 2e4,
  exponent.out = 2.1
)
g <- get_lcc(g)

sp <- sandpile(g, 8e4, 0.01)

gammas <- c(2.1, 2.3, 2.5, 2.7, 2.9)
n <- 1e4
m <- 2e4
n_samples <- 50
n_iters <- 8e4
sink_frac <- 0.01
start_ind <- 2e4
size_dist_sf(gammas, n_samples, n, m, n_iters, sink_frac, start_ind) |>
  fwrite("size_dist.out")
