library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_minimal(base_size = 15, base_family = "Fira Sans Condensed")
)
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

sp <- sandpile(g, 1e5, 10^-4)

# save in datatable all variables except loads
sp_dt <- as.data.table(sp[-1])

n_avs <- nrow(sp_dt)

# subset events before the first whole-network avalanche
# ("before full capacity")
sp_bfc <- sp_dt[1:(min(which(areas == 1000)) - 1)]
time <- seq_len(nrow(sp_bfc))

sp_bfc[areas != sizes]

ggplot(sp_bfc, aes(x = seq_len(nrow(sp_bfc)))) +
  geom_line(aes(y = areas), colour = "black") +
  geom_line(aes(y = sizes), colour = "firebrick")
