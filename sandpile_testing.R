library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("sandpile.R")

g <- sample_fitness_pl(
  no.of.nodes = 1000,
  no.of.edges = 5000,
  exponent.out = 2.5
)
clusters(g)$no # test number of clusters

plot(
  g, layout = layout_with_fr(g),
  vertex.color = "firebrick", edge.color = "grey80",
  vertex.label = NA, vertex.size = sqrt(degree(g))
)

l_smpl <- seq(1, 5e3, length = 5)[-1] |> as.integer()
sp <- sandpile(g, 5e3, 10^-4, samples = l_smpl)

g_data <- data.table(
  node = 1:vcount(g),
  degree = degree(g),
  load = sp$loads
)

loads <- g_data[load > 0, .(degree, load, fill_perc = 100 * load / degree)]

ggplot(data = loads, aes(x = fill_perc)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "aquamarine4") +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = "Filled capacity (%)", y = "Count")
