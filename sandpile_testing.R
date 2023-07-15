library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("sandpile.R")

repeat {
  g <- sample_fitness_pl(
    no.of.nodes = 1000,
    no.of.edges = 4000,
    exponent.out = 3
  )
  if (clusters(g)$no == 1) break
}

plot(
  g, layout = layout_with_kk(g),
  vertex.color = "firebrick", edge.color = "grey80",
  vertex.label = NA, vertex.size = sqrt(degree(g))
)

sp <- sandpile(g, n_iters = 7e3, sink_frac = 10^-4, sample_freq = 50)

g_data <- data.table(
  node = 1:vcount(g),
  degree = degree(g),
  load = sp$loads[[length(sp$loads)]]$after
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

g_data <- data.table(node = 1:vcount(g), degree = degree(g))
g_data[, sons := purrr::map_int(node, ~ degree[.x] * sum(sp$toppled == .x))]

ggplot(g_data[sons > 0]) +
  geom_histogram(aes(x = sons), fill = "firebrick", binwidth = 1)

ggplot(g_data) +
  geom_histogram(aes(x = degree), fill = "firebrick", binwidth = 1)

g <- make_graph(~ 1--2, 2--3, 2--4, 2--5, 4--5)
