library(data.table)
library(ggplot2)
theme_set(
  theme_bw(base_size = 10, base_family = "Fira Sans Condensed")
)
source("size_distribution_sf.R")

gammas <- c(2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9)
n <- 1e4
m <- 15e4
n_iters <- m
sink_frac <- 1e-4
sizes <- size_dist_sf(gammas, n_samples = 100, n, m, n_iters, sink_frac)

s_counts <- sizes[, .(s = unlist(unique(sizes))), by = gamma
                  ][, .(count = .N), keyby = .(gamma, s)]

s_counts[gamma %in% c(2.3, 2.5, 2.7, 2.9)] |>
  ggplot(aes(x = s, y = count)) +
    geom_point(
      aes(colour = factor(gamma), shape = factor(gamma)),
      size = 1.25
    ) +
    geom_line(aes(colour = factor(gamma)), linewidth = 0.5) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(breaks = c(1, 1e2, 1e4, 1e6), trans = "log10") +
    scale_colour_viridis_d(option = "viridis") +
    annotation_logticks() +
    labs(
      x = "Avalanche size S", y = "Count",
      shape = "Exponent γ", colour = "Exponent γ"
    )

ggsave(
  "size_dist_sf.png", device = "png",
  width = 13, height = 10, units = "cm",
)

source("graph_utils.R")

g <- sample_fitness_pl(
  no.of.nodes = 4941,
  no.of.edges = 6594,
  exponent.out = 2.5
) |> get_lcc()

d_counts <- data.table(deg = degree(g))[, .(n = .N), by = deg
                                        ][, lapply(.SD, log)]

sp <- sandpile(
  g, n_iters = length(V(g)) + length(E(g)),
  sink_frac = 1e-4, sample_freq = 100
)
