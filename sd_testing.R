library(data.table)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("size_distribution_sf.R")

gammas <- c(2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9)
n <- 1000
m <- 5000
n_iters <- 5000
sink_frac <- 1e-4
sizes <- size_dist_sf(gammas, n_samples = 100, n, m, n_iters, sink_frac)

s_counts <- sizes[, .(s = unlist(unique(sizes))), by = gamma
                  ][, .(count = .N), keyby = .(gamma, s)]

ggplot(s_counts, aes(x = s, y = count)) +
  geom_point(aes(colour = factor(gamma), shape = factor(gamma))) +
  geom_line(aes(colour = factor(gamma))) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  annotation_logticks()
