library(igraph)
library(data.table)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/size_distribution_sf.R")
source("source/graph_utils.R")

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

logbins <- function(data, base = 10, dlog = 0.1) {
  # determine counts from data
  cnt <- data.frame(val = unique(data))
  cnt$n <- sapply(cnt$val, \(x) sum(data == x))
  cnt <- cnt[order(cnt$val), ]

  # get (log-)midpoints and lower/upper limits (to nearest integer)
  log_mid <- seq(0, log(max(cnt$val), base), by = dlog)
  lower <- ceiling(base^(log_mid - 0.5 * dlog))
  upper <- floor(base^(log_mid + 0.5 * dlog))

  # select only bins larger than two, e.g. lower = 9, upper = 11
  bin_window <- which(upper > lower + 1)
  lower <- lower[bin_window]
  upper <- upper[bin_window]

  # the values before bin_window are taken directly from the counts
  x <- seq(1, lower[1] - 1)
  y <- c(
    cnt$n[cnt$val %in% x], # these are from the counts
    # here we average the counts falling inside each remaining bin
    mapply(
      \(l, u) sum(cnt$n[l <= cnt$val & cnt$val <= u]) / (1 + u - l),
      lower, upper
    )
  )
  # take the remaining x values from the midpoints defined before
  x <- c(x, base^log_mid[bin_window])

  return(data.frame(x = x, y = y))
}
