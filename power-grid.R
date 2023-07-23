library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/size_distribution_sf.R")
source("source/utils.R")

wus_edges <- fread("./opsahl-powergrid/out.opsahl-powergrid", skip = 2)
g <- graph_from_edgelist(as.matrix(wus_edges), directed = FALSE)

max_cap <- sum(degree(g) - 1)

sp <- sandpile_sz(g, n_iters = max_cap * 5, sink_frac = 1e-2)
sp <- tail(sp, -vcount(g))

x_thr <- 1.4
szd <- data.table(logbins(sp))
tau <- lm(y ~ x, data = szd[x < x_thr])$coefficients[["x"]]

n_smpl <- 10
diam0 <- rep(0, n_smpl)
trans0 <- rep(0, n_smpl)
tau0 <- rep(0, n_smpl)
for (i in seq_len(n_smpl)) {
  g0 <- degree.sequence.game(out.deg = degree(g), method = "vl")
  diam0[i] <- diameter(g0)
  trans0[i] <- transitivity(g0)

  sp0 <- sandpile_sz(g0, 5 * max_cap, 1e-2) |> tail(-vcount(g0))
  szd0 <- data.table(logbins(sp0))
  tau0[i] <- lm(y ~ x, data = szd0[x < x_thr])$coefficients[["x"]]
}

tau0_m <- mean(tau0)
tau0_s <- sd(tau0)

# p-values
1 - pnorm(tau, tau0_m, tau0_s)
1 - pnorm(diameter(g), mean(diam0), sd(diam0))
1 - pnorm(transitivity(g), mean(trans0), sd(trans0))

# with n_smpl = 10
#
# tau = -0.9917519, tau0 = -1.08765 ± 0.0151482
# p-value = 1.220563e-10
# z = 6.330661
# 
# diam = 46, diam0 = 22.9 ± 1.595131
# p-value = 0
# z = 14.48156
#
# trans = 0.1031532, trans0 = 0.0007130407 ± 0.000301108
# p-value = 0
# z = 340.2108
