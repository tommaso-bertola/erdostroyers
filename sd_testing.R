library(igraph)
library(data.table)
library(ggplot2)

global_font <- "Fira Sans"
theme_set(theme_bw(base_size = 15, base_family = global_font))
source("source/sandpile.R")
source("source/size_distribution_sf.R")
source("source/utils.R")

# generate the size distributions and write to file
gammas <- c(2.1, 2.3, 2.5, 2.7, 2.9)
n <- 1e4
m <- 2e4
n_samples <- 50
n_iters <- 8e4
sink_frac <- 0.01
# index from which to start sampling (i.e. beginning of the critical region)
# 2e4 / 8e4 is a bit conservative for gamma >~ 2.3, but
# necessary for gamma = 2.1
start_ind <- 2e4
# DANGER: VERY SLOW!
# size_dist_sf(gammas, n_samples, n, m, n_iters, sink_frac, start_ind) |>
#  fwrite("size_dist.out")

# define function to fit logarithmically-binned data points
# (only inside linear region)
fit_sizes <- function(data) {
  require(rjags)

  # define a linear model for JAGS
  model <- "
    model {
      tau <- 1 / sigma^2

      for (i in 1:length(x)) {
        mu[i] <- a + b * x[i]
        y[i] ~ dnorm(mu[i], tau)
      }

      a ~ dnorm(0, 1e-6)
      b ~ dnorm(0, 1e-6)
      sigma ~ dexp(0.001)
    }"

  inits <- function(chain) {
    list(a = runif(1, 0, 10), b = runif(1, -2, 0), sigma = runif(1, 0, 1))
  }

  params <- c("a", "b", "sigma")

  # build the model and run the chains
  jm <- jags.model(
    textConnection(model), data, inits,
    n.chains = 3, n.adapt = 1000, quiet = TRUE
  )
  chain <- coda.samples(jm, variable.names = params, n.iter = 1e5)

  # extract statistics
  chain_sum <- summary(chain)
  means <- chain_sum$statistics[, "Mean"] |> unname()
  taus <- tidybayes::tidy_draws(chain) |>
    dplyr::select("b") |>
    dplyr::pull()

  # b is -tau
  taus <- -taus

  list(a = means[1], b = means[2], c = means[3], taus = list(taus))
}

# read from file and extract counts
szd <- fread("out-files/size_dist.out")
szd[, sizes := stringr::str_split(szd$sizes, "\\|") |> lapply(as.integer)]
str(szd) # `sizes` should be a list of integer vectors

# bin the data logarithmically and adjust counts by n_samples
szd <- szd[, logbins(unlist(sizes)), keyby = gamma
           ][, y := y - log10(n_samples)]

# filter data for fitting
szd_fit <- copy(szd)
szd_fit <- szd_fit[x < 1.1, fit_sizes(.SD), keyby = gamma]

# add fitted data points
szd[, y_fit := szd_fit[.GRP, a] + szd_fit[.GRP, b] * x, keyby = gamma]

# choose palette
my_pal <- wesanderson::wes_palette("Zissou1", 5)[c(1, 3, 5)]
# produce the plots
szd[gamma %in% c(2.1, 2.5, 2.9)] |>
  ggplot(aes(x = x, colour = factor(gamma))) +
    geom_point(
      aes(y = y, shape = factor(gamma)), size = 1
    ) +
    geom_line(aes(y = y_fit), linetype = "dotted") +
    scale_colour_manual(values = my_pal) +
    scale_x_continuous(breaks = 0:2, labels = \(b) 10^b) +
    scale_y_continuous(breaks = seq(-2, 4, 2), labels = \(b) 10^b) +
    annotation_logticks() +
    labs(
      x = "Avalanche size", y = "Counts per network",
      colour = "Exponent γ", shape = "Exponent γ"
    ) +
    theme_bw(base_size = 10, base_family = global_font)

ggsave(
  "figures/size_dist_fit.pdf", device = cairo_pdf,
  width = 14, height = 8, units = "cm"
)

# summarize exponent results
fit_res <- szd_fit[, .(taus = unlist(taus)), keyby = gamma
                   ][, .(mean = mean(taus),
                         sd = sd(taus),
                         q2.5 = quantile(taus, 0.025),
                         q97.5 = quantile(taus, 0.975))
                     , keyby = gamma]
