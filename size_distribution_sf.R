get_sizes_sf <- function(gamma, n, m, n_iters, sink_frac) {
  require(igraph)
  source("sandpile.R")

  repeat {
    g <- sample_fitness_pl(
      no.of.nodes = n,
      no.of.edges = m,
      exponent.out = gamma
    )
    if (clusters(g)$no == 1) break
  }

  sp <- sandpile(g, n_iters = n_iters, sink_frac = sink_frac)

  return(sp$sizes)
}

size_dist_sf <- function(gammas, n_samples, n, m, n_iters, sink_frac) {
  require(data.table)
  require(foreach)
  require(doParallel)

  cluster <- parallel::makeCluster(3)
  parallel::clusterExport(cluster, "get_sizes_sf")
  registerDoParallel(cluster)

  ensemble <- function(gamma, n, m, n_iters, sink_frac) {
    foreach(s = 1:n_samples, .combine = "c", .inorder = FALSE) %dopar% {
      get_sizes_sf(gamma, n, m, n_iters, sink_frac)
    }
  }

  size_data <- data.table(
    purrr::map(
      gammas,
      \(g) ensemble(g, n, m, n_iters, sink_frac)
    )
  )[, c(list(gamma = gammas), .SD)]
  names(size_data)[2] <- "sizes"

  parallel::stopCluster(cluster)

  return(size_data)
}
