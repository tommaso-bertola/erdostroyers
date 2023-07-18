get_sizes_sf <- function(gamma, n, m, n_iters, sink_frac) {
  # return the avalanche sizes from a sandpile dynamics on a 
  # scale-free network
  require(igraph)
  source("sandpile.R")

  # generate a network with the static model
  repeat {
    g <- sample_fitness_pl(
      no.of.nodes = n,
      no.of.edges = m,
      exponent.out = gamma
    )
    # we want only one cc
    if (clusters(g)$no == 1) {
      print("Found network with 1 CC, proceeding...")
      break
    }
  }

  sp <- sandpile(g, n_iters = n_iters, sink_frac = sink_frac)

  return(sp$sizes)
}

size_dist_sf <- function(gammas, n_samples, n, m, n_iters, sink_frac) {
  # returns a data.table with the avalanche size distributions from
  # n_samples sandpile dynamics on a network with gamma = gammas
  # (so if you provide three gammas the simulation runs 300 times)
  require(data.table)
  require(foreach)
  require(doParallel)

  cluster <- parallel::makeCluster(2)
  parallel::clusterExport(cluster, "get_sizes_sf")
  registerDoParallel(cluster)

  ensemble <- function(gamma, n, m, n_iters, sink_frac) {
    foreach(s = 1:n_samples, .combine = "c", .inorder = FALSE) %dopar% {
      get_sizes_sf(gamma, n, m, n_iters, sink_frac)
    }
  }

  # organize size lists in a data.table
  # one `gamma` column and one `sizes` column of vectors
  size_data <- data.table(
    purrr::map(
      gammas,
      function(g) {
        print(paste0("Simulating with gamma = ", g))
        return(ensemble(g, n, m, n_iters, sink_frac))
      }
    )
  )[, c(list(gamma = gammas), .SD)]
  names(size_data)[2] <- "sizes"

  parallel::stopCluster(cluster)

  return(size_data)
}
