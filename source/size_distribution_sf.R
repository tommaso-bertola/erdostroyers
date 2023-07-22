get_sizes_sf <- function(
  gamma, n, m, n_iters, sink_frac, start_ind, lcc = FALSE
) {
  # return the avalanche sizes from a sandpile dynamics on a
  # scale-free network
  source(here::here("source", "utils.R"))
  source(here::here("source", "sandpile.R"))

  # generate a network with the static model
  repeat {
    g <- igraph::sample_fitness_pl(
      no.of.nodes = n,
      no.of.edges = m,
      exponent.out = gamma
    )
    # we want only one cc
    if (lcc) {
      g <- get_lcc(g)
      break
    } else if (igraph::clusters(g)$no == 1) {
      print("Found network with 1 CC, proceeding...")
      break
    }
  }

  sp <- sandpile_sz(g, n_iters = n_iters, sink_frac = sink_frac)

  return(tail(sp, -start_ind))
}

size_dist_sf <- function(
  gammas, n_samples, n, m, n_iters, sink_frac, start_ind
) {
  # returns a data.table with the avalanche size distributions from
  # n_samples sandpile dynamics on a network with gamma = gammas
  # (so if you set three gammas the simulation runs 3 * n_samples times)
  require(data.table)
  require(foreach)
  require(doParallel)

  cluster <- parallel::makeCluster(2)
  parallel::clusterExport(
    cluster,
    c("get_sizes_sf", "n", "m", "n_iters", "sink_frac", "start_ind")
  )
  registerDoParallel(cluster)

  # organize size lists in a data.table
  # one `gamma` column and one `sizes` column of vectors
  smp <- 1:n_samples
  size_data <- data.table(
    purrr::map(
      gammas,
      function(g) {
        print(paste0("Simulating with gamma = ", g))
        foreach(s = smp, .combine = "c", .inorder = FALSE) %dopar% {
          get_sizes_sf(
            g, n, m, n_iters, sink_frac, start_ind, lcc = TRUE
          )
        }
      }
    )
  )[, c(list(gamma = gammas), .SD)]
  names(size_data)[2] <- "sizes"

  parallel::stopCluster(cluster)

  return(size_data)
}
