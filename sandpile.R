sandpile <- function(graph, n_iters, sink_frac, sample_freq = Inf) {
  if (igraph::clusters(graph)$no > 1) {
    stop("Network has >= 1 connected components!")
  }

  degrees <- igraph::degree(graph)
  adj_mat <- igraph::get.adjacency(graph)
  max_smpl <- length(degrees) + 1 # upper bound for sampling idx
  # "grains" in each node
  loads <- rep(0, length(degrees))

  durations <- c()

  # avalanche parameters
  durations <- c()
  toppled <- c()
  whens <- c()
  areas <- c()
  sizes <- c()
  total_grains <- c()
  toppled_grains <- c()

  # to save `loads` samples
  load_samples <- list()
  s_cnt <- 0 # sampling counter
  a_cnt <- 0 # avalanche counter
  sample_now <- FALSE

  for (t in 1:n_iters) {
    # choose node at random (floor(runif()) is faster than sample())
    pick <- floor(runif(1, min = 1, max = max_smpl))
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      # we're in an avalanche --> update counter
      a_cnt <- a_cnt + 1
      # print loads before avalanche every sample_freq avalanches
      if (a_cnt %% sample_freq == 0) {
        s_cnt <- s_cnt + 1
        sample_now <- TRUE
        load_samples[[s_cnt]] <- loads
      }

      d_cnt <- 1
      # for the area, track the unique indices of affected notes
      top <- pick
      g_cnt <- loads[pick]
      whens <- c(whens, t)

      loads[pick] <- 0
      # select neighbours and offload
      nbs <- adj_mat[, pick, drop = FALSE]@i + 1
      loads[nbs] <- loads[nbs] + 1

      # check if avalanche is proceeding
      overs <- which(loads >= degrees)
      while (length(overs) > 0) {
        # update counters
        d_cnt <- d_cnt + 1
        top <- c(top, overs)
        g_cnt <- g_cnt + sum(loads[overs])

        loads[overs] <- 0
        nbs <- adj_mat[, overs, drop = FALSE]@i + 1
        # drop grains with probability sink_frac
        nbs <- nbs[runif(length(nbs)) > sink_frac]
        for (n in nbs) {
          loads[n] <- loads[n] + 1
        }

        # update overflown nodes
        overs <- which(loads >= degrees)
      }

      # save avalanche parameters
      durations <- c(durations, d_cnt)
      toppled <- c(toppled, top)
      areas <- c(areas, length(unique(top)))
      sizes <- c(sizes, length(top))
      total_grains <- c(total_grains, sum(loads))
      toppled_grains <- c(toppled_grains, g_cnt)

      # add after-loads if we're in a sampling window
      if (sample_now) {
        load_samples[[s_cnt]] <- list(
          before = load_samples[[s_cnt]],
          after = loads
        )
        sample_now <- FALSE
      }
    }
  }

  list(
    loads = load_samples,
    toppled = toppled,
    whens = whens,
    durations = durations,
    areas = areas,
    sizes = sizes,
    total_grains = total_grains,
    toppled_grains = toppled_grains
  )
}
