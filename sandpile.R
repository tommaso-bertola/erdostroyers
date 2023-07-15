sandpile <- function(graph, n_iters, sink_frac, samples,
                     bef_aft = c(TRUE, TRUE)) {
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
  affected <- c()
  whens <- c()
  areas <- c()
  sizes <- c()
  grains <- c()

  # to save `loads` samples (happening when iter == samples)
  load_samples <- vector(mode = "list", length = length(samples))
  s <- 1

  for (t in 1:n_iters) {
    # choose node at random (floor(runif()) is faster than sample())
    pick <- floor(runif(1, min = 1, max = max_smpl))
    loads[pick] <- loads[pick] + 1

    if (t %in% samples && bef_aft[1]) {
      load_samples[[s]] <- loads
      if (!bef_aft[2]) s <- s + 1
    }

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      d_cnt <- 1
      # for the area, track the unique indices of affected notes
      a_cnt <- pick
      s_cnt <- 1
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
        a_cnt <- union(a_cnt, overs)
        s_cnt <- s_cnt + length(overs)
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
      affected <- c(affected, a_cnt)
      areas <- c(areas, length(a_cnt))
      sizes <- c(sizes, s_cnt)
      grains <- c(grains, g_cnt)
    }

    if (t %in% samples && bef_aft[2]) {
      if (bef_aft[1]) {
        load_samples[[s]] <- list(load_samples[[s]], loads)
      } else {
        load_samples[[s]] <- loads
      }
      s <- s + 1
    }
  }

  list(
    loads = load_samples,
    affected = affected,
    whens = whens,
    durations = durations,
    areas = areas,
    sizes = sizes,
    grains = grains
  )
}
