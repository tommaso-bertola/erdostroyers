sandpile <- function(
  graph, n_iters, sink_frac, sample_freq = Inf, stop_at_soc = FALSE
) {
  if (igraph::clusters(graph)$no > 1) {
    stop("Network has >= 1 connected components!")
  }

  degrees <- igraph::degree(graph)
  adj_mat <- igraph::get.adjacency(graph)
  n_nodes <- igraph::vcount(graph)
  max_smp <- n_nodes + 1 # upper bound for sampling idx
  # "grains" in each node
  loads <- rep(0, n_nodes)

  durations <- c()

  # avalanche parameters
  durations <- c()
  toppled <- list()
  whens <- c()
  areas <- c()
  sizes <- c()
  # total and toppled grains
  tot_gr <- c()
  top_gr <- c()

  # to save `loads` samples
  load_smp <- list()
  s_cnt <- 0 # sampling counter
  a_cnt <- 0 # avalanche counter
  sample_now <- FALSE
  big_aval <- FALSE

  for (t in 1:n_iters) {
    # choose node at random (floor(runif()) is faster than sample())
    pick <- floor(runif(1, min = 1, max = max_smp))
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] == degrees[pick]) {
      # we're in an avalanche --> update counter
      a_cnt <- a_cnt + 1
      # print loads before avalanche every sample_freq avalanches
      if (a_cnt %% sample_freq == 0) {
        s_cnt <- s_cnt + 1
        sample_now <- TRUE
        load_smp[[s_cnt]] <- loads
      }

      d_cnt <- 1
      # for the area, track the unique indices of affected notes
      top <- pick
      g_cnt <- loads[pick]
      whens <- c(whens, t)

      loads[pick] <- 0
      # select neighbours and offload
      # drop = F is needed because when length(pick) is 1
      # `[` returns a vector
      nbs <- adj_mat[, pick, drop = FALSE]@i + 1
      # drop grains with probability sink_frac
      to_keep <- runif(length(nbs)) > sink_frac
      is_bulk <- all(to_keep)
      nbs <- nbs[to_keep]
      loads[nbs] <- loads[nbs] + 1

      # check if avalanche is proceeding
      overs <- which(loads >= degrees)
      while (length(overs) > 0) {
        # update counters
        d_cnt <- d_cnt + 1
        top <- c(top, overs)
        # check if the avalanche is too big
        # (thus likely to keep on forever)
        if (stop_at_soc && length(top) > n_nodes) {
          print("Big avalanches region reached! Aborting...")
          big_aval <- TRUE
          break
        }
        g_cnt <- g_cnt + sum(degrees[overs])

        loads[overs] <- loads[overs] - degrees[overs]
        nbs <- adj_mat[, overs, drop = FALSE]@i + 1
        # drop grains with probability sink_frac
        to_keep <- runif(length(nbs)) > sink_frac
        is_bulk <- all(c(is_bulk, to_keep))
        nbs <- nbs[to_keep]
        for (n in nbs) {
          loads[n] <- loads[n] + 1
        }

        # update overflown nodes
        overs <- which(loads >= degrees)
      }

      # exit if we are in the big avalanches region
      if (big_aval) break

      # save avalanche parameters
      if (is_bulk) {
        durations <- c(durations, d_cnt)
        toppled[[a_cnt]] <- top
        areas <- c(areas, length(unique(top)))
        sizes <- c(sizes, length(top))
        tot_gr <- c(tot_gr, sum(loads))
        top_gr <- c(top_gr, g_cnt)
      }

      # add after-loads if we're in a sampling window
      if (sample_now) {
        load_smp[[s_cnt]] <- list(
          before = load_smp[[s_cnt]],
          after = loads
        )
        sample_now <- FALSE
      }
    }
  }

  list(
    loads = load_smp,
    toppled = toppled,
    whens = whens,
    durations = durations,
    areas = areas,
    sizes = sizes,
    tot_gr = tot_gr,
    top_gr = top_gr
  )
}

sandpile_sz <- function(graph, n_iters, sink_frac) {
  # stripped-down version to use when you want only the sizes
  degrees <- igraph::degree(graph)
  adj_mat <- igraph::get.adjacency(graph)
  n_nodes <- igraph::vcount(graph)
  max_smp <- n_nodes + 1
  loads <- rep(0, n_nodes)

  sizes <- c()
  for (t in 1:n_iters) {
    pick <- floor(runif(1, min = 1, max = max_smp))
    loads[pick] <- loads[pick] + 1

    if (loads[pick] == degrees[pick]) {
      s_cnt <- 1

      loads[pick] <- 0
      nbs <- adj_mat[, pick, drop = FALSE]@i + 1
      to_keep <- runif(length(nbs)) > sink_frac
      is_bulk <- all(to_keep)
      nbs <- nbs[to_keep]
      loads[nbs] <- loads[nbs] + 1

      overs <- which(loads >= degrees)
      while (length(overs) > 0) {
        s_cnt <- s_cnt + length(overs)

        loads[overs] <- loads[overs] - degrees[overs]
        nbs <- adj_mat[, overs, drop = FALSE]@i + 1
        to_keep <- runif(length(nbs)) > sink_frac
        is_bulk <- all(c(is_bulk, to_keep))
        nbs <- nbs[to_keep]
        for (n in nbs) {
          loads[n] <- loads[n] + 1
        }

        overs <- which(loads >= degrees)
      }

      if (is_bulk) sizes <- c(sizes, s_cnt)
    }
  }

  return(sizes)
}