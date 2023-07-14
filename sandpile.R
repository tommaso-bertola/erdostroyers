sandpile <- function(graph, n_iters, sink_frac) {
  if (igraph::clusters(graph)$no > 1) {
    stop("Network has >= 1 connected components!")
  }

  degrees <- igraph::degree(graph)
  adj_mat <- igraph::get.adjacency(graph)
  # number of nodes
  n <- length(degrees)
  # "grains" in each node
  loads <- rep(0, length(degrees))

  durations <- c()

  # avalanche parameters
  durations <- c()
  affected <- c()
  areas <- c()
  sizes <- c()
  grains <- c()

  for (i in 1:n_iters) {
    # choose node at random
    pick <- sample(1:n, 1)
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      d_cnt <- 1
      # for the area, track the unique indices
      # of affected notes
      a_cnt <- pick
      s_cnt <- 1
      g_cnt <- loads[pick]

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
  }

  list(
    loads = loads,
    affected = affected,
    durations = durations,
    areas = areas,
    sizes = sizes,
    grains = grains
  )
}
