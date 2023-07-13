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

  # avalanche length (timesteps)
  durations <- c()

  for (i in 1:n_iters) {
    # choose node at random
    pick <- sample(1:n, 1)
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      loads[pick] <- 0
      count <- 1
      # select neighbours and offload
      nbs <- adj_mat[, pick, drop = FALSE]@i + 1
      loads[nbs] <- loads[nbs] + 1

      overs <- which(loads >= degrees)
      count <- 1
      while (length(overs) > 0) {
        loads[overs] <- 0
        nbs <- adj_mat[, overs, drop = FALSE]@i + 1
        # drop grains with probability sink_frac
        nbs <- nbs[runif(length(nbs)) > sink_frac]
        loads[nbs] <- loads[nbs] + 1

        # update overflown nodes
        overs <- which(loads >= degrees)
        count <- count + 1
      }

      # save avalanche duration
      durations <- c(durations, count)
    }
  }

  list(loads = loads, durations = durations)
}
