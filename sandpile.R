library(igraph)

sandpile <- function(graph, n_iters) {
  degrees <- degree(graph)
  adj_mat <- get.adjacency(graph)
  # number of nodes
  n <- length(degrees)
  # "grains" in each node
  loads <- rep(0, length(degrees))

  for (i in 1:n_iters) {
    # choose node at random
    pick <- sample(1:n, 1)
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      loads[pick] <- 0
      # select neighbours and offload
      nbs <- which(adj_mat[, pick] == 1)
      loads[nbs] <- loads[nbs] + 1

      overs <- which(loads >= degrees)
      while (length(overs) > 0) {
        loads[overs] <- 0
        nbs <- which(adj_mat[, overs] == 1)
        loads[nbs] <- loads[nbs] + 1

        # update overflown nodes
        overs <- which(loads >= degrees)
      }
    }
  }
}
