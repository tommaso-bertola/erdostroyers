library(igraph)

sandpile <- function(graph, n_iters, sink_frac) {
  
  if(clusters(graph)$no > 1) {
    return(print('graph not connected'))
  }
  
  degrees <- degree(graph)
  adj_mat <- get.adjacency(graph)
  # number of nodes
  n <- length(degrees)
  # "grains" in each node
  loads <- rep(0, length(degrees))
  
  durations <- c()

  for (i in 1:n_iters) {
    # choose node at random
    pick <- sample(1:n, 1)
    loads[pick] <- loads[pick] + 1

    # check if the node is overflowing
    if (loads[pick] >= degrees[pick]) {
      loads[pick] <- 0
      # select neighbours and offload
      nbs <- adj_mat[, pick, drop=F]@i + 1
      loads[nbs] <- loads[nbs] + 1

      overs <- which(loads >= degrees)
      count <- 1
      
      while (length(overs) > 0) {
        loads[overs] <- 0
        nbs <- adj_mat[, overs, drop=F]@i + 1
        nbs <- nbs[runif(length(nbs)) > sink_frac]
        loads[nbs] <- loads[nbs] + 1

        # update overflown nodes
        overs <- which(loads >= degrees)
        count <- count + 1
      }
      durations <- c(durations, count) 
    }
  }
  return(
    list(
      loads = loads,
      durations = durations
    )
  )
}

g <- sample_fitness_pl(
  no.of.nodes = 1000, no.of.edges = 5000, exponent.out = 2.5
)

sandpile(g, 200, 10**-4)
