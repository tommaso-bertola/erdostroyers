get_lcc <- function(graph, mode = "weak") {
  # extract the largest connected component from an igraph network
  require(igraph)

  components <- clusters(graph, mode = mode)
  lcc_id <- which.max(components$csize)
  lcc_verts <- V(graph)[components$membership == lcc_id]
  return(induced_subgraph(graph, lcc_verts))
}

get_top_matrix <- function(sp, n_nodes) {
  # get the toppling matrix for the sandpile dynamics object `sp`
  # top_matrix[i, j] = number of toppling events in node j that
  #                    that came after a toppling in node i

  top_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  for (top in sp$toppled) {
    t <- length(top)
    for (i in seq_len(t - 1)) {
      for (j in seq(i + 1, t)) {
        top_matrix[top[i], top[j]] <- top_matrix[top[i], top[j]] + 1
      }
    }
  }

  return(top_matrix)
}