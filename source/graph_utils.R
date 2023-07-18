get_lcc <- function(graph, mode = "weak") {
  # extract the largest connected component from an igraph network
  require(igraph)

  components <- clusters(graph, mode = mode)
  lcc_id <- which.max(components$csize)
  lcc_verts <- V(graph)[components$membership == lcc_id]
  return(induced_subgraph(graph, lcc_verts))
}
