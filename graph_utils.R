get_lcc <- function(graph) {
  require(igraph)

  components <- clusters(graph)
  lcc_id <- which.max(components$csize)
  lcc_verts <- V(graph)[components$membership == lcc_id]
  return(induced_subgraph(graph, lcc_verts))
}
