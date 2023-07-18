get_lcc <- function(graph, mode = "weak") {
  # extract the largest connected component from an igraph network
  require(igraph)

  components <- clusters(graph, mode = mode)
  lcc_id <- which.max(components$csize)
  lcc_verts <- V(graph)[components$membership == lcc_id]
  return(induced_subgraph(graph, lcc_verts))
}

get_top_matrix <- function(graph, sp){
  n <- vcount(g)
  top_matrix <- matrix(0, nrow = n, ncol = n)
  for(top in sp$toppled){
    top <- sp$toppled[[509]]
    for(i in seq_along(top)){
      for(j in (i+1):length(top)) {
        if((i+1)<=length(top)){ 
          top_matrix[top[i], top[j]] <- top_matrix[top[i], top[j]] + 1
        }
      }
    }
  }
  
  return(top_matrix)
}