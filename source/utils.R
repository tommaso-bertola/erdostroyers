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

logbins <- function(data, base = 10, dlog = 0.1) {
  # determine counts from data
  cnt <- data.frame(val = unique(data))
  cnt$n <- sapply(cnt$val, \(x) sum(data == x))
  cnt <- cnt[order(cnt$val), ]

  # get (log-)midpoints and lower/upper limits (to nearest integer)
  log_mid <- seq(0, log(max(cnt$val), base), by = dlog)
  lower <- ceiling(base^(log_mid - 0.5 * dlog))
  upper <- floor(base^(log_mid + 0.5 * dlog))

  # select only bins larger than two, e.g. lower = 9, upper = 11
  bin_window <- which(upper > lower + 1)
  lower <- lower[bin_window]
  upper <- upper[bin_window]

  # the values before bin_window are taken directly from the counts
  x <- seq(1, lower[1] - 1)
  y <- c(
    cnt$n[cnt$val %in% x], # these are from the counts
    # here we average the counts falling inside each remaining bin
    mapply(
      \(l, u) sum(cnt$n[l <= cnt$val & cnt$val <= u]) / (1 + u - l),
      lower, upper
    )
  )
  # take the remaining x values from the midpoints defined before
  x <- c(x, base^log_mid[bin_window])

  return(data.frame(x = x, y = y))
}
