library(igraph)

n <- 1000
k_min <- 2
k_max <- n
p <- 0.7 * log(n) / n
weights <- (k_min:k_max)^(-gamma)

degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)

# g <- sample_gnp(n, p)
g <- sample_pa(n = n, out.seq = degs)



V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
overloaded <- order(V(g)$load, decreasing = TRUE)[1]

layout <- layout_with_fr(g)
si <- 5 * log(V(g)$load + 2)

V(g)$capacity <- (1 + 0.9) * V(g)$load
g_or <- g

col <- rep(0, length(g_or))
removed <- overloaded
while (length(overloaded) > 0) {
    g_or <- delete_vertices(g_or, overloaded)
    V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)
    overloaded <- as.vector(V(g_or)[load > capacity])
    removed <- c(removed, overloaded)
}

col[removed] <- 1

n_prime <- max(components(g_or)$csize)
n_orig <- max(components(g)$csize)

print(n_prime / n_orig)

# plot(g,
#     vertex.size = si, layout = layout,
#     vertex.color = components(g)$membership
# )

# g_or <- delete_vertices(g_or, n_to_f)
# V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)
# overloaded <- V(g_or)[load > capacity]
# col <- rep(0, length(g_or))
# col[overloaded] <- 1

# plot(g,
#     vertex.size = si, layout = layout,
#     vertex.color = col
# )


# survived <- function(g) {
#     V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
#     V(g)$capacity <- (1 + 0.03) * V(g)$load
#     overloaded <- order(V(g)$load, decreasing = TRUE)[1]

#     g_or <- g

#     while (length(overloaded) > 0) {
#         g_or <- delete_vertices(g_or, overloaded)
#         V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)
#         overloaded <- as.vector(V(g_or)[load > capacity])
#     }


#     n_prime <- max(components(g_or)$csize)
#     n_orig <- max(components(g)$csize)

#     return(n_prime / n_orig)
# }
