library(igraph) # handles graphs
library(compiler) # speeds thing up by compiling the functions

# the 3 different protocols implemented for the cascade failures

# uses the betweenness centrality based attacks
survived <- function(g, alpha) {
    # compute the betweenness
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)

    # sets the capacity of each node according to alpha
    V(g)$capacity <- (1 + alpha) * V(g)$load

    # get the size of the larget connected component
    largest_component <- order(components(g)$csize, decreasing = TRUE)[1]

    # mask th enodes, choosing only the nodes in the largest connected componet
    # security measure in case there were more than 1 components in the network
    mask <- which(components(g)$membership %in% largest_component)

    # samples the first node id to fail
    # the one with higher load
    overloaded <- order(V(g)[mask]$load, decreasing = TRUE)[1]

    # saves a copy of the original graph
    g_or <- g

    # continue al long as there are failing nodes
    while (length(overloaded) > 0) {
        # just delete the vertices from the network
        g_or <- delete_vertices(g_or, overloaded)

        # compute the betweenness again and set it as load
        V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)

        # get the indexes of the next failing nodes
        overloaded <- as.vector(V(g_or)[load > capacity])
    }

    # compute the size of the larget connected component
    # in the original and failed network
    n_prime <- max(components(g_or)$csize)
    n_orig <- max(components(g)$csize)

    # return the ratio
    return(n_prime / n_orig)
}

# uses the degree based attacks
# the implementatio is the same except for the choice
# of the first node to fail
survived_deg <- function(g, alpha) {
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    largest_component <- order(components(g)$csize, decreasing = TRUE)[1]
    mask <- which(components(g)$membership %in% largest_component)

    # choose the node to fail sampling from the first 10
    overloaded <- sample(order(degree(g)[mask], decreasing = TRUE)[1:10], size = 1)

    g_or <- g

    while (length(overloaded) > 0) {
        g_or <- delete_vertices(g_or, overloaded)
        V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)
        overloaded <- as.vector(V(g_or)[load > capacity])
    }


    n_prime <- max(components(g_or)$csize)
    n_orig <- max(components(g)$csize)

    return(n_prime / n_orig)
}

# uses the random attacks
# the implementatio is the same except for the choice
# of the first node to fail
survived_random <- function(g, alpha) {
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    largest_component <- order(components(g)$csize, decreasing = TRUE)[1]
    mask <- which(components(g)$membership %in% largest_component)

    # samples taken uniformly on all the nodes
    overloaded <- sample(x = as.vector(V(g)[mask]), size = 1)

    g_or <- g

    while (length(overloaded) > 0) {
        g_or <- delete_vertices(g_or, overloaded)
        V(g_or)$load <- betweenness(g_or, v = V(g_or), directed = FALSE)
        overloaded <- as.vector(V(g_or)[load > capacity])
    }


    n_prime <- max(components(g_or)$csize)
    n_orig <- max(components(g)$csize)

    return(n_prime / n_orig)
}

# compile the functions so that they are a bit faster
survived <- cmpfun(survived)
survived_deg <- cmpfun(survived_deg)
survived_random <- cmpfun(survived_random)