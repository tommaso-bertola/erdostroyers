library(igraph, warn.conflicts = FALSE, verbose = FALSE)
library(compiler)

# a function wrapper to prevent errors when generating
# configuration entworks
# triggers a new generation of the degree sequence
safely <- function(fn, ..., max_attempts = 50) {
    function(...) {
        this_env <- environment()
        for (i in seq_len(max_attempts)) {
            ok <- tryCatch(
                {
                    assign("result", fn(...), envir = this_env)
                    TRUE
                },
                error = function(e) {
                    FALSE
                }
            )
            if (ok) {
                return(this_env$result)
            }
        }
        msg <- sprintf(
            "%s failed after %d tries; returning NULL.",
            deparse(match.call()),
            max_attempts
        )
        warning(msg)
        NULL
    }
}
degree_distr <- function(method = "powerlaw", k_min, k_max, gamma) {
    if (method == "powerlaw") {
        return(c((k_min:k_max)^(-gamma)))
    }
}

# generates the network according to the configuration model
generate_configuration <- function(n, k_min, gamma) {
    # generate each time a new distribution
    # from k_min
    k_min <- k_min
    k_max <- k_min * floor(n^(1 / (gamma - 1)))
    probs <- degree_distr(method = "powerlaw", k_min, k_max, gamma)
    degs <- sample(x = c(k_min:k_max), size = n, prob = probs, replace = TRUE)
    # check if it is a good degree sequence
    # sum must be evenF
    if (sum(degs) %% 2 != 0) {
        degs[1] <- degs[1] + 1
    }
    return(realize_degseq(degs, allowed.edge.types = "simple"))
}

generate_configuration_safe <- safely(generate_configuration)

generate <- function(
    method = "er",
    n = 1000,
    p = 1 / 1000,
    k_min = 1,
    gamma = 2,
    alpha = 0.5,
    metric_method = "be") {
    if (method == "er") {
        g <- sample_gnp(n, p)

        # make sure there is only 1 cluster
        # is almost always if p>C log(n)/n
        while (clusters(g)$no > 1) {
            g <- sample_gnp(n, p)
        }
    } else if (method == "pa") {
        # another way of creating networks
        # preferential attachment
        # barabasi albert like
        g <- sample_pa(n, directed = FALSE, power = 3)
    } else if (method == "conf") {
        g <- generate_configuration_safe(n, k_min, gamma)
    } else if (method == "homo") {
        g <- sample_degseq(rep(k_min, n), method = "simple.no.multiple")
    }
    # set custom attributes
    V(g)$cst_ids <- seq_along(g)
    V(g)$load <- metric(g, metric_method)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    return(g)
}


# the function to compute the metric
# of each node; default if betweenness centrality
metric <- function(g, metric_method = "be") {
    if (metric_method == "be") {
        return(betweenness(g, normalized = FALSE))
    } else if (metric_method == "de") {
        return(degree(g))
    }
}


destroy_protocol <- function(g, node_index = 1, method = "targeted") {
    output <- list()
    if (method == "targeted") {
        output$nodes_to_fail <- order(V(g)$load, decreasing = TRUE)[node_index]
        output$cst_ids_failed <- V(g)$cst_ids[output$nodes_to_fail]
    } else if (method == "random") {
        output$nodes_to_fail <- sample(seq_along(g), 1)
        output$cst_ids_failed <- V(g)$cst_ids[output$nodes_to_fail]
    }

    return(output)
}

# return the fraction of survived network
survived_size <- function(g, metric_method = "be", destroy_method = "targeted", node_index = 1) {
    g_failed <- g

    first_nodes <- destroy_protocol(g, node_index, destroy_method)
    nodes_to_fail <- first_nodes$nodes_to_fail
    cst_ids_failed <- first_nodes$cst_ids_failed

    while (length(nodes_to_fail > 0)) {
        g_failed <- delete_vertices(g_failed, nodes_to_fail)
        V(g_failed)$new_load <- metric(g_failed, metric_method)

        failing_nodes <- V(g_failed)$new_load > V(g_failed)$capacity
        nodes_to_fail <- V(g_failed)[failing_nodes]
        cst_ids_failed <- c(cst_ids_failed, V(g_failed)$cst_ids[nodes_to_fail])
    }

    return(length(g_failed) / length(g))
}

# to hopefully speed thing up a bit
# generate <- cmpfun(generate)
# metric <- cmpfun(metric)
