library(igraph)
library(tidyverse)
library(compiler)

survived <- function(g, alpha) {
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    overloaded <- order(V(g)$load, decreasing = TRUE)[1]

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
survived <- cmpfun(survived)


survived_deg <- function(g, alpha) {
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    overloaded <- sample(order(degree(g), decreasing = TRUE)[1:10], size = 1)

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
survived_deg <- cmpfun(survived_deg)


survived_random <- function(g, alpha) {
    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    V(g)$capacity <- (1 + alpha) * V(g)$load
    overloaded <- sample(x = 1:length(g), size = 1)

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
survived_random <- cmpfun(survived_random)

nodes <- read.csv("nodes_clean.csv")
edges <- read.csv("edges.csv")

el <- as.matrix(edges + 1)
g <- graph_from_edgelist(el = el, directed = FALSE)
# g<-as.undirected(g)
g_conf <- g

g_conf <- sample_degseq(degree(g), method = "simple.no.multiple.uniform")

alphas <- unique(c(
    seq(from = 0, to = 0.29, by = 0.05),
    seq(from = 0.3, to = 1, by = 0.1)
))
n_averages <- 5

# g_ratio_zoom <- vector()
# g_ratio_deg_zoom <- vector()
# g_ratio_random_zoom <- vector()
# g_ratio_random_zoom_sd <- vector()
# for (i in seq_along(alphas_zoom)) {
#     g_ratio_zoom[i] <- survived(g_conf, alpha = alphas_zoom[i])
#     attempts <- vector()
#     for (j in 1:10) {
#         attempts[j] <- survived_random(g_conf, alpha = alphas_zoom[i])
#     }
#     g_ratio_random_zoom[i] <- mean(attempts)
#     g_ratio_random_zoom_sd[i] <- sd(attempts)
#     g_ratio_deg_zoom[i] <- survived_deg(g_conf, alpha = alphas_zoom[i])
# }


g_alphas <- vector()
err_g_alphas <- vector()
g_alphas_random <- vector()
err_g_alphas_random <- vector()
g_alphas_deg <- vector()
err_g_alphas_deg <- vector()
for (a in seq_along(alphas)) {
    avg_g <- vector()
    avg_g_random <- vector()
    avg_g_deg <- vector()
    for (i in 1:n_averages) {
        # g <- sample_degseq_safely(n, k_min, k_max, weights)
        # g <- sample_pa(n = n, out.seq = degs)
        avg_g[i] <- survived(g_conf, alphas[a])
        avg_g_random[i] <- survived_random(g_conf, alphas[a])
        avg_g_deg[i] <- survived_deg(g_conf, alphas[a])
        print(paste(i, alphas[a]))
    }
    g_alphas[a] <- mean(avg_g)
    err_g_alphas[a] <- sd(avg_g)
    g_alphas_random[a] <- mean(avg_g_random)
    err_g_alphas_random[a] <- sd(avg_g_random)
    g_alphas_deg[a] <- mean(avg_g_deg)
    err_g_alphas_deg[a] <- sd(avg_g_deg)
    print(alphas[a])
}


btw <- data.frame(
    alpha = alphas,
    g_ratio = g_alphas,
    g_ratio_sd = err_g_alphas,
    type = "Betw Targeted"
)

rnd <- data.frame(
    alpha = alphas,
    g_ratio = g_alphas_random,
    g_ratio_sd = err_g_alphas_random,
    type = "Random"
)

deg <- data.frame(
    alpha = alphas,
    g_ratio = g_alphas_deg,
    g_ratio_sd = err_g_alphas_deg,
    type = "Deg targeted"
)

conf <- rbind(btw, rnd, deg)

write.csv(conf, "null_config.csv")
