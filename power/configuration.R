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
    overloaded <- order(degree(g), decreasing = TRUE)[1]

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

alphas_zoom <- unique(c(
    seq(from = 0, to = 0.29, by = 0.05),
    seq(from = 0.3, to = 1, by = 0.1)
))

g_ratio_zoom <- vector()
g_ratio_deg_zoom <- vector()
g_ratio_random_zoom <- vector()
g_ratio_random_zoom_sd <- vector()
for (i in seq_along(alphas_zoom)) {
    g_ratio_zoom[i] <- survived(g_conf, alpha = alphas_zoom[i])
    attempts <- vector()
    for (j in 1:10) {
        attempts[j] <- survived_random(g_conf, alpha = alphas_zoom[i])
    }
    g_ratio_random_zoom[i] <- mean(attempts)
    g_ratio_random_zoom_sd[i] <- sd(attempts)
    g_ratio_deg_zoom[i] <- survived_deg(g_conf, alpha = alphas_zoom[i])
}


conf_ratio <- data.frame(
    alpha = alphas_zoom,
    g_ratio = g_ratio_zoom,
    g_ratio_sd = 0,
    type = "Betw Targeted"
)

conf_ratio_deg <- data.frame(
    alpha = alphas_zoom,
    g_ratio = g_ratio_deg_zoom,
    g_ratio_sd = 0,
    type = "Deg Targeted"
)

conf_ratio_ran <- data.frame(
    alpha = alphas_zoom,
    g_ratio = g_ratio_random_zoom,
    g_ratio_sd = g_ratio_random_zoom_sd,
    type = "Random"
)

conf <- rbind(conf_ratio, conf_ratio_deg, conf_ratio_ran)

write.csv(conf, "null_config.csv")
