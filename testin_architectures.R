library(igraph)

n <- 5000
k_min <- 1
k_max <- n
gamma <- 3
weights <- (k_min:k_max)^(-gamma)

degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)


g <- sample_pa(n = n, out.seq = degs)
