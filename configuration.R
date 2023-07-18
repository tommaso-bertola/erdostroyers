library(igraph)


n <- 5000
k_min <- 2
k_max <- n
gamma <- 3
weights <- (k_min:k_max)^(-gamma)

f <- function(gamma) {
    function(x) {
        return(x^(-gamma))
    }
}

f2 <- f(2)
f3 <- f(3)

gr <- list()
for (i in 1:2) {
    degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
    t <- as.vector(table(degs))
    t <- t / sum(t)
    # plot(t, log = "xy")
    # curve(f2, from = 1, to = 10000, add = TRUE)
    # curve(f3, from = 1, to = 1000, add = TRUE)

    if (sum(degs) %% 2 == 1) {
        degs[1] <- degs[1] + 1
    }
    g <- sample_degseq(out.deg = degs, method = "vl")

    V(g)$load <- betweenness(g, v = V(g), directed = FALSE)
    gr[[i]] <- g
}
# print(components(g)$no)
# print(components(g)$csize)

for (i in 1:10) {
    print(components(gr[[i]])$no)
}


g <- gr[[1]]
