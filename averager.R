source("delete_func.R")
alphas <- seq(from = 0.1, to = 0.9, by = 0.1)

n <- 50
k_min <- 2
k_max <- n
gamma <- 3
weights <- (k_min:k_max)^(-gamma)

n_averages <- 3

g_alphas <- vector()
err_g_alphas <- vector()
for (a in seq_along(alphas)) {
    avg_g <- vector()
    for (i in 1:n_averages) {
        degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
        if (sum(degs) %% 2 == 1) {
            degs[1] <- degs[1] + 1
        }
        g <- sample_degseq(out.deg = degs, method = "vl")
        # g <- sample_pa(n = n, out.seq = degs)
        avg_g[i] <- survived(g, alphas[a])
    }
    g_alphas[a] <- mean(avg_g)
    err_g_alphas[a] <- sd(avg_g)

    # print(alphas[a])
}
df <- data.frame(alpha = alphas, mean = avg_g, err = err_g_alphas)

write.csv(df, paste("test", Sys.time(), sep = ""))
print(g_alphas)

# plot(alphas, g_alphas)
