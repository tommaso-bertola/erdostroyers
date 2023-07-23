source("delete_func.R")

# alphas <- seq(from = 0.1, to = 0.9, by = 0.05)
# alphas <- c(0.001, 0.01, 0.03, 0.05, 0.08)
# alphas <- c(0.001, 0.01, 0.03, 0.05, 0.08, seq(from = 0.1, to = 1, by = 0.05), 1.5, 2)
# alphas <- c(0.00, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00, 1.10, 1.20, 1.50)
alphas<-seq(from=0, to=0.3, by=0.01)

n <- 1000
p <-  1.3*log(n) / n
# k_min <- 2
# k_max <- n
# gamma <- 3
# weights <- (k_min:k_max)^(-gamma)

n_averages <- 40

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
        # degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
        # if (sum(degs) %% 2 == 1) {
        #     degs[1] <- degs[1] + 1
        # }
        # g <- sample_degseq(out.deg = degs, method = "vl")
        # g <- sample_pa(n = n, out.seq = degs)
        g_conf <- sample_gnp(n = n, p = p)
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
# df <- data.frame(
#     alpha = alphas,
#     mean = g_alphas,
#     err = err_g_alphas,
#     mean_random = g_alphas_random,
#     err_mean_random = err_g_alphas_random
# )
# write.csv(df, "er_1000_40means_11logn.csv")

# write.csv(df, paste("test", Sys.time(), sep = ""))
# print(g_alphas)

# plot(alphas, g_alphas)


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

write.csv(conf, "er_1000n_13lognnp_40means_zoom.csv")
