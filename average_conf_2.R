source("delete_func.R")
library(compiler)

safely <- function(fn, ..., max_attempts = 80) {
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
                # print(paste("N att", i, length(this_env$result)))
                return(this_env$result)
            }
        }
        msg <- sprintf(
            "%s failed after %d tries; returning NULL.",
            deparse(match.call()),
            max_attempts
        )
        stop(msg)
        NULL
    }
}

degseq <- function(n, k_min, k_max, weights) {
    degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
    if (sum(degs) %% 2 == 1) {
        degs[1] <- degs[1] + 1
    }
    g <- igraph::sample_degseq(out.deg = degs, method = "vl")
    return(g)
}

sample_degseq_safely <- safely(degseq)
sample_degseq_safely <- cmpfun(sample_degseq_safely)

# alphas <- seq(from = 0.1, to = 0.9, by = 0.05)
# alphas <- c(0.001, 0.01, 0.03, 0.05, 0.08)
alphas <- c(0.00, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00, 1.10, 1.20, 1.50)



n <- 20000
k_min <- 1
k_max <- n
gamma <- 2.8
weights <- (k_min:k_max)^(-gamma)

n_averages <- 20

g_alphas <- vector()
err_g_alphas <- vector()
g_alphas_random <- vector()
err_g_alphas_random <- vector()
for (a in seq_along(alphas)) {
    avg_g <- vector()
    avg_g_random <- vector()
    degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
    if (sum(degs) %% 2 == 1) {
        degs[1] <- degs[1] + 1
    }
    for (i in 1:n_averages) {
        g <- sample_degseq(out.deg = degs, method = "simple.no.multiple.uniform")
        lcc_index <- which.max(components(g)$csize)
        g <- delete_vertices(g, V(g)[components(g)$membership != lcc_index])
        # g <- sample_pa(n = n, out.seq = degs)
        avg_g[i] <- survived(g, alphas[a])
        avg_g_random[i] <- survived_random(g, alphas[a])
        print(paste(i, alphas[a]))
    }
    g_alphas[a] <- mean(avg_g)
    err_g_alphas[a] <- sd(avg_g)
    g_alphas_random[a] <- mean(avg_g_random)
    err_g_alphas_random[a] <- sd(avg_g_random)
    print(alphas[a])
}
df <- data.frame(
    alpha = alphas,
    mean = g_alphas,
    err = err_g_alphas,
    mean_random = g_alphas_random,
    err_mean_random = err_g_alphas_random
)
write.csv(df, "conf_20000_keep_lcc_20means_2.8gamma_1kmin.csv")

# write.csv(df, paste("test", Sys.time(), sep = ""))
# print(g_alphas)

# plot(alphas, g_alphas)
