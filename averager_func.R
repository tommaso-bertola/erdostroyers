source("delete_func.R")
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
                print(paste("N att", i, length(this_env$result)))
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
    g <- sample_degseq(out.deg = degs, method = "vl")
    return(g)
}

sample_degseq_safely <- safely(degseq)
averager <- function(alphas, n_averages, n, k_min, k_max, gamma) {
    weights <- (k_min:k_max)^(-gamma)
    g_alphas <- vector()
    err_g_alphas <- vector()
    for (a in seq_along(alphas)) {
        avg_g <- vector()
        for (i in 1:n_averages) {
            # degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)
            # if (sum(degs) %% 2 == 1) {
            #     degs[1] <- degs[1] + 1
            # }
            # g <- sample_degseq(out.deg = degs, method = "vl")
            g <- sample_degseq_safely(n, k_min, k_max, weights)

            # g <- sample_pa(n = n, out.seq = degs)
            avg_g[i] <- survived(g, alphas[a])
        }
        g_alphas[a] <- mean(avg_g)
        err_g_alphas[a] <- sd(avg_g)
    }
    df <- data.frame(alpha = alphas, mean = g_alphas, err = err_g_alphas)
    return(df)
}
# write.csv(df, paste("test", Sys.time(), sep = ""))
# print(g_alphas)

# plot(alphas, g_alphas)

alphas <- seq(from = 0.1, to = 0.9, by = 0.1)

n <- 50
k_min <- 2
k_max <- n
gamma <- 3

n_averages <- 3

dd <- averager(alphas, 3, n, k_min, k_max, gamma)
print(dd)
