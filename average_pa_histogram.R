source("delete_func.R")

# code to create histogram for g ratio in BA model

# alphas to choose from
alphas <- c(0.2, 0.5, 1)

n <- 1000 # net size
n_averages <- 2000 # averages done

# store data
data <- matrix(nrow = n_averages, ncol = 3)
colnames(data) <- alphas

# sample and compute g ratio
for (i in 1:n_averages) {
    g <- sample_pa(n = n, m = 4, directed = FALSE)
    for (a in seq_along(alphas)) {
        data[i, a] <- survived(g, alphas[a])
    }
    print(i)
}

# save to dataframe
df <- data.frame(data)
write.csv(df, "pa_hist_2_5_1_1000n_4m_2000means.csv")