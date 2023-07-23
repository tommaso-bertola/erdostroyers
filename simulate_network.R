# author: Tommaso Bertola

# the following file is a self consistent script to generate different
# topologies of networks
# once the global parameters and the file name to save the data are set
# uncomment the proper function 'sample_*' to generate the appropriate
# network. The code logs its activity showing how much it has computed
# of the whole task

#***********************
#    DEPENDENCIES      *
#***********************

library(igraph) # handles graphs
library(compiler) # speeds thing up by compiling the functions

#***********************
#    ATTACK PROTOCOLS  *
#***********************

source("attack_protocols.R")


#***********************
#    CONVENENT WRAPPER *
#***********************

# not all degree sequences are graphical
# to prevent errors that stop the code, repeat at maximum 80 times
# the sampling until one success if found
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

# the wrapper sampling the degree sequence according to the weights
degseq <- function(n, k_min, k_max, weights) {
    degs <- sample(c(k_min:k_max), size = n, replace = TRUE, prob = weights)

    # the sum needs to evaluate to an even number
    # correct by adding 1 if this is not the case
    if (sum(degs) %% 2 == 1) {
        degs[1] <- degs[1] + 1
    }

    # sample te graph form the degree sequence
    g <- igraph::sample_degseq(out.deg = degs, method = "vl")
    return(g)
}

# wrap and compile the functions
sample_degseq_safely <- safely(degseq)
sample_degseq_safely <- cmpfun(sample_degseq_safely)



#*************************
#    GLOBAL PARAMETERS   *
#*************************

# unique name describing the simulation process
output_name_csv <- "sim_config_model_40means.csv"

n_averages <- 40 # the number of averages each simulation is independently run

# the list of values of alpha to test the network
# it can be modified to be more fine grained
alphas <- c(
    0.00, 0.05, 0.10, 0.20,
    0.30, 0.40, 0.50, 0.60,
    0.70, 0.80, 0.90, 1.00,
    1.10, 1.20, 1.50
)

#********************************
#    SIMULATION PARAMETERS      *
#********************************

# parameters required depending on
# the network under simulation

# -> Molloy-Reed configuration model
# -> with degrees following power law distribution
# *  gamma is varied

n <- 1000 # number of nodes
k_min <- 2 # min degree
k_max <- n # max degree
gamma <- 3 # exponent
weights <- (k_min:k_max)^(-gamma) # wheights to sample the degree sequence

# -> Erods Renyi Gilbert
# *  p is varied

n <- 1000
p <- 1.1 * log(n) / n # probability of having an edge


# -> Barabasi-Albert preferential attachment
# *  m is varied

n <- 1000
m <- 3

# -> Regular lattice
# *  k is varied

n <- 1000
k <- 4


# -> Stochastic block model
# *  the Bernoulli matrix is generated randomly

n <- 1000
sizes <- runif(n = 8)
sizes <- round(sizes / sum(sizes) * 1000, 0)
sizes[1] <- 1000 - sum(sizes[-1])

prob_matr <- matrix(0, nrow = 8, ncol = 8)
prob_matr[1, 2] <- 0.008
prob_matr[1, 3] <- 0.007
prob_matr[1, 4] <- 0.008
prob_matr[1, 5] <- 0.005
prob_matr[2, 8] <- 0.003
prob_matr[4, 6] <- 0.008
prob_matr[6, 7] <- 0.007
prob_matr[7, 8] <- 0.009
prob_matr <- prob_matr + t(prob_matr)
prob_matr <- prob_matr + diag(nrow = 8) * log(max(sizes)) / max(sizes)


# -> Small-world model
# *  p is varied

n <- 1000
dim <- 1 # lattice dimension
nei <- 5 # number of neighbours
p <- nei / n # rewiring probability


#***********************
#    NETWORK CASCADE   *
#***********************
# instead of using the predefined g inside the for loops below
# use this g and put it outside the loop: read just once

# to simulate the network (in this case the power grid)
# decomment below
nodes <- read.csv("power/nodes_clean.csv")
edges <- read.csv("power/edges.csv")

el <- as.matrix(edges + 1)
# g <- graph_from_edgelist(el = el, directed = FALSE)

# used for the null model comparison 
# decomment if needed
# g <- sample_degseq(out.deg = degree(g), method = "simple.no.multiple.uniform")



#***********************************
#    ACTUAL HEAVY STUFF TO RUN     *
#***********************************

# vector to store the values
g_alphas <- vector()
err_g_alphas <- vector()
g_alphas_random <- vector()
err_g_alphas_random <- vector()
g_alphas_deg <- vector()
err_g_alphas_deg <- vector()

# for each alpha in the sequence
for (a in seq_along(alphas)) {
    # vector to store the results of the
    # independent trials
    avg_g <- vector()
    avg_g_random <- vector()
    avg_g_deg <- vector()

    # for each independent average
    for (i in 1:n_averages) {
        # do one of these samplings
        # uncomment the chosen one
        g <- sample_gnp(n = n, p = p)
        # g <- sample_pa(n = n, m = 4, directed = FALSE)
        # g <- sample_degseq_safely(n, k_min, k_max, weights)
        # g <- sample_k_regular(no.of.nodes = n, k = k, directed = FALSE)
        # g <- sample_sbm(n, prob_matr, sizes, directed = FALSE)
        # g <- sample_smallworld(dim, n, nei, p)

        # compute the ratio depending on the attack protocol
        avg_g[i] <- survived(g, alphas[a])
        avg_g_random[i] <- survived_random(g, alphas[a])
        avg_g_deg[i] <- survived_deg(g, alphas[a])

        # print the status, just to know how long the execution takes
        print(paste(i, alphas[a]))
    }

    # compute all the averages
    g_alphas[a] <- mean(avg_g)
    g_alphas_random[a] <- mean(avg_g_random)
    g_alphas_deg[a] <- mean(avg_g_deg)
    err_g_alphas[a] <- sd(avg_g)
    err_g_alphas_random[a] <- sd(avg_g_random)
    err_g_alphas_deg[a] <- sd(avg_g_deg)

    # print the alpha to know how long the execution lasts
    print(alphas[a])
}

# store into dataframes the output
# data for easier management

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

# combine the dataframes
conf <- rbind(btw, rnd, deg)

# save to a csv file
# to be later graphed
write.csv(conf, output_name_csv)
