library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/size_distribution_sf.R")

#cycle over type and number of edges
type_list <- c('scale-free','erdosrenie', 'small-world', 'barabasis')

#average k list
k_list <- c(12,15,20,25,30,40,50)
df <- data.frame()

for(type in type_list){
  print(type)
  eta_list <- c()
  edge_list <- c()
  clus_list <- c()
  diam_list <- c()
  
  for(kl in k_list){
    n <- 5000
    m <- kl*n/2
    
    if(type=='scale-free'){
      repeat {
      g <- sample_fitness_pl(
          no.of.nodes = n,
          no.of.edges = m,
          exponent.out = 2.5
        )
        # we want only one cc
        if (clusters(g)$no == 1) {
          print("Found network with 1 CC, proceeding...")
          break
        }
      }
    }
    if(type=='erdosrenie'){
      g <- erdos.renyi.game(n = n, p.or.m = m, type = 'gnm')
    }
    
    if(type=='small-world'){
      g <- sample_smallworld(1, n, m/n, p = 0.005, loops = FALSE, multiple = FALSE)
    }
    
    if(type=='barabasis'){
      g <- barabasi.game(n = n, m = m/n, directed = F)
    }
    
    max_cap <- sum(degree(g)-1)
    sp <- sandpile(graph = g, n_iters = max_cap, sink_frac = 1e-4, stop_at_soc = T)
    #computing the efficacy
    eta <- max(sp$tot_gr)/max_cap
    eta_list <- c(eta_list, eta)
    clus_list <- c(clus_list, transitivity(g))
    diam_list <- c(diam_list, diameter(g))
    edge_list <- c(edge_list, ecount(g))
  }
  
  df <- rbind(df, data.frame(network = rep(type, length(k_list)), eta=eta_list,
                             clus = clus_list,  diam = diam_list, m = edge_list))
}

fwrite(df, "out-data/eta_dataframe.out")
ggplot(data = df) +
  geom_line(aes(x = m, y = eta, colour = network), linewidth = 1)

ggplot(data = df) +
  geom_line(aes(x = m, y = diam, colour = net), linewidth = 1)

ggplot(data = df) +
  geom_line(aes(x = m, y = clus, colour = net), linewidth = 1)
