library(data.table)
library(igraph)
library(ggplot2)
theme_set(
  theme_bw(base_size = 15, base_family = "Fira Sans Condensed")
)
source("source/sandpile.R")
source("source/size_distribution_sf.R")

edg_info <- fread("./opsahl-powergrid/out.opsahl-powergrid", skip = 2)
g <- graph_from_edgelist(as.matrix(edg_info), directed = FALSE)

max_cap <- sum(degree(g)-1)

sp <- sandpile(g, n_iters = max_cap*5, sink_frac = 1e-2)
plot(sp$sizes)
sizes <- data.table(size = sp$sizes)[, .(count = .N), by = size]
tau <- lm(size~count, data = log10(sizes))
tau$coefficients[[2]]

tau0 <- c()
for(i in 1:5){
  g0 <- degree.sequence.game(out.deg = degree(g), method = 'vl')
  sp0 <- sandpile(g0, n_iters = gsize(g0)*2, sink_frac = 1e-2)
  sizes0 <- data.table(size = sp0$sizes)[, .(count = .N), by = size]
  t <- lm(size~count, data = sizes0)
  tau0 <- c(tau0,t$coefficients[[2]])
}
mu <- mean(tau0)
s <- sd(tau0)

gg <- ggplot(sizes, aes(x=size, y=count)) + 
  geom_point() + geom_line() +
  scale_x_log10() + scale_y_log10() + annotation_logticks()
gg
gg0 <- ggplot(sizes0, aes(x=size, y=count)) + 
  geom_point() + geom_line() +
  scale_x_log10() + scale_y_log10() + annotation_logticks()

d <- c()
for(i in 1:20){
  g0 <- degree.sequence.game(out.deg = degree(g), method = 'vl')
  d <- c(d, diameter(g0))
}
