library(igraph)
library(ggplot2)
library(data.table)

source("source/sandpile.R")

my_font <- "Fira Sans"
my_pal <- wesanderson::wes_palette("Zissou1", 5)[c(1, 3, 5)]

wus <- fread("network-data/out.opsahl-powergrid", skip = 2)
g_wus <- graph_from_edgelist(as.matrix(wus), directed = FALSE)

sp <- sandpile(g_wus, n_iters = 4e4, sink_frac = 0.01)

data.frame(iter = sp$whens, sizes = sp$sizes) |>
  ggplot(aes(iter, sizes)) +
    geom_point(size = 0.15, alpha = 0.25) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(x = "Iteration", y = "Avalanche size") +
    theme_bw(base_size = 10, base_family = my_font)

ggsave(
  "figures/sizes_wus.pdf", device = cairo_pdf,
  width = 11, height = 7, units = "cm"
)

data.frame(iter = sp$whens, grains = sp$tot_gr) |>
  ggplot(aes(iter, grains)) +
    geom_line(linewidth = 0.25) +
    geom_hline(
      yintercept = ecount(g_wus),
      colour = "firebrick",
      linewidth = 0.25
    ) +
    annotate(
      "text", label = paste0("m = ", ecount(g_wus)),
      x = 2500, y = ecount(g_wus) - 250,
      colour = "firebrick", size = 9 * 25.4 / 72
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    labs(x = "Iteration", y = "Total load in the network") +
    theme_bw(base_size = 10, base_family = my_font)

ggsave(
  "figures/total_grains_wus.pdf", device = cairo_pdf,
  width = 11, height = 7, units = "cm"
)

eta <- fread("eta_dataframe.out")
net_names <- c("Scale-free", "Erdős-Rényi", "Watts-Strogatz", "Barabási")
names(net_names) <- eta[, unique(network)]

eta$network <- forcats::fct_relevel(
  factor(eta$net), "small-world", "erdosrenie", "scale-free", "barabasis"
)

ggplot(eta, aes(m, eta, colour = network, shape = network)) +
  geom_point(size = 1) +
  geom_line(linewidth = 0.25) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_colour_brewer(palette = "Dark2", labels = net_names) +
  scale_shape_discrete(labels = net_names) +
  labs(
    x = "Number of edges m", y = "Effective capacity η",
    colour = "Model", shape = "Model"
  ) +
  theme_bw(base_size = 7, base_family = my_font)

ggsave(
  "figures/efficacy_comparison.pdf", device = cairo_pdf,
  width = 9.5, height = 5, units = "cm"
)
