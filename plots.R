library(igraph)
library(ggplot2)
library(data.table)

source("source/sandpile.R")

my_font <- "Fira Sans"
my_pal <- wesanderson::wes_palette("Zissou1", 5)[c(1, 3, 5)]

wus <- fread("opsahl-powergrid/out.opsahl-powergrid", skip = 2)
g_wus <- graph_from_edgelist(as.matrix(wus), directed = FALSE)

sp <- sandpile(g_wus, n_iters = 4e4, sink_frac = 0.01)

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
