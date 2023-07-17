library(igraph)

nodes_df=read.csv("network/nodes_cleaned.csv")
edges_df=read.csv("network/edges.csv")

g=graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)

l1=matrix(nrow=nrow(nodes_df), ncol=2, NA)
l1[,1]=nodes_df[["xpos"]]
l1[,2]=nodes_df[["ypos"]]
plot(g, layout=l1, vertex.size=0.2, vertex.label=NA, edge.width=0.2)




nodes_df=read.csv("CA/nodes_cleaned.csv")
edges_df=read.csv("CA/edges.csv")

g=graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)

l1=matrix(nrow=nrow(nodes_df), ncol=2, NA)
l1[,1]=nodes_df[["xpos"]]
l1[,2]=nodes_df[["ypos"]]
plot(g, layout=l1, vertex.size=0.2, vertex.label=NA, edge.width=0.2)
