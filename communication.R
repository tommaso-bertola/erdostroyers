library(igraph)

nodes_df = read.csv("Kdl/nodes_cleaned.csv")
edges_df = read.csv("Kdl/edges.csv")

num_nodes = nrow(nodes_df)
nodes_df[, "queue"] = NA

g = graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)

d = distances(g)

p = 10

packets = data.frame("id"=integer(), "curr"=integer(), "dest"=integer(), "time"=integer())
p_id = 0
tmax = 1000

times = c()

for(t in 1:tmax) {
    if(!(t%%20)) {print(t);flush.console();}
    for(i in 1:p) {
        #generate new packet
        packets = rbind(packets, data.frame("id"=p_id, "curr"=as.integer(runif(1, 1, num_nodes+1)), "dest"=as.integer(runif(1, 1, num_nodes+1)), "time"=0))
        p_id = p_id + 1
    }
    to_remove = c()
    for(j in 1:nrow(packets)) {
        #decide movement
        a=d[packets[j, "curr"],]
        neighbors = (1:num_nodes)[a == 1]
        distances = d[neighbors, packets[j, "dest"]]
        closest_neighbors = neighbors[distances == min(distances)]
        packets[j, "curr"] = closest_neighbors[sample(length(closest_neighbors), 1)]
        packets[j, "time"] = packets[j, "time"] + 1
        if(packets[j, "curr"] == packets[j, "dest"]) {
            to_remove = c(to_remove, j)
            times = c(times, packets[j, "time"])
        }
    }
    if(length(to_remove) != 0) {
        packets = packets[-to_remove,]
    }
}
