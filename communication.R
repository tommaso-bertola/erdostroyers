library(igraph)

nodes_df = read.csv("network/nodes_cleaned.csv")
edges_df = read.csv("network/edges.csv")

num_nodes = nrow(nodes_df)
nodes_queue = list()
for(i in 1:num_nodes+1){
    nodes_queue[i]=c()
}

g = graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)

d = distances(g)

new_packets = c(0.5, 1, 3)

tmax = 1000

A = matrix(nrow=length(new_packets), ncol=tmax)

h = 0.75

for(pi in 1:length(new_packets)) {
    times = c()
    p = new_packets[pi]/num_nodes
    
    packets = data.frame("id"=integer(), "curr"=integer(), "dest"=integer(), "time"=integer())
    p_id = 0
    
    print(p)
    flush.console()
    
    for(t in 1:tmax) {
        if(!(t%%200)) {print(t);flush.console();}
#         for(i in 1:p) {
#             #generate new packet
#             curr = as.integer(runif(1, 1, num_nodes+1))
#             packets = rbind(packets, data.frame("id"=p_id, "curr"=curr, "dest"=as.integer(runif(1, 1, num_nodes+1)), "time"=0))
#             
#             nodes_queue[[curr]] = c(nodes_queue[[curr]], p_id)
#             p_id = p_id + 1
#         }
        probs = runif(num_nodes)
        for(i in 1:num_nodes) {
            if(probs[i]<p){
                packets = rbind(packets, data.frame("id"=p_id, "curr"=i, "dest"=as.integer(runif(1, 1, num_nodes+1)), "time"=0))
                nodes_queue[[i]] = c(nodes_queue[[i]], p_id)
                p_id = p_id + 1
            }
        }
        to_remove = c()
        for(j in 1:nrow(packets)) {
            #print(nodes_queue[[packets[j,"curr"]]])
            #flush.console()
            if(nodes_queue[[packets[j,"curr"]]][1] == packets[j, "id"]) {
                nodes_queue[[packets[j,"curr"]]] = nodes_queue[[packets[j,"curr"]]][-1]
                #decide movement
                a=d[packets[j, "curr"],]
                neighbors = (1:num_nodes)[a == 1]
                distances = d[neighbors, packets[j, "dest"]]
                closest_neighbors = neighbors[distances == min(distances)]
                packets[j, "curr"] = closest_neighbors[sample(length(closest_neighbors), 1)]
                nodes_queue[[packets[j, "curr"]]] = c(nodes_queue[[packets[j, "curr"]]], packets[j, "id"])
                if(packets[j, "curr"] == packets[j, "dest"]) {
                    to_remove = c(to_remove, j)
                    times = c(times, packets[j, "time"])
                }
            }
            packets[j, "time"] = packets[j, "time"] + 1
        }
        if(length(to_remove) != 0) {
            packets = packets[-to_remove,]
        }
        A[pi,t] = nrow(packets)
    }
    print(mean(times))
    print(length(times)/(tmax*p))
    print(mean(packets[,"time"]))
}
