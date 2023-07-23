library(igraph)
if(F) {
    load("modbagraph.RData")
    num_nodes = length(V(g)$name)
    print("using BA-like network")
} else {
    nodes_df = read.csv("network/nodes_cleaned.csv")
    edges_df = read.csv("network/edges.csv")
    
    num_nodes = nrow(nodes_df)
    
    g = graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)
    print("using internet network")
}

d = distances(g)

new_packets = c(35, 40, 45)

tmax = 1000

A = matrix(nrow=length(new_packets), ncol=tmax)

# h = 0.82
h = 1

for(pi in 1:length(new_packets)) {
    nodes_queue = list()
    for(i in 1:num_nodes+1){
        nodes_queue[i]=c()
    }
    
    times = c()
    p = new_packets[pi]/num_nodes
    
    packets = data.frame("id"=integer(), "curr"=integer(), "dest"=integer(), "time"=integer())
    p_id = 0
    
    print("new cycle")
    print(p)
    flush.console()
    
    for(t in 1:tmax) {
        if(!(t%%200)) {print(t);flush.console();}
#         for(i in 1:p) {
#             #generate new packet
#             curr = as.integer(runif(1, 1, num_nodes+1))
#             packets = rbind(packets, data.frame("id"=p_id, "curr"=curr, "dest"=as.integer(runif(1, 1, num_nodes+1)), "time"=1))
#             
#             nodes_queue[[curr]] = c(nodes_queue[[curr]], p_id)
#             p_id = p_id + 1
#         }
        probs = runif(num_nodes)
        for(i in 1:num_nodes) {
            if(probs[i]<p){
                packets = rbind(packets, data.frame("id"=p_id, "curr"=i, "dest"=as.integer(runif(1, 1, num_nodes+1)), "time"=1))
                nodes_queue[[i]] = c(nodes_queue[[i]], p_id)
                p_id = p_id + 1
            }
        }
        to_remove = c()
        nodes_ready = rep(T, num_nodes)
        if(nrow(packets)!=0) {
            for(j in 1:nrow(packets)) {
                if(nodes_queue[[packets[j, "curr"]]][1] == packets[j, "id"] && nodes_ready[packets[j, "curr"]]) {
                    nodes_queue[[packets[j, "curr"]]] = nodes_queue[[packets[j, "curr"]]][-1]
                    nodes_ready[packets[j, "curr"]] = F
                    #decide movement
                    a=d[packets[j, "curr"],]
                    neighbors = (1:num_nodes)[a == 1]
                    distances = h * d[neighbors, packets[j, "dest"]] + (1-h) * sapply(nodes_queue[neighbors], length)
                    closest_neighbors = neighbors[distances == min(distances)]
                    packets[j, "curr"] = closest_neighbors[sample(length(closest_neighbors), 1)]
                    if(packets[j, "curr"] == packets[j, "dest"]) {
                        to_remove = c(to_remove, j)
                        times = c(times, packets[j, "time"])
                    } else {
                        nodes_queue[[packets[j, "curr"]]] = c(nodes_queue[[packets[j, "curr"]]], packets[j, "id"])
                    }
                }
                packets[j, "time"] = packets[j, "time"] + 1
            }
        }
        if(length(to_remove) != 0) {
            packets = packets[-to_remove,]
        }
        A[pi,t] = nrow(packets)
    }
     print(mean(times)) #mean time of packets arriving
     print(length(times)/(p_id)) #fraction of packets arriving
     print(mean(packets[,"time"])) #mean time of packets not arriving
}



colors=c("red", "green", "blue", "pink", "violet", "orange", "#eeee00")

# png("A_net.png")
# plot(A[length(new_packets),])
# ymax=par("usr")[4]/new_packets[length(new_packets)]
# plot(A[length(new_packets),]/new_packets[length(new_packets)], type="l", main=NA, xlab="time", ylab="Active packets", ylim=c(0, ymax))
# if(length(new_packets)!=1) {
#     for(i in 1:(length(new_packets)-1)) {
#         lines(A[i,]/new_packets[i], col=colors[i])
#     }
# }
# dev.off()

diffs = c()
for(i in 1:length(new_packets)) {
    diffs=c(diffs, mean(diff(A[i,(tmax-400):tmax], lag=10))/(10*new_packets[i]))
}
print(diffs)
