library(igraph)
if(T) {
  load("tree.RData")
  num_nodes = length(V(g)$name)
  print("using hierarchical tree network")
} else {
  nodes_df = read.csv("network/nodes_cleaned.csv")
  edges_df = read.csv("network/edges.csv")
  
  num_nodes = nrow(nodes_df)
  
  g = graph_from_data_frame(edges_df, directed=F, vertices=nodes_df)
  print("using internet network")
}

d = distances(g)

#new_packets = c(0.5, 1.5, 3, 10)
new_packets= c(1,2,3)

tmax = 1000

A = matrix(nrow=length(new_packets), ncol=tmax)

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
    if(nrow(packets)!=0) {
      for(j in 1:nrow(packets)) {
          #decide movement
          a=d[packets[j, "curr"],]
          neighbors = (1:num_nodes)[a == 1]
          distances = h * d[neighbors, packets[j, "dest"]] + (1-h) * sapply(nodes_queue[neighbors], length)
          closest_neighbors = neighbors[distances == min(distances)]
          #following line different assigning strategy
          #packets[j, "curr"] = closest_neighbors[sample(length(closest_neighbors), 1)]
          #first sample one of the closest neighbors randomly, then compute q_ij
          closest_neighbor = closest_neighbors[sample(length(closest_neighbors), 1)]
          gamma = 1.5
          #xab = runif(1, min=0, max=1) #change to one to switch off noise
          xab = 1
          fa = ifelse(length(nodes_queue[[packets[j,"curr"]]])>0, length(nodes_queue[[packets[j,"curr"]]]), 1)
          kab = xab*fa^(-gamma)
          #xba = runif(1, min=0, max=1) #change to one to switch off noise
          xba = 1
          fb = ifelse(length(nodes_queue[[closest_neighbor]])>0, length(nodes_queue[[closest_neighbor]]), 1)
          kba = xba*fb^(-gamma)
          qab = sqrt(kab*kba)
          hop = runif(1, 0, 1)
          if(hop<qab){
            nodes_queue[[packets[j,"curr"]]] = nodes_queue[[packets[j,"curr"]]][-1]
            packets[j, "curr"] = closest_neighbor
            #i put this block inside here
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

png("A_net")
plot(A[length(new_packets),], type="l", main=NA, xlab="time", ylab="Active packets")
if(length(new_packets)!=1) {
  for(i in 1:(length(new_packets)-1)) {
    lines(A[i,])
  }
}
dev.off()

###order parameter

