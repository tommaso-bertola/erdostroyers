library(igraph) # for graphs
library(tidyverse) # for working with dataframes
library(ggplot2) # for plots
library(dplyr) #for queries
library(latex2exp) #for latex
options(warn=-1)

#this functions implants the seed of state 1 in the network, 
#by random as deafault 
#and as many sees as many is specified by the parameter n.seed

seed <- function(nodes.state, n.seeds = 1, method = 'random'){
    
    N <- length(nodes.state)
    
    if (method == 'random'){
        seeds.id <- sample.int(N, n.seeds)
    }
    
    nodes.state[seeds.id] <- 1
    
    return(nodes.state)
}

#this function is meanted for networks that do not 
#change their structure during the evolution of the dynamics.
#The function update the state of each node of a threshold based 
#on his neighbours and on the node threshold

update <- function(nodes.state, nodes.threshold, nodes.neighbors){
    
    N <- length(nodes.state)
    
    nodes.state.in <- nodes.state
    
    nodes.state.out <- nodes.state
    
    for (i in 1:N){
    
        if (nodes.state[i] != 1){
            
            if(length(nodes.neighbors[[i]])!=0){
        
                if(sum(nodes.state.in[nodes.neighbors[[i]]])/length(nodes.neighbors[[i]]) >= nodes.threshold[i]){
                    nodes.state.out[i] <- 1
                }
            }          
        
        }
    
    }
    
    return(nodes.state.out) 
}


n <- 10000 #number of nodes
threshold.list <- seq(from = 0.1, to = 0.25, by=0.01) #values of threshold used
z.list <- seq(from=1, to=30, by=1) #values of avarage degree used
max.update <- 100   #maximum number of updates
n.seeds <- 1 # number of initial seeds
n.run <- 25 #number of runs for each set of parameters
filename <- 'data/ERG_simulation_data.csv' # file in which to save the results

#writing the head in the file
write.table(list('nodes', 'max.update', 'n.seeds', 'threshold', 'z', 'time', 'n.run', 'cascade.fraction'),
                    file = filename,
                    append = TRUE,
                    sep = ',', 
                    row.names = FALSE,
                    col.names = FALSE)


for (phi in threshold.list){ #for loop on the threshold
    
    for (z in z.list){ #for loop on z
               
        for(k in 1:n.run){ #repeat with the same parameters n.run times
            
            #Initialization fo the network
            cascade <- 0
            
            nodes.threshold <- rep(phi, n)

            p=z/n

            g <- erdos.renyi.game(n = n,
                         p.or.m = p,  
                         type = "gnp",
                         directed = FALSE,
                         loops = FALSE)

            #Saving the negihbours of each node
            nodes.neighbors <- list()
            for (i in 1:n){
                nodes.neighbors[i] <- list(neighbors(g, i))
            }

            #initializaing the state of each node
            nodes.state <- numeric(n) #initializing to zero all nodes state
            t <- 0   #initial time

            nodes.state <- seed(nodes.state, n.seeds) #inserting some seeds
            t <- 1   #time at which happen the seed

            #updating the state of the nodes maximum max.update times
            for (j in 1:max.update){

                nodes.state.previous <- nodes.state

                nodes.state <- update(nodes.state, nodes.threshold, nodes.neighbors)

                #adjourn of the time
                t <- t+1

                #break if the state remain the same
                if(identical(nodes.state, nodes.state.previous)){
                    break
                }
            }

            #saving the fraction of nodes that are in state 1
            cascade <- sum(nodes.state)/n
         
            #saving the results in a csv file
            write.table(list(n, max.update, n.seeds, phi, z, t, k, cascade),
            file = filename,
            append = TRUE,
            sep = ',', 
            row.names = FALSE,
            col.names = FALSE)
            
            if(cascade>0.1){  #we are only interested in finding that a network does cascade with these parameters
                break
            }
            
            
        }
    }
}

n <- 10000 #number of nodes
threshold.list <- c(0.18) #values of threshold used
z.list <- seq(from=0.5, to=7, by=0.1) #values of avarage degree used
max.update <- 100   #maximum number of updates
n.seeds <- 1 # number of initial seeds
n.run <- 100 #number of runs for each set of parameters
filename <- 'data/dataERG_simulation_time.csv' #file name in whoch to save data

write.table(list('nodes', 'max.update', 'n.seeds', 'threshold', 'z', 'time', 'n.run', 'cascade.fraction'),
                    file = filename,
                    append = TRUE,
                    sep = ',', 
                    row.names = FALSE,
                    col.names = FALSE)


for (phi in threshold.list){
    
    for (z in z.list){ 
        
        for(j in 1:n.run){
            
            cascade <- 0
            
            nodes.threshold <- rep(phi, n)

            p=z/n

            g <- erdos.renyi.game(n = n,
                         p.or.m = p,  
                         type = "gnp",
                         directed = FALSE,
                         loops = FALSE)

            nodes.neighbors <- list()
            for (i in 1:n){
                nodes.neighbors[i] <- list(neighbors(g, i))
            }


            nodes.state <- numeric(n) #initializing to zero all nodes state
            t <- 0   #initial time

            nodes.state <- seed(nodes.state, n.seeds) #inserting some seeds
            t <- 1   #time at which happen the seed

            for (j in 1:max.update){

                nodes.state.previous <- nodes.state

                nodes.state <- update(nodes.state, nodes.threshold, nodes.neighbors)

                t <- t+1

                if(identical(nodes.state, nodes.state.previous)){
                    break
                }
            }


            cascade <- sum(nodes.state)/n

            write.table(list(n, max.update, n.seeds, phi, z, t, n.run, cascade),
            file = filename,
            append = TRUE,
            sep = ',', 
            row.names = FALSE,
            col.names = FALSE)
        }
    }
}

n <- 10000 #number of nodes
threshold.list <- seq(from = 0.1, to = 0.3, by=0.02)  #mean of the threshold
z.list <- seq(from=1, to=15, by=1) #avarage degree used
max.update <- 100   #maximum number of updates
n.seeds <- 1 # number of initial seeds
n.run <- 25 #number of runs for each set of parameters
filename <- 'ERG_simulation_sigma0.05.csv' #file in which save the results
sd <- 0.05  #standard deviation 

write.table(list('nodes', 'max.update', 'n.seeds', 'threshold', 'z', 'time', 'n.run', 'cascade.fraction'),
                    file = filename,
                    append = TRUE,
                    sep = ',', 
                    row.names = FALSE,
                    col.names = FALSE)


for (phi in threshold.list){
    
    for (z in z.list){
               
        for(k in 1:n.run){
            
            cascade <- 0
            
            nodes.threshold <- rnorm(n=n, mean =phi , sd=sd) #the threshold is now normally sampled

            p=z/n

            g <- erdos.renyi.game(n = n,
                         p.or.m = p,  
                         type = "gnp",
                         directed = FALSE,
                         loops = FALSE)

            nodes.neighbors <- list()
            for (i in 1:n){
                nodes.neighbors[i] <- list(neighbors(g, i))
            }


            nodes.state <- numeric(n) #initializing to zero all nodes state
            t <- 0   #initial time

            nodes.state <- seed(nodes.state, n.seeds) #inserting some seeds
            t <- 1   #time at which happen the seed

            for (j in 1:max.update){

                nodes.state.previous <- nodes.state

                nodes.state <- update(nodes.state, nodes.threshold, nodes.neighbors)

                t <- t+1

                if(identical(nodes.state, nodes.state.previous)){
                    break
                }
            }

            cascade <- sum(nodes.state)/n
         

            write.table(list(n, max.update, n.seeds, phi, z, t, k, cascade),
            file = filename,
            append = TRUE,
            sep = ',', 
            row.names = FALSE,
            col.names = FALSE)
            
            if(cascade>0.1){  #we are only interested in finding that a network does cascade with these parameters
                break
            }
            
        }
    }
}

n <- 10000 #number of nodes
threshold.list <-seq(from = 0.1, to = 0.25, by=0.2) #threshold list
m.list <- seq(from=1, to=20, by=1) # z=2m for a Barabasi Albert model
max.update <- 100   #maximum number of updates
n.seeds <- 1 # number of initial seeds
n.run <- 25 #number of runs for each set of parameters
filename <- 'BA_simulation_data.csv'

write.table(list('nodes', 'max.update', 'n.seeds', 'threshold', 'z', 'time', 'n.run', 'cascade.fraction'),
                    file = filename,
                    append = TRUE,
                    sep = ',', 
                    row.names = FALSE,
                    col.names = FALSE)


for (phi in threshold.list){
    
    for (m in m.list){
               
        for(k in 1:n.run){
            
            #Initialinz a Barabasi-Albert network
            cascade <- 0
            
            nodes.threshold <- rep(phi, n)

            g <- barabasi.game(n, m = m, directed=FALSE)

            nodes.neighbors <- list()
            for (i in 1:n){
                nodes.neighbors[i] <- list(neighbors(g, i))
            }


            nodes.state <- numeric(n) #initializing to zero all nodes state
            t <- 0   #initial time

            nodes.state <- seed(nodes.state, n.seeds) #inserting some seeds
            t <- 1   #time at which happen the seed

            for (j in 1:max.update){

                nodes.state.previous <- nodes.state

                nodes.state <- update(nodes.state, nodes.threshold, nodes.neighbors)

                t <- t+1

                if(identical(nodes.state, nodes.state.previous)){
                    break
                }
            }

            cascade <- sum(nodes.state)/n
         

            write.table(list(n, max.update, n.seeds, phi, mean(degree(g)), t, k, cascade),
            file = filename,
            append = TRUE,
            sep = ',', 
            row.names = FALSE,
            col.names = FALSE)
            
            if(cascade>0.1){
                break
            }  
        }
    }
}

filename <- 'data/ERG_simulation_data.csv'  # file with data
#reading the data
df <- read.csv(filename)
#cleaning the data
data <- df %>% group_by(threshold, z)%>% 
    arrange(desc(cascade.fraction), .by_group=TRUE) %>% 
    filter(row_number()==1)

#selecting systems that show cascade and not
cascade <- data %>% filter(cascade.fraction>=0.1)
no_cascade <- data %>% filter(cascade.fraction <0.1)

##Gamma incomplete function with upper and lower specification for different branch

gamma_inc <- function(a, x, type='upper'){
    out <- 0
    if(type=='upper'){
        out <- integrate(integrand, a=a, x, Inf)
    }
    else if(type=='lower'){
        out <- integrate(integrand, a=a, 0, x)
    }
    return(out$value)
}

#integrand in the gamma incomplete function
integrand <- function(t, a){
    return(t^(a-1)*exp(-t))
}

#theoretical cascade condition: condition==0
condition <- function(z, phi, type='upper'){
    out <- 0
    if(type=='upper'){
        out <- z*gamma_inc(floor(1/phi)-1, z, 'upper')-1
    }
    else if(type=='lower'){
        out <- z*gamma_inc(floor(1/phi)-1, z, 'lower')-1
    }
    return(out)
}

#Parameters for numerical solution of the condition
lower_z=1  #lower value of z
upper_z=40  #upper values of z
n_intervals=1000  #number of intervals
phi.list <- seq(from = 0.1, to = 0.25, by=0.001) #list of threshold considered

#Upper branch of the gamma incomplete function roots
z.roots.upper <- c()
phi.roots.upper <- c()
for(phi in phi.list){

    h <- (upper_z-lower_z)/n_intervals
    segments <- seq(from= lower_z, to=upper_z, by=h)

    for (i in 1:(n_intervals-1)){
        if(condition(segments[i], phi, 'upper')*condition(segments[i+1], phi, 'upper')<0){
            zero <- uniroot(condition, phi=phi, type='upper', lower=segments[i], upper=segments[i+1])$root
            z.roots.upper <- c(z.roots.upper, zero)
            phi.roots.upper <- c(phi.roots.upper, phi)
        }
    }
}

#Lower branch of the gamma incomplete function roots
z.roots.lower <- c()
phi.roots.lower <- c()
for(phi in phi.list){

    h <- (upper_z-lower_z)/n_intervals
    segments <- seq(from= lower_z, to=upper_z, by=h)

    for (i in 1:(n_intervals-1)){
        if(condition(segments[i], phi, 'lower')*condition(segments[i+1], phi, 'lower')<0){
            zero <- uniroot(condition, phi=phi, type='lower', lower=segments[i], upper=segments[i+1])$root
            z.roots.lower <- c(z.roots.lower, zero)
            phi.roots.lower <- c(phi.roots.lower, phi)
        }
    }
}

#Saving the theory prediction
teor.lower <- data.frame(phi.roots.lower, z.roots.lower)
teor.upper <- data.frame(phi.roots.upper, z.roots.upper)

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot() +theme_light()+
    geom_point(data = cascade, aes(x=threshold,y=z,col='Cascade simulations', ))+
    geom_point(data = no_cascade, aes(x=threshold,y=z,col='No cascade simulations'))+
    geom_step(data=teor.lower, aes(x=phi.roots.lower, y=z.roots.lower, col='Theoretical cascade limit'))+
    geom_step(data=teor.upper, aes(x=phi.roots.upper, y=z.roots.upper, col='Theoretical cascade limit'))+
    xlab(label = TeX('Threshold $\\Phi$'))+
    xlim(0.1, 0.25)+
    ylim(1, 30)+
    ylab(label = TeX('Avarage degree $z$'))+
    theme(text = element_text(size = 20), legend.position = c(0.76, 0.9))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c("red", "blue", 'black'))+
    guides(color = guide_legend(title = "Legend"))

#ggsave('figures/CC_ERG_simulations.png')

filename <- 'data/ERG_simulation_time.csv'

df <- read.csv(filename)
times <- df %>% group_by(z) %>% summarize(mean_time=mean(time)-2, var=sd(time)) 
#mean(time)-2 beacuase of how time is accounted in simulations

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot()+ theme_light()+
    #annotate("text", x=3.5, y=30, size=12,label = TeX('$\\Phi=0.18$'))+
    geom_point(data=times, aes(x=z, y=mean_time, col='Evolution time'))+
    geom_errorbar(data=times, aes(x=z, ymin=mean_time-var, ymax=mean_time+var))+
    xlab(label = TeX('Avarage degree $z$'))+
    scale_x_continuous(breaks = seq(1, 7, by = 1))+
    ylab(label = TeX('Time [number of steps]'))+
    theme(text = element_text(size = 20), legend.position = c(0.76, 0.9))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c('black'))+
    guides(color = guide_legend(title = TeX('$\\Phi$   = 0.18')))

#ggsave('figures/CC_ERG_time.png')

filename <- 'data/ERG_simulation_time.csv'

df <- read.csv(filename)

#selecting only the simulations with cascade failures
df.cascade <- df %>% filter(cascade.fraction>0.1) %>% 
                group_by(z) %>% 
                summarize(mean=mean(cascade.fraction), sd=sd(cascade.fraction))

#Condition for connected componente size: condition==0
connected_component <- function(S, z){
    return(S-1+exp(-z*S))
}

#Parameters to solve the connect component size
z <- seq(1.1, 7, by=0.1)
lower <- rep(0.1, length(z))
upper <- rep(1, length(z))

#Solution of connected component size
S <- c()
for (i in seq_along(z)){
    root <- uniroot(connected_component, z=z[i], lower=lower[i], upper=upper[i])$root
    S <- c(S, root )
    }
prediction <- data.frame(z, S)

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot()+ theme_light()+
    #annotate("text", x=2, y=1, size=12,label = TeX('$\\Phi=0.18$'))+
    geom_point(data=df.cascade, aes(x=z, y=mean, col='Cascade fraction'))+
    geom_errorbar(data=df.cascade, aes(x=z, ymin=mean-sd, ymax=mean+sd))+
    geom_line(data=prediction, aes(x=z, y=S, col='Theoretical connected component'))+
    xlab(label = 'Average degree z')+
    xlab(label = TeX('Avarage degree $z$'))+
    scale_x_continuous(breaks = seq(1, 7, by = 1))+
    ylab(label = TeX('Cascade fraction'))+
    theme(text = element_text(size = 20), legend.position = c(0.70, 0.1))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c('black', 'red'))+
    guides(color = guide_legend(title = TeX('$\\Phi$   = 0.18')))

#ggsave('figures/CC_connected_component.png')

filename <- 'data/ERG_simulation_time.csv'

df <- read.csv(filename)

n.run <-100 #number of attemps for each configuration

#computing the theoretical prediction
count_prob <- df %>% group_by(z) %>% filter(cascade.fraction>0.1) %>% summarize(prob = n())
count_prob['prob']<- count_prob['prob']/n.run
z.values <- unique(df['z'])

#saving the prob for each simulation
prob <- merge(x = z.values, y = count_prob, by = 'z', all.x = TRUE) %>% mutate_all(~replace(., is.na(.), 0))

z.list <- seq(from=0.5, to=7, by=0.1) #value of z
n <- 10000 #number of nodes
phi <- 0.18 #threshold
n.run <- 10  #number of run

filename <- 'probability_prediction.csv' #file in which to save the predictions

#vectors to save the results
Sv.list <-c()
Se.list <-c()

#writing the head in the files
write.table(list('n', 'z', 'phi', 'n.run', 'Sv', 'Se'),
           file = filename,
           append = TRUE,
           sep = ',', 
           row.names = FALSE,
           col.names = FALSE)

#for loop on the z parameter
for(z in z.list){
    Sv <- 0
    Se <- 0
    #running n.run times
    for(i in 1:n.run){
        
        #Realization of a network
        p=z/n

        g <- erdos.renyi.game(n = n,
            p.or.m = p,  
            type = "gnp",
            directed = FALSE,
            loops = FALSE)
        
        #Computing the neighbors
        nodes.neighbors <- list()

        for (i in 1:n){
            nodes.neighbors[i] <- list(neighbors(g, i))
        }
        
        #Computing the vulnerable nodes
        nodes.vulnerable <- c()
        for (j in seq_along(nodes.neighbors)){
            if(length(nodes.neighbors[[j]])<1/phi){
                nodes.vulnerable <- c(nodes.vulnerable, j)
            }
        }
        
        #Calculating the vulnerable subgraph connected component
        if(length(nodes.vulnerable)!=0){
            gv<-subgraph(g, nodes.vulnerable)
            components <- clusters(gv, mode="strong")
            max_size_gv <- max(components$csize)
            biggest_cluster_id <- which.max(components$csize)
            vert_ids <- V(gv)[components$membership == biggest_cluster_id]
        }else {
            max_size_gv <- 0
        }
    
        #saving the results (in an avarage way over n.run attemps)
        Sv <- Sv+ max_size_gv/n

        #saving the nodes of the vulnerable connected component
        vert <- vector()
        for (i in 1:length(vert_ids)){
            vert[i]<- vert_ids[i]
        }
      
        #calculating the exteded vulnerable nodes
        nodes.ex.vulnerable <- vert
        
        for (k in seq_along(nodes.neighbors[vert])){
            nodes.ex.vulnerable <- c(nodes.ex.vulnerable,nodes.neighbors[nodes.vulnerable][[k]] )
        }
        nodes.ex.vulnerable<- unique(nodes.ex.vulnerable)
        
        #computing the subgraph and size of extended vulnerable connected component
        if(length(nodes.ex.vulnerable)!=0){
            ge<-subgraph(g, nodes.ex.vulnerable)
            size_components_ge <-clusters(ge, mode="strong")$csize
            max_size_ge <- max(size_components_ge)
        }else {
            max_size_ge <- 0
        }
        
        #saving the nodes of the extended vulnerable connected component
        Se <-Se+ max_size_ge/n
    }
    
    #Saving the results
    Se.list<- c(Se.list, Se/n.run)
    Sv.list<- c(Sv.list, Sv/n.run)
    write.table(list(n, z, phi, n.run, Sv/n.run, Se/n.run),
            file = filename,
            append = TRUE,
            sep = ',', 
            row.names = FALSE,
            col.names = FALSE)
}

#Reading the results from the saved file
prob_pred <- read.csv('data/probability_prediction.csv')

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot()+ theme_light()+
    #annotate("text", x=1, y=1, size=10,label = TeX('$\\Phi=0.18$'))+
    geom_point(data=prob, aes(x=z, y=prob, col='Estimated probability'))+
    geom_line(data=prob_pred, aes(x=z, y=Se, col='Extended vulnerable cluster size'))+
    geom_line(data=prob_pred, aes(x=z, y=Sv, col='Vulnerable cluster size'))+
    xlab(label = TeX('Avarage degree $z$'))+
    scale_x_continuous(breaks = seq(1, 7, by = 1))+
    ylab(label = TeX('Probability'))+
    theme(text = element_text(size = 20), legend.position = c(0.45, 0.1))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c('red', 'green', 'blue'))+
    guides(color = guide_legend(title = TeX('$\\Phi$   = 0.18')))

#ggsave('figures/CC_probability.png')

#function that take in input the csv with the data and returns the data for the plot
geom_step_cascade <- function(filename){  
    df <- read.csv(filename)
    data <- df %>% group_by(threshold, z)%>% arrange(desc(cascade.fraction), .by_group=TRUE) %>% filter(row_number()==1)
    cascade <- data %>% filter(cascade.fraction>=0.1)
    no_cascade <- data %>% filter(cascade.fraction <0.1)
    upper.df <- cascade %>% group_by(threshold) %>% arrange(desc(z), .by_group=TRUE) %>% filter(row_number()==1) %>% select('z', 'threshold')
    lower.df <- cascade %>% group_by(threshold) %>% arrange(.by_group=TRUE) %>% filter(row_number()==1)%>% select('z', 'threshold') 
    colnames(lower.df) <- c('z_min', 'threshold')
    colnames(upper.df) <- c('z_max', 'threshold')
    final.df <- merge(x = upper.df, y = lower.df, by = "threshold", all.x = TRUE)
    return(final.df)
}

#Uniform threshold
filename <- 'data/ERG_simulation_data.csv'
cascade_unif <- geom_step_cascade(filename)

#Normal distributed threshold with std=0.1
filename <- 'data/ERG_simulation_sigma0.1.csv'
cascade_st2 <- geom_step_cascade(filename)

#Normal distributed threshold with std=0.05
filename <- 'data/ERG_simulation_sigma0.05.csv'
cascade_st1 <- geom_step_cascade(filename)

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot() +theme_light()+
    geom_step(data=cascade_st2, aes(x=threshold, y=z_max, col='sigma=0.1'))+
    geom_step(data=cascade_st1, aes(x=threshold, y=z_max, col='sigma=0.05'))+
    geom_step(data=cascade_unif, aes(x=threshold, y=z_max, col='Uniform'))+
    geom_step(data=cascade_st2, aes(x=threshold, y=z_min, col='sigma=0.1'))+
    geom_step(data=cascade_st1, aes(x=threshold, y=z_min, col='sigma=0.05'))+
    geom_step(data=cascade_unif, aes(x=threshold, y=z_min, col='Uniform'))+
    xlab(label = TeX('Threshold $\\Phi$'))+
    xlim(0.1, 0.30)+
    ylim(1, 16)+
    ylab(label = TeX('Avarage degree $z$'))+
    theme(text = element_text(size = 20), legend.position = c(0.15, 0.3))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c("red", "blue", 'black'))+
    guides(color = guide_legend(title = "Legend"))

#ggsave('figures/CC_ERG_threshold_comparison.png')

#file with barabasi albert simulation data
filename <- 'data/BA_simulation_data.csv'
BA<- geom_step_cascade(filename)

options(repr.plot.width = 10, repr.plot.height = 10)

ggplot() +theme_light()+
    geom_step(data=BA, aes(x=threshold, y=z_max, col='Barabasi Albert'))+
    geom_step(data=cascade_unif, aes(x=threshold, y=z_max, col='Uniform Random Graph'))+
    geom_step(data=BA, aes(x=threshold, y=z_min, col='Barabasi Albert'))+
    geom_step(data=cascade_unif, aes(x=threshold, y=z_min, col='Uniform Random Graph'))+
    xlab(label = TeX('Threshold $\\Phi$'))+
    xlim(0.1, 0.20)+
    ylim(1, 16)+
    ylab(label = TeX('Avarage degree $z$'))+
    theme(text = element_text(size = 20), legend.position = c(0.75, 0.9))+
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))+
    scale_colour_manual(values = c("red", "blue"))+
    guides(color = guide_legend(title = "Legend"))

#ggsave('figures/CC_BA_simulations.png')
