
#Load the package igraph (Network analysis and visualization)
library(igraph)
library(dplyr)
#library (network)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
busStops <- read.csv("BusStops.csv")
routes = read.csv("Busroutes.csv")
highVolumeBusStop <- read.csv("High_volume_bus_stop.csv")


#edges <- routes %>% select(BusStopCode, NextBusStopCode, ServiceNo)
#weight is # of count per edges 
edges <- routes %>% select(BusStopCode, NextBusStopCode)%>% group_by(BusStopCode, NextBusStopCode) %>% summarize(n())

colnames(edges)[1] <- "from"
colnames(edges)[2] <- "to"
colnames(edges)[3] <- "weight"



###################################
# data preparation
###################################

# make sure nodes is unique
nodes <- busStops %>% select(BusStopCode, Latitude, Longitude)
nodes <-unique(nodes[,1:3])
nodes <- subset(nodes, nodes$BusStopCode != '')
nodes <- nodes[!(nodes$BusStopCode %in% highVolumeBusStop$PT_CODE),]


# remove edges that do not have valid nodes
edges <- edges[(edges$from %in% nodes$BusStopCode),]
edges <- edges[(edges$to %in% nodes$BusStopCode),]

edges <- edges[(edges$to %in% nodes$BusStopCode),]

#remove node that is not used in any edge
nodes <- nodes[((nodes$BusStopCode %in% edges$to) | (nodes$BusStopCode %in% edges$from)),]
  

## reduce number of edges and nodes
#edges <- edges[1:100,]
#nodes <- nodes[((nodes$BusStopCode %in% edges$to) | (nodes$BusStopCode %in% edges$from)),]



#convert data to igraph object
#routes_igraph <- graph_from_data_frame(d = edges[, 1:2], vertices = nodes$BusStopCode, directed = FALSE)
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed =FALSE)
plot (routes_igraph)

routes_igraph
##edges
E(routes_igraph )

##Vetices
V(routes_igraph )

table(head_of(routes_igraph , E(routes_igraph )))
V(routes_igraph )$degree<-degree(routes_igraph )
gorder(routes_igraph ) 

#plot(routes_igraph ,vertex.color="red3",vertex.size=6,edge.arrow.size=0.3,
#     vertex.label.cex=0,vertex.label.dist=0.3)
#dev.new(width=24, height=11)
#plot (routes_igraph )



###########################################
#community Detection
##########################################

##########################################
# Clique detection
##########################################

# The functions find cliques, ie. complete subgraphs in a graph
# clique.number calculates the size of the largest clique(s).
clique.number(routes_igraph )

#cliques find all complete subgraphs in the input graph, 
#obeying the size limitations given in the min and max arguments.
cliques(routes_igraph , min=4)

# How about we want to find smaller cliques
cliques(routes_igraph , min=3)

#maximal.cliques finds all maximal cliques in the input graph.
#A clique in maximal if it cannot be extended to a larger clique. 
#The largest cliques (maximum cliques) are always maximal, but a maximal clique is not neccessarily the largest.
maximal.cliques(routes_igraph )

# Maximum cliques: largest.cliques finds all largest cliques in the input 
# graph. A clique is largest if there is no other clique including more vertices.
largest.cliques(routes_igraph )



####################################################
# walktrap method
#####################################################

wc <- walktrap.community(routes_igraph )

#modularity calculates the modularity of a graph with respect to the given membership vector.
#Clauset, A.; Newman, M. E. J. & Moore, C. Finding community structure in very large networks,
#Phyisical Review E 2004, 70, 066111
modularity(wc)

membership(wc)
plot(wc, routes_igraph )



############################################################
# edge.betweenness.community
# Community structure detection based on edge betweenness
###########################################################
# Many networks consist of modules which are densely connected themselves 
#but sparsely connected to other modules.
#M Newman and M Girvan: Finding and evaluating community structure in networks, Physical
#Review E 69, 026113 (2004)
eb <- edge.betweenness.community(routes_igraph )
eb





############################################################
# fastgreedy.community 
# Community structure via greedy optimization of modularity
###########################################################
#routes_igraph  <- add.edges(routes_igraph , c(1,6, 1,11, 6, 11))
#plot(routes_igraph )
fc <- fastgreedy.community(routes_igraph )
membership(fc)
sizes(fc)

#karate <- graph.famous("Zachary")
#plot(karate)
fc
dendPlot(fc)

##########################################################
# leading.eigenvector.community
# Community structure detecting based on the leading eigenvector of the
# community matrix
##########################################################
lec <- leading.eigenvector.community(routes_igraph )
lec
leading.eigenvector.community(routes_igraph , start=membership(lec))



############################################################
# spinglass.community 
# Finding communities in graphs based on statistical meachanics
# This function tries to find communities in graphs via a 
# spin-glass model and simulated annealing.
############################################################
# J. Reichardt and S. Bornholdt: Statistical Mechanics of Community Detection, Phys. Rev. E, 74,
# 016110 (2006), http://arxiv.org/abs/cond-mat/0603718
# M. E. J. Newman and M. Girvan: Finding and evaluating community structure in networks, Phys.
# Rev. E 69, 026113 (2004)
# V.A. Traag and Jeroen Bruggeman: Community detection in networks with positive and negative
# links, http://arxiv.org/abs/0811.2329 (2008).
routes_igraph  <- induced.subgraph(routes_igraph , subcomponent(routes_igraph , 1))
plot(routes_igraph )
spinglass.community(routes_igraph , spins=2)
spinglass.community(routes_igraph , vertex=1)



#infomap Method
imc <- infomap.community(routes_igraph )
membership(imc)
communities(imc)

