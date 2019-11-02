
#Load the package igraph (Network analysis and visualization)
library(igraph)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
airports <- read.csv("airports.csv")
routes = read.csv("routes.csv")

edges <- routes %>% select(Source.Airport, Destinaton.Airport)%>% group_by(Source.Airport, Destinaton.Airport) %>% summarize(n())
colnames(edges)[1] <- "from"
colnames(edges)[2] <- "to"
colnames(edges)[3] <- "weight"

###################################
# data preparation
###################################

# make sure nodes is unique
nodes <- airports %>% select(IATA, Latitude, Longitude)
nodes <-unique(nodes[,1:3])
nodes <- subset(nodes, nodes$IATA != '\\N')

# remove edges that do not have valid nodes
edges <- edges[(edges$from %in% nodes$IATA),]
edges <- edges[(edges$to %in% nodes$IATA),]

#remove node that is used in any edge
nodes <- nodes[((nodes$IATA %in% edges$to) | (nodes$IATA %in% edges$from)),]


## reduce number of edges and nodes
edges <- edges[1:30,]
nodes <- nodes[((nodes$IATA %in% edges$to) | (nodes$IATA %in% edges$from)),]



#convert data to igraph object
g1<- graph_from_data_frame(d = edges[, 1:2], vertices = nodes$IATA, directed = FALSE)

##edges
E(g1)

##Vetices
V(g1)

table(head_of(g1, E(g1)))
V(g1)$degree<-degree(g1)
gorder(g1) 

plot(g1,vertex.color="red3",vertex.size=8,edge.arrow.size=0.3,
     vertex.label.cex=0,vertex.label.dist=0.7)
plot (g1)


###########################################
#community Detection
##########################################

##########################################
# Clique detection
##########################################

# The functions find cliques, ie. complete subgraphs in a graph
# clique.number calculates the size of the largest clique(s).
clique.number(g1)

#cliques find all complete subgraphs in the input graph, 
#obeying the size limitations given in the min and max arguments.
cliques(g1, min=3)

# How about we want to find smaller cliques
cliques(g1, min=22)

#maximal.cliques finds all maximal cliques in the input graph.
#A clique in maximal if it cannot be extended to a larger clique. 
#The largest cliques (maximum cliques) are always maximal, but a maximal clique is not neccessarily the largest.
maximal.cliques(g1)

# Maximum cliques: largest.cliques finds all largest cliques in the input 
# graph. A clique is largest if there is no other clique including more vertices.
largest.cliques(g1)




####################################################
# walktrap method
#####################################################

wc <- walktrap.community(g1)

#modularity calculates the modularity of a graph with respect to the given membership vector.
#Clauset, A.; Newman, M. E. J. & Moore, C. Finding community structure in very large networks,
#Phyisical Review E 2004, 70, 066111
modularity(wc)

membership(wc)
plot(wc, g1)



############################################################
# edge.betweenness.community
# Community structure detection based on edge betweenness
###########################################################
# Many networks consist of modules which are densely connected themselves 
#but sparsely connected to other modules.
#M Newman and M Girvan: Finding and evaluating community structure in networks, Physical
#Review E 69, 026113 (2004)
eb <- edge.betweenness.community(g1)
eb





############################################################
# fastgreedy.community 
# Community structure via greedy optimization of modularity
###########################################################
g1 <- add.edges(g1, c(1,6, 1,11, 6, 11))
plot(g1)
fc <- fastgreedy.community(g1)
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
lec <- leading.eigenvector.community(g1)
lec
leading.eigenvector.community(g1, start=membership(lec))



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
g1 <- induced.subgraph(g1, subcomponent(g1, 1))
plot(g1)
spinglass.community(g1, spins=2)
spinglass.community(g1, vertex=1)



#infomap Method
imc <- infomap.community(g1)
membership(imc)
communities(imc)


#########################################
# http://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html
########################################

# let's generate two networks and merge them into one graph.
g2 <- barabasi.game(50, p=2, directed=F)
plot(g2)
g1 <- watts.strogatz.game(1, size=100, nei=5, p=0.05)
plot(g1)
g <- graph.union(g1,g2)
plot(g)

# let's remove multi-edges and loops
g <- simplify(g)

# let's see if we have communities here using the 
# Grivan-Newman algorithm
# 1st we calculate the edge betweenness, merges, etc...
ebc <- edge.betweenness.community(g, directed=F)
ebc

# Now we have the merges/splits and we need to calculate the modularity
# for each merge for this we'll use a function that for each edge
# removed will create a second graph, check for its membership and use
# that membership to calculate the modularity
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  # March 13, 2014 - compute modularity on the original graph g 
  # (Thank you to Augustin Luna for detecting this typo) and not on the induced one g2. 
  modularity(g,cl)
})

# we can now plot all modularities
plot(mods, pch=20)

# Now, let's color the nodes according to their membership
g2<-delete.edges(g, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
plot(g, vertex.label=NA)

# if we wanted to use the fastgreedy.community agorithm we would do
fc <- fastgreedy.community(g)
com<-community.to.membership(g, fc$merges, steps= which.max(fc$modularity)-1)
V(g)$color <- com$membership+1
g$layout <- layout.fruchterman.reingold
plot(g, vertex.label=NA)


###############################################################
# The following URL could be useful for community detection 
###############################################################
#http://igraph.wikidot.com/community-detection-in-r
#########################################
#https://sites.google.com/site/andrewjedelman/statistical-tools/network-analysis/community-detection
###########################################

#########################################
#http://smallstats.blogspot.sg/2012/12/community-detection-algorithm-with.html

