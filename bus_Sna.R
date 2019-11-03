#Load the package SNA (Social Network Analysis Library)
#Load the package igraph (Network analysis and visualization)
library(sna)
library(igraph)
library(tidyverse)
library("ggplot2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################################

# define function to plot a graph
plotG <- function(g) {
  plot(g, 
       # force-directed layout
       #layout=layout.fruchterman.reingold,
       layout=layout_with_fr,
       vertex.label.font=2, 
       vertex.size=4, 
       vertex.label.dist=0.5,
       vertex.color="blue",
       vertex.frame.color=FALSE,
       edge.arrow.size=0.1,
       arrow.width=0.1,
       edge.color="red")
}

# Alternatively, you could read in the data from a similar CSV file as follows:
df_br <- read.csv("BusRoutes2.csv")

#rename BusStopCode to from and NextBusStopCode to 'to' for plotting.
df_br <- df_br %>% 
  rename(
    from = BusStopCode,
    to = NextBusStopCode
  )


#drop unused columns
df_br <- subset(df_br, select = -c(SAT_FirstBus, SAT_LastBus,SAT_FirstBus, SUN_FirstBus, SUN_LastBus, WD_FirstBus, WD_LastBus))
df_br


#read in bus stop volume data 
dfnv <- read.csv("transport_node_bus_201909.csv")


#get only 9am weekday bus passenger data
df8nv <- dfnv[(dfnv$TIME_PER_HOUR== 8 & dfnv$DAY_TYPE =="WEEKDAY"),]
df8nv


# add weights to the busstop via passenger volume (merge)
df <- merge(df_br, df8nv, by.x="from", by.y="PT_CODE")
df

# change TOTAL_TAP_IN_VOLUME to weight
#names(df)[names(df8nv) == 'TOTAL_TAP_IN_VOLUME'] <- 'weight'
#df


#read in bus stop data
df_bs <- read.csv("BusStops.csv")


#drop unused columns
df_bs <- subset(df_bs, select = -c(RoadName))
df_bs


# merge in bus stops
df <- merge(df, df_bs, by.x="from", by.y="BusStopCode")
df


gr <- graph.data.frame(df, directed=TRUE)


# too much nodes
plot(gr, edge.width=E(gr)$TOTAL_TAP_IN_VOLUME)
plot(gr, edge.width=E(gr)$TOTAL_TAP_IN_VOLUME, vertex.label=gr$Description)


#remove nodes and replot 
df_trim <- df[(df$TOTAL_TAP_IN_VOLUME > 15000),]
df_trim
gr_trim <- graph.data.frame(df_trim, directed=TRUE)
plot(gr_trim, edge.width=E(gr_trim)$TOTAL_TAP_IN_VOLUME, vertex.label=gr_trim$Description,
     vertex.label.color=V(gr_trim)$color, vertex.label.font=2.5,vertex.label.cex=.6,)



gr_d <- degree(gr, mode="all")
gr_ddc <- degree.distribution(gr, cumulative=T, mode="all")
plot( x=0:max(gr_d), y=1-gr_ddc, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency")

gr_dd <- degree.distribution(gr, mode="all")
plot( x=0:max(gr_d), y=1-gr_dd, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency")


hist(gr_d, main="Histogram of node degree")



gr_d.histogram <- as.data.frame(table(gr_d))

gr_d.histogram[,1] <- as.numeric(gr_d.histogram[,1])


# Now, plot it!
ggplot(gr_d.histogram, aes(x =gr_d, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()





connectedness(gr)

page.rank(gr)$weight

hub.score(gr)$vector
authority.score(gr)$vector

