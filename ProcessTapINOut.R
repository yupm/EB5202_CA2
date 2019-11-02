#Load the package igraph (Network analysis and visualization)
#library(igraph)
#library(dplyr)
#library (network)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
busStops_passengert <- read.csv("transport_node_bus_201909.csv")

busStops_passengert <- busStops_passengert[(busStops_passengert$TOTAL_TAP_IN_VOLUME > 100 | busStops_passengert$TOTAL_TAP_OUT_VOLUME > 100),]
busStops_passengert <- busStops_passengert[(busStops_passengert$DAY_TYPE == 'WEEKDAY'),]

write.csv(busStops_passengert, "High_volume_bus_stop.csv")

unique(busStops_passengert$PT_CODE)
