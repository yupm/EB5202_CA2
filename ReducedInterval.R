
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
reduced_interval <- read.csv("GephiBusEdgesWithIntervals.csv")

reduced_interval <- reduced_interval[(reduced_interval$Intervals > 5 & reduced_interval$Intervals < 11),]



write.csv(reduced_interval, "GephiBusEdgesWithIntervals6-10.csv", row.names=FALSE)

