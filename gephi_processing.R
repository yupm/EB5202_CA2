

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df_br <- read.csv("BusRoutesSimple.csv")
df_br


#read in bus stop volume data 
dfnv <- read.csv("transport_node_bus_201909.csv")
dfnv


#get only 9am weekday bus passenger data
dfnvwd <- dfnv[(dfnv$DAY_TYPE =="WEEKDAY"),]
dfnvwd

dfnvwd$Weight <- dfnvwd$TOTAL_TAP_IN_VOLUME + dfnvwd$TOTAL_TAP_OUT_VOLUME 
dfnvwd

#drop YEAR_MONTH DAY_TYPE PT_TYPE
dfnvwd <- subset(dfnvwd, select = -c(YEAR_MONTH, DAY_TYPE, PT_TYPE))
dfnvwd


# add weights to the busstop via passenger volume (merge)
df <- merge(df_br, df8nv, by.x="from", by.y="PT_CODE")
df