##################################################################################
# Preprocesamiento datos de trajectoria UBER
#Input: datos brutos de trayectoria de Uber. Datos del momento y la localización
#       de el evento de recogida de Uber, en la ciudad de Nueva York en los meses  de 
#       julio a septiembre de 2014.(3 trimestre)


# # Libraries

library(ggmap)# Para pintar mapas bajo capas de google
library(dplyr)# Manejo de bases de datos
library(VIM) # para identificar missing
library(lubridate)

# # output

output <- "../output"

# # Lectura de datos

jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")

data14 <- bind_rows(jul14, aug14, sep14)

rm(jul14)
rm(aug14)
rm(sep14)

gc()

Rprofmem()

# # crear nuevas variables

#pasajeros
data14["Passengers"] <- round(runif(dim(data14)[1], min=1, max=4))
#Conductor ID
data14["driver"] <- round(runif(dim(data14)[1], min=1, max=5))

# # Ver datos ausentes

aggr(data14)


# # Ajustar formato de fecha

data14["Date.Time"] <- mdy_hms(data14[,"Date.Time"])
data14["Year"] <- factor(year(data14[,"Date.Time"]))
data14["Month"] <- factor(month(data14[,"Date.Time"]))
data14["Day"] <- factor(day(data14[,"Date.Time"]))
data14["Weekday"] <- factor(wday(data14[,"Date.Time"]))
data14["Hour"] <- factor(hour(data14[,"Date.Time"]))
data14["Minute"] <- factor(minute(data14[,"Date.Time"]))
data14["Second"] <- factor(second(data14[,"Date.Time"]))



# # Data Mining

# Clustering 30 clusters

set.seed(20)
clusters <- kmeans(data14[,2:3], 20)


# Save the cluster number in the dataset 
data14["Cluster"] <- as.factor(clusters$cluster)


# # Agregación tiempo<(dias, meses) espacio(clusters)

dataResum <- group_by(data14,Base, Cluster,Month, Day) %>% 
  summarise(meanPas = round(mean(Passengers)),
            countPick = n(),
            sumPass =sum(Passengers))

dataResum <- data.frame(dataResum)

Centers <- data.frame(clusters$centers)
Centers["Cluster"] <- rownames(Centers)

# Merge centers to clusters
dataResum <- left_join(dataResum,Centers, by="Cluster")

## De 2653532 de registros  a 9078


 # # guardar datos agregados
setwd(output)
write.table(dataResum,"aggregate_uber_trip_data.txt", quote=F,col.names = T, sep="\t")

getwd()



