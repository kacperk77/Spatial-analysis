#Autor - Kacper Kalinowski 76975

rm(list = ls())
#setwd("C:\\Users\\lenovo\\Desktop\\ekonometria przestrzenna")

library(rgdal)
library(sp)
library(tidyverse)



mapa <- readOGR(".", "NUTS_RG_01M_2013")
mapa <- spTransform(mapa, "+proj=longlat")

#select Poland
mapa@data$NUTS_ID_char <- as.character(mapa@data$NUTS_ID)
mapa@data$country <- substr(mapa@data$NUTS_ID_char, 1, 2) 
mapa <- mapa[mapa@data$country == "PL", ]

mapa_NUTS3 <- mapa[mapa@data$STAT_LEVL_ == 3, ]


#Wczytanie danych dot. rozbojow
dane <- read.table('crim_gen_reg.tsv', sep = '\t', header = TRUE, na.strings =   ":")

#Przygotowanie danych
dane <-  dane %>% select(unit.iccs.geo.time, X2010)

dane <- dane %>%
  separate(unit.iccs.geo.time, c("unit", "iccs", 'geo'), ",")

dane <- dane %>% filter(substr(geo, 1, 2) == 'PL', iccs =="ICCS0401")

dane$X2010 <- as.numeric(dane$X2010)


#Merge danych dot. rozbojow oraz mapy
spatial_data <- merge(y = dane, x = mapa_NUTS3, by.y = "geo", by.x = "NUTS_ID")


#Stworzenie mapy
black_area <- rgb(0,0,1, max=255)
pal <- colorRampPalette(c("white", black_area), bias = 1)
spplot(spatial_data, zcol = "X2010", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Robbery")
