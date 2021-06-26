#Autor - Kacper Kalinowski 76975

rm(list = ls())
setwd("C:\\Users\\lenovo\\Desktop\\ekonometria przestrzenna")

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
dane <- read.table('estat_crim_gen_reg$defaultview_filtered.tsv', sep = '\t', header = TRUE, na.strings =   ":")



#Przygotowanie danych
dane <-  dane %>% select(unit.iccs.geo.time, X2010)

dane <- dane %>%
  separate(freq.unit.iccs.geo.TIME_PERIOD, c('freq', "unit", "iccs", 'geo', 'TIME_PERIOD'), ",")

dane <- dane %>% filter(substr(geo, 1, 2) == 'PL', iccs =="ICCS0401" & nchar(dane$geo) == 5)

dane <- dane %>% filter(nchar(dane$geo) >= 5)

dane$X2010 <- as.numeric(dane$X2010)

colnames(dane)[4] <- 'NUTS3_2008'

#slownik 2008-2013
slownik_2008_2013 <- read.csv("slownik_nuts3_2008_2013.csv", sep = ';')

dane <- dane %>% left_join(slownik_2008_2013)


#rozdzielnie regionow 
ciechanowski <- dane[6,8]/2
plocki <- dane[6,8]/2

siedlecki <- dane[7,8]/2
ostrolecki <- dane[7,8]/2

#Merge danych dot. rozbojow oraz mapy
spatial_data <- merge(y = dane, x = mapa_NUTS3, by.y = "NUTS3_2013", by.x = "NUTS_ID")

spatial_data <- spatial_data[,c(1,2,3,4,5,6,14)]

spatial_data@data[10,7] <- ciechanowski
spatial_data[13,7] <- plocki


spatial_data[14,7] <- ostrolecki
spatial_data[19,7] <- siedlecki


spatial_data <- spatial_data[!is.na(spatial_data@data$X2010),]


library(spdep)
library(geosphere)



#Macierz nr 1 - sasiedztwo pierwszego rzedu z normalizacja minmax (normalizacja skalarem bedacym najwyzsza wartoscia wlasna macierzy)

cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "minmax")
W1 <- listw2mat(W1_list)
W1_list$weights


#Macierz nr 2 - opart na odwróconych kwadratach odleg³oœci

distance <- distm(coordinates(spatial_data), fun = distCosine) / 1000
rownames(distance) <- spatial_data@data$jpt_kod_je
colnames(distance) <- spatial_data@data$jpt_kod_je
#wprowadzamy prog 200 km 
distance[distance > 200] = 0

gamma <- 2
W2 <- 1 / (distance ^ gamma)
W2[is.infinite(W2)] = 0
W2_list <- mat2listw(W2, style="W")



#Macierz nr 3
# 1 zmienna przygotowanie danych

#populacja
dane_populacja <- read.table('estat_demo_r_pjangrp3_filtered.tsv', sep = '\t', header = TRUE, na.strings =   ":")

dane_populacja$X2016 <- as.numeric(as.character(dane_populacja$X2016))

dane_populacja <- dane_populacja %>%
  separate(freq.sex.unit.age.geo.TIME_PERIOD, c("freq", 'sex', "unit", 'age', 'geo', 'TIME_PERIOD'), ",") %>% 
  filter(sex == 'T' & age == 'TOTAL')

dane_populacja <- dane_populacja %>% select(geo, X2016)

dane_populacja <- dane_populacja %>% filter(substr(geo, 1, 2) == 'PL')

colnames(dane_populacja) <- c('geo', 'population')


#gdp
dane_gdp <- read.table('estat_nama_10r_3gdp$defaultview_filtered.tsv', sep = '\t', header = TRUE, na.strings =   ":")

dane_gdp <- dane_gdp %>%
  separate(freq.unit.geo.TIME_PERIOD, c("freq", "unit", 'geo', 'TIME_PERIOD'), ",") 
  
dane_gdp$X2016 <- as.numeric(as.character(dane_gdp$X2016))

dane_gdp <- dane_gdp %>% select(geo, X2016)
colnames(dane_gdp) <- c('geo', 'gdp_mln_euro')

dane_gdp$gdp <- dane_gdp$gdp_mln_euro*1000000

dane_gdp <- dane_gdp %>% filter(substr(geo, 1, 2) == 'PL')


join1 <- dane_gdp %>% left_join(dane_populacja)
join1$gdp_per_capita <- (join1$gdp/join1$population)/1000

#join1 gdp z population - > gdp_per_capita

join1 <- join1 %>% select(geo, gdp_per_capita)

#density population

dane_density <- read.table('estat_demo_r_d3dens$defaultview_filtered.tsv', sep = '\t', header = TRUE, na.strings =   ": z")

dane_density <- dane_density %>%
  separate(freq.unit.geo.TIME_PERIOD, c("freq",  "unit", 'geo', 'TIME_PERIOD'), ",") 

dane_density <- dane_density %>% select(geo, X2015)

colnames(dane_density) <- c('geo', 'pop_density')

dane_density <- dane_density %>% filter(substr(geo, 1, 2) == 'PL')

dane_density$pop_density <- as.numeric(as.character(dane_density$pop_density))

#join2 gdp_per_capita z density population

join2 <- join1 %>% left_join(dane_density)



#liczba zatrudnionych

dane_zatrudnieni <- read.table('estat_nama_10r_3empers$defaultview_filtered.tsv', sep = '\t', header = TRUE, na.strings =   ": z")

dane_zatrudnieni <- dane_zatrudnieni %>%
  separate(freq.unit.wstatus.nace_r2.geo.TIME_PERIOD, c("freq", 'unit', "wstatus", 'nace_r2', 'geo', 'TIME_PERIOD'), ",") 

dane_zatrudnieni <- dane_zatrudnieni %>% select(geo, X2016)

dane_zatrudnieni <- dane_zatrudnieni %>% filter(substr(geo, 1, 2) == 'PL')

dane_zatrudnieni$employed <- substr(dane_zatrudnieni$X2016, 1, nchar(as.character(dane_zatrudnieni$X2016)) - 1)

dane_zatrudnieni$employed <- as.numeric(dane_zatrudnieni$employed)

dane_zatrudnieni$employed <- dane_zatrudnieni$employed * 1000

dane_zatrudnieni <- dane_zatrudnieni %>% select(geo, employed)

#join3 zatrudnieni z population - > udzial zatrudnionych w populacji

join3 <- dane_zatrudnieni %>% left_join(dane_populacja)

join3$emp_share <- round(join3$employed/join3$population,3)

join3 <- join3 %>% select(geo, emp_share, population)

#join4 emp_share (join3) z gdp_per_capita i density population (join2) -> final data

#gdp, populacja dane z 2016, density z 2015, liczba zatrudnionych z 2015 

final_data <- join2 %>% left_join(join3)

final_data <- final_data %>% filter(nchar(geo) >= 5)

slownik <- read.csv('slownik_nuts3.csv', sep = ';')

colnames(slownik) <- c('region', 'geo', 'NUTS_2013')


final_data_NUTS3 <- final_data %>% left_join(slownik)




spatial_data2 <- merge(y = final_data_NUTS3, x = spatial_data, by.y = "NUTS_2013", by.x = "NUTS_ID")



N <- nrow(spatial_data2)

distance2 <- matrix(0, nrow = N, ncol = N)
for(ii in 1:N) {
  for(jj in 1:N) {
    distance2[ii, jj] <- ((spatial_data2@data$gdp_per_capita[ii] - spatial_data2@data$gdp_per_capita[jj])^2 +
                            (spatial_data2@data$pop_density[ii] - spatial_data2@data$pop_density[jj])^2 +
                            (spatial_data2@data$emp_share[ii] - spatial_data2@data$emp_share[jj]))^(1/2)
  }
}
gamma <- 1
W3 <- 1 / (distance2 ^ gamma)
W3[distance2==0] <- 1/min(distance2[distance2>0]) 
diag(W3) <- 0
W3_list <- mat2listw(W3, style = "W")




colnames(spatial_data2@data)[7] <- 'robbery'



#Testy - do opisu wszystko

model.liniowy <- lm(spatial_data2$robbery ~ spatial_data2$pop_density)
summary(model.liniowy)

#GRAPHICAL EVALUATION
spatial_data2$res <- model.liniowy$residuals
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
pal <- colorRampPalette(c("red", "white", green_area), bias = 1.2)
#Put 3 colors into the map, "white" in the middle of the palette, play around with the bias other than 1 so as to plot the near-zero residuals as white
spplot(spatial_data2, zcol = "res", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Robbery")




#GLOBAL MORAN'S TEST FOR RESIDUALS
cont1 <- poly2nb(spatial_data2, queen = T)
lm.morantest(model.liniowy, W2_list, alternative = "greater")
res <- model.liniowy$residuals
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
moran.plot(res, W2_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

moran.test(res, W2_list, alternative = 'greater') #Test statistic with p-value

#Local Moran's tests
localmoran(res, W2_list, p.adjust.method = "bonferroni")


#Geary's C test
geary.test(res, W2_list)


#Join count tests - do dokonczenia


joincount.test(as.factor(res > 0), listw = W2_list)

joincount.test(as.factor(res > 100), listw = W2_list)

joincount.test(as.factor(res > 200), listw = W2_list)

joincount.test(as.factor(res > -100), listw = W2_list)

