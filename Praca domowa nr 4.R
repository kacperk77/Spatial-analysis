remove(list=ls())
library(rgdal)
library(sp)
library(tidyverse)


#Pliki ze strony http://download.geofabrik.de/europe/poland.html zosta³y pobrane 
# a nastepnie wypakowane do oddzielnych folderow (kazde wojewodztwo ma swoj folder)

setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/mazowieckie")
 
  #mazowieckie
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")


  #Let's pick police
  mazowieckie <- mapa[mapa@data$fclass == "police",]
  mazowieckie@data <- mazowieckie@data %>% select(osm_id, code, fclass, name)
  


  
  #dolnoslaskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/dolnoslaskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  dolnoslaskie <- mapa[mapa@data$fclass == "police",]
  
  #kujawsko-pomorskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/kujawsko-pomorskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  kujawskopomorskie <- mapa[mapa@data$fclass == "police",]
  
  #lodzkie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/lodzkie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  lodzkie <- mapa[mapa@data$fclass == "police",]
  
  
  #lubelskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/lubelskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  lubelskie <- mapa[mapa@data$fclass == "police",]
  plot(lubelskie)
  
  #lubuskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/lubuskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  lubuskie <- mapa[mapa@data$fclass == "police",]
  
  
  #malopolskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/malopolskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  malopolskie <- mapa[mapa@data$fclass == "police",]
  
  #opolskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/opolskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  opolskie <- mapa[mapa@data$fclass == "police",]
  
  #podkarpackie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/podkarpackie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  podkarpackie <- mapa[mapa@data$fclass == "police",]
  
  #podlaskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/podlaskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  podlaskie <- mapa[mapa@data$fclass == "police",]
  
  #pomorskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/pomorskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  pomorskie <- mapa[mapa@data$fclass == "police",]
  
  
  #slaskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/slaskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  slaskie <- mapa[mapa@data$fclass == "police",]
  
  
  #swietokrzyskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/swietokrzyskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  swietokrzyskie <- mapa[mapa@data$fclass == "police",]
  
  
  #warminsko-mazurskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/warminsko-mazurskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  warminskomazurskie <- mapa[mapa@data$fclass == "police",]
  
  #wielkopolskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/wielkopolskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  wielkopolskie <- mapa[mapa@data$fclass == "police",]
  
  
  #zachodniopomorskie
  setwd("C:/Users/lenovo/Desktop/ekonometria przestrzenna/pd4/zachodniopomorskie")
  mapa <- readOGR(".", "gis_osm_pois_a_free_1")
  mapa <- spTransform(mapa, "+proj=longlat")
  
  
  #Let's pick police
  zachodniopomorskie <- mapa[mapa@data$fclass == "police",]
  
  
  
all <- rbind(dolnoslaskie, kujawskopomorskie, lodzkie, lubelskie, lubuskie, malopolskie, mazowieckie, opolskie,
              podkarpackie, podlaskie, pomorskie, slaskie, swietokrzyskie, warminskomazurskie, wielkopolskie,
              zachodniopomorskie)
  

remove(dolnoslaskie, kujawskopomorskie, lodzkie, lubelskie, lubuskie, malopolskie, mazowieckie, opolskie,
         podkarpackie, podlaskie, pomorskie, slaskie, swietokrzyskie, warminskomazurskie, wielkopolskie,
         zachodniopomorskie)


setwd("C:\\Users\\lenovo\\Desktop\\ekonometria przestrzenna")




mapa <- readOGR(".", "NUTS_RG_01M_2013")
mapa <- spTransform(mapa, "+proj=longlat")

mapa@data$NUTS_ID_char <- as.character(mapa@data$NUTS_ID)
mapa@data$country <- substr(mapa@data$NUTS_ID_char, 1, 2) 
mapa <- mapa[mapa@data$country == "PL", ]

mapa_NUTS3 <- mapa[mapa@data$STAT_LEVL_ == 3, ]

  

plot(mapa_NUTS3)
plot(all, add = TRUE, col = "blue", lwd = 4)
  
  
  
data_for_sum_of_points <- over(all, mapa_NUTS3)
  


    
sumof_points <- data_for_sum_of_points %>%
  group_by(NUTS_ID) %>%
  summarise(n_of_police=n())
  
  
  





#Wczytanie danych z poprzedniej pracy domowej

setwd("C:\\Users\\lenovo\\Desktop\\ekonometria przestrzenna")


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


colnames(spatial_data2@data)[7] <- 'robbery'


remove(spatial_data3)



#polaczenie danych z poprzedniej pracy domowej i obecnej
spatial_data3 <- merge(y = sumof_points, x =spatial_data2 , by.y = "NUTS_ID", by.x = "NUTS_ID")

spatial_data3$n_of_police_per_100k <- spatial_data3$n_of_police/(spatial_data3$population/100000)


#wizualizacja nowych danych
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
pal <- colorRampPalette(c("red", "white", green_area), bias = 1)

spplot(spatial_data3, zcol = "n_of_police_per_100k", colorkey = TRUE, col.regions = pal(100), cuts = 99,
       par.settings = list(axis.line = list(col =  'transparent')),
       main = "Number of police stations per 100k people")
