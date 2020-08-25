#https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html

#install.packages("tmap")
library(tmap)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("sf")
library(sf)
#install.packages("COVID19")
library(COVID19)
#install.packages("maps")
#library(maps)
#The package rnaturalearth also provides a map of countries of the entire world
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages('rnaturalearthdata')
library(rnaturalearthdata)
#install.packages("rgeos")
library(rgeos)

world <- ne_countries(scale='medium',returnclass = 'sf')

#world <- map_data('world')#maps package

data <- covid19(start = "2020-02-29",end="2020-02-29")

countries<-aggregate(confirmed~administrative_area_level_1,data,sum)
#that wasn't exactly necessary
head(countries)

#renaming countries
#countries$administrative_area_level_1 <- gsub("United Kingdom", "UK", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("United States", "United States of America", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Congo", "Republic of Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Republic of Congo, the Democratic Republic of the", "Democratic Republic of the Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Cote d'Ivoire", "Ivory Coast", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Korea, South", "South Korea", countries$administrative_area_level_1)

#Korea, South South Korea

names(countries)[names(countries) == "administrative_area_level_1"] <- "subunit"

total<-merge(world,countries,by="subunit")
total$subunit<-factor(total$subunit)
total <- total[order(total$confirmed),] # order the data [very important!]

###########################################
tm_shape(world) +
  tm_polygons('subunit') +
  tm_legend(show = FALSE)

tmap_mode("plot")
tmap_style("beaver")
?tmap_style

## tmap mode set to plotting
tm_shape(total) +
  tm_polygons('confirmed')+
  tm_borders("white", lwd = .5) +
  tm_text("name", size = "AREA")

#############

data <- covid19(start = "2020-02-29",end="2020-02-29")
data2 <- covid19(start = "2020-03-31",end="2020-03-31")

countries <- rbind(data,data2)

#renaming countries
#countries$administrative_area_level_1 <- gsub("United Kingdom", "UK", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("United States", "United States of America", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Congo", "Republic of Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Republic of Congo, the Democratic Republic of the", "Democratic Republic of the Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Cote d'Ivoire", "Ivory Coast", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Korea, South", "South Korea", countries$administrative_area_level_1)


names(countries)[names(countries) == "administrative_area_level_1"] <- "subunit"

total<-merge(world,countries,by="subunit")
total$subunit<-factor(total$subunit)
total <- total[order(total$confirmed),] # order the data [very important!]


#by doing this i can work with dates
tm_shape(total) +
  tm_polygons('confirmed')+
  tm_borders("white", lwd = .5) +
  tm_text("name", size = "AREA")+
  tm_facets(by='date')

#more advanced stuff
#install.packages("leaflet")
library(leaflet)
total2 <- total %>% filter(date=="2020-02-29")

#you can get base map and tiles from leaflet
tmap_mode("view")
tmap_style("natural")
?tm_basemap()


#testing leaflet features with bubble maps
tm_basemap(leaflet::providers$Stamen.Toner) +
  tm_shape(total2) + tm_bubbles(size = "confirmed")+#tm_polygons()+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}{r}.{ext}')#+
  #tm_text("name", size = "AREA")+
 # tm_facets(by='date')

  
#interactive plot
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(total2) + tm_polygons("confirmed")+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}')+
#tm_text("name", size = "AREA")+
 tm_facets(by='date')

#more advanced features
#here: https://mran.microsoft.com/snapshot/2017-01-20/web/packages/tmap/vignettes/tmap-nutshell.html

tmap_mode('plot')
?tm_shape()
#run the first part of the code again to run this
tm_shape(total,projection = "+proj=robin") +
  tm_polygons('confirmed')+
  tm_borders("white", lwd = .5) +
  tm_text("name", size = "AREA")

#more projections here: https://www.rdocumentation.org/packages/tmap/versions/0.7/topics/set_projection

tm_shape(total,projection ="+proj=eck4") +
  tm_polygons('confirmed')+
  tm_borders("white", lwd = .5) +
  tm_text("name", size = "AREA")

tm_shape(total,projection ="+proj=gall") +
  tm_polygons('confirmed')+
  tm_borders("white", lwd = .5) +
  tm_text("name", size = "AREA")

  
