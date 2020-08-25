#later: https://edzer.github.io/UseR2016/
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
#https://www.youtube.com/watch?v=nlcZcWJiyX8&t=4s

#install.packages("tidyverse")
library(tidyverse)
#install.packages("sf")
library(sf)
#install.packages("COVID19")
library(COVID19)
#install.packages("maps")
library(maps)
#The package rnaturalearth also provides a map of countries of the entire world
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages('rnaturalearthdata')
library(rnaturalearthdata)
#install.packages("rgeos")
library(rgeos)

world <- ne_countries(scale='medium',returnclass = 'sf')

#world <- map_data('world')#maps package

data <- covid19(start = Sys.Date(),end=Sys.Date())

countries<-aggregate(confirmed~administrative_area_level_1,data,sum)
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

#plotting a normal map
(gworld <- ggplot(data = total) +
    geom_sf(aes(fill = subunit)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
     #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_viridis_d(option = "plasma") +
    theme(legend.position = "none",panel.background = element_rect(fill = "azure"),
          panel.border = element_rect(fill = NA)))

#plotting chlotopleth map
#plotting  map of confirmed cases

options(scipen=999)
(gworld2 <- ggplot(data = total) +
    geom_sf(aes(fill = confirmed)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_viridis_c(option = "plasma") +
    theme(panel.background = element_rect(fill = "azure"),
          panel.border = element_rect(fill = NA))+labs(title = "Cumulative number of confirmed cases from covid19",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#https://www.youtube.com/watch?v=nlcZcWJiyX8&t=4s <-more costumization tips

