#https://rpubs.com/lokigao/maps
library(tidyverse)
library(COVID19)
library(maps)
world <- map_data('world')
data <- covid19(start ='2020-08-20',end='2020-08-20')

countries<-aggregate(confirmed~administrative_area_level_1,data,sum)
head(countries)

#renaming countries
countries$administrative_area_level_1 <- gsub("United Kingdom", "UK", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("United States", "USA", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Congo", "Republic of Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Republic of Congo, the Democratic Republic of the", "Democratic Republic of the Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Cote d'Ivoire", "Ivory Coast", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Korea, South", "South Korea", countries$administrative_area_level_1)

#Korea, South South Korea





names(countries)[names(countries) == "administrative_area_level_1"] <- "region"

total<-merge(world,countries,by="region")
total$region<-factor(total$region)
total <- total[order(total$order),] # order the data [very important!]

options(scipen=999)

p<-ggplot(total, aes(long, lat, group=group, fill=confirmed)) +
  geom_polygon(color="grey")
p

#naming countries

region.lab.data <- total %>%
  group_by(region) %>%
  summarise(long = quantile(long,0.70), lat = quantile(lat,0.325),confirmed=confirmed,group=group)

z=ggplot(total, aes(long, lat, group=group, fill=confirmed))+
  geom_polygon(color='grey')

z=z+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  theme(legend.position = "none")
z

#playing with colors and themes
#install.packages("ggthemes")
library(ggthemes)

p<-ggplot(total, aes(long, lat, group=group, fill=confirmed)) +
  geom_polygon(color="grey")+theme_map()+scale_fill_continuous_tableau()
p


