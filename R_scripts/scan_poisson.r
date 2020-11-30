#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("zoo")
#install.packages("sf")
#install.packages("maps")
#The package rnaturalearth also provides a map of countries of the entire world
#install.packages("rnaturalearth")
#install.packages('rnaturalearthdata')
#install.packages("rgeos")
#install.packages("gridExtra")
#install.packages("rgdal")
#install.packages("spdep")
library(rgeos)
library(sf)
library(zoo)#deal with dates
library(ggthemes)
library(tidyverse)
library(COVID19)
library(rnaturalearthdata)
library(rnaturalearth)
library(maps)
library(gridExtra)
library(rgdal)
library(spdep)
#install.packages("devtools")
library(devtools)
#devtools::install_github("benjak/scanstatistics", ref = "develop")
library(scanstatistics)
world <- ne_countries(scale='medium',returnclass = 'sf')
covidata<-covid19(start = '2020-02-29',end = '2020-02-29',raw = F )
covidata<-covid19(raw = F )
#install.packages("openxlsx")
library(openxlsx)
library(sf)

## it will guess the driver automatically based on the .shp extension
st_write(total, "my_total.shp")
run_app()
countries <- data.frame("areaid"=total$iso_a3,"date"=total$date,"population"=total$population,"cases"=total$cases)
write.csv(countries,"total.csv")
countries$administrative_area_level_1 <- gsub("United States", "United States of America", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Congo", "Republic of Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Republic of Congo, the Democratic Republic of the", "Democratic Republic of the Congo", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Cote d'Ivoire", "Ivory Coast", countries$administrative_area_level_1)
countries$administrative_area_level_1 <- gsub("Korea, South", "South Korea", countries$administrative_area_level_1)
  
names(countries)[names(countries) == "areaid"] <- "iso_a3"
  

total <- list(Covid=NULL)


total<-merge(world,countries,by="iso_a3"#,duplicateGeoms=T
             )

###################
#nearest neighbours
coor <- coordinates(total)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$subunit)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

#######################################################################
#devtools::install_github("Paula-Moraga/SpatialEpiApp")
library(SpatialEpiApp)
run_app()

##########################
#next try
#SIR

table <- read.csv("table.csv")

names(table)[names(table) == "ID.area"] <- "iso_a3"


total <- list(Covid=NULL)

total<-merge(world,table,by="iso_a3"#,duplicateGeoms=T
)

ggplot(data = total) +
  geom_sf(aes(fill = SIR)) +
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))


#######

world <- ne_countries(scale='medium',returnclass = 'sf')
covidata<-covid19(start = '2020-02-29',end = '2020-06-29',raw = F )

countries <- covidata

names(countries)[names(countries) == "iso_alpha_3"] <- "iso_a3"

total <- list(Covid=NULL)

total<-merge(world,countries,by="iso_a3"#,duplicateGeoms=T
)

st_write(total, "my_new_total.shp")
countries <- data.frame("areaid"=total$iso_a3,"date"=total$date,"population"=total$population,"cases"=total$confirmed)
write.csv(countries,"ntotal.csv")

run_app()

table2 <- read.csv("table2.csv")

names(table2)[names(table2) == "ID.area"] <- "iso_a3"


total <- list(Covid=NULL)

total<-merge(world,table2,by="iso_a3"#,duplicateGeoms=T
)
vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
ggplot(data = total) +
  geom_sf(aes(fill = SIR)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="de Abril a Junho",fill="SIR ",caption=c("Fonte: Covid19DataHub"))

