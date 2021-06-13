#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
#install.packages("geobr")
#install.packages("spdep")
#install.packages("sf")
#install.packages("crul")
#install.packages("rgdal")
#install.packages('spDataLarge',
#           repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)
library(crul)
library(sf)
library(spdep)
library(geobr)
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
#geobr https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html

#geobr
datasets <- list_geobr()

print(datasets, n=21)
###################################
#analisarei a crime_rate
br <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
df_br <- br%>%group_by(date,state)%>%
  summarise_if(is.numeric,sum,na.rm=T)

df_br <- df_br %>% filter(state!="TOTAL")
########################################

df_br <- df_br[,c(1,2,6)]

df_br <- df_br %>% filter(date==Sys.Date())

#Mesorregioes#geo_br
mesos <- read_state(year=2017,simplified = T)
mesos$name_state

ggplot() +
  geom_sf(data=mesos,aes(fill=mesos$abbrev_state), size=.15) +
  labs(subtitle="Mapa do Brasil", size=8,fill="Regiões") +
  theme_minimal() +theme(legend.position = "None")

mesos <- mesos[,c(2,6)]

#estados

df_br <- as_tibble(df_br)

?left_join

mesos_sp <- left_join(mesos,df_br, by = c("abbrev_state" = "state"))

mesos_sp$abbrev_state

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=mesos_sp$newCases), size=.15) +
  labs(subtitle="Mapa do Brasil", size=8,fill="Regiões") +
  theme_minimal() +theme(legend.position = "None")


