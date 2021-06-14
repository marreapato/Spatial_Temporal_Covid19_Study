#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly",dependencies = TRUE)
#install.packages("tmap")
#install.packages("geobr")
#install.packages("spdep")
#install.packages("sf")
#install.packages("crul")
#install.packages("rgdal")
#install.packages('spDataLarge',
#           repos='https://nowosad.github.io/drat/', type='source')

#install.packages('rlang')

#install.packages("leaflet")
#install.packages("htmltools")
library(leaflet)
library(crul)
library(sf)
library(spdep)
library(geobr)
library(tmap)
library(tidyverse)
library(readxl)
library(rgdal)
library(maptools)
library(plotly)

data <- reactive({
    invalidateLater(4.32e+7/4,NULL)
    df_br = read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
}) 


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <-  renderPlotly({
        
        ###################################
        #analisarei a crime_rate
        df_br <- data()
        df_br <- df_br%>%group_by(date,state)%>%
            summarise_if(is.numeric,sum,na.rm=T)
        
        df_br <- df_br %>% filter(state!="TOTAL")
        ########################################
        
        df_br <- df_br[,c(1,2,6)]
        
        df_br <- df_br %>% filter(date==Sys.Date())
        
        #Mesorregioes#geo_br
        mesos <- read_state(year=2017,simplified = T)
        
        mesos <- mesos[,c(2,6)]
        
        #estados
        
        df_br <- as_tibble(df_br)
        
        mesos_sp <- left_join(mesos,df_br, by = c("abbrev_state" = "state"))
        
        colnames(mesos_sp)[3] <- "Casos"
        
        mp <- ggplot() +
                geom_sf(data=mesos_sp,aes(fill=Casos), size=.15) +
                labs(title="Mapa de Novos Casos de COVID-19 no Brasil", size=8,fill="Casos") +
                theme(legend.position = "right",axis.title.x=element_blank(),
                                        axis.text.x=element_blank(),
                                        axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank())
        
        
        
        
        
        ggplotly(mp)
        
    })
    
    
    output$distPlot2 <-  renderPlotly({
        
        ###################################
        #analisarei a crime_rate
        df_br <- data()
        df_br <- df_br%>%group_by(date,state)%>%
            summarise_if(is.numeric,sum,na.rm=T)
        
        df_br <- df_br %>% filter(state!="TOTAL")
        ########################################
        
        df_br <- df_br[,c(1,2,6)]
        
        df_br <- df_br %>% filter(date==Sys.Date()-1)
        
        #Mesorregioes#geo_br
        mesos <- read_state(year=2017,simplified = T)
        
        mesos <- mesos[,c(2,6)]
        
        #estados
        
        df_br <- as_tibble(df_br)
        
        mesos_sp <- left_join(mesos,df_br, by = c("abbrev_state" = "state"))
        
        colnames(mesos_sp)[3] <- "Casos"
        
        mp <- ggplot() +
            geom_sf(data=mesos_sp,aes(fill=Casos), size=.15) +
            labs(title="Mapa de Casos de COVID-19 no Brasil ontem", size=8,fill="Casos") +
            theme(legend.position = "right",axis.title.x=element_blank(),
                                    axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                    axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank())
        
        
        
        
        
        ggplotly(mp)
        
    })
    
    
})
