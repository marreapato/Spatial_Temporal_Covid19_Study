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

world <- ne_countries(scale='medium',returnclass = 'sf')
#obs: to check the number of hospitalized patients i'll have aggregate and sum

#saving datasets
feb <-covid19(start ="2020-02-29" ,end ="2020-02-29" ) 
march <- covid19(start ="2020-03-31" ,end ="2020-03-31" ) 
april <-covid19(start ="2020-04-30" ,end ="2020-04-30" )  
may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ) 
june <- covid19(start ="2020-06-30" ,end ="2020-06-30" ) 
july <- covid19(start ="2020-07-31" ,end ="2020-07-31" ) 

#list of datasets
datasets <- list(feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july)

#renaming a few cells in the datasets

for(i in 1:length(datasets)){
 
  datasets[[i]]$school_closing <- gsub(0, "No measures", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(1, "Recommend closing", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(2, "Require closing (only some levels or categories)", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(3, "Require closing all levels", datasets[[i]]$school_closing)
  
  datasets[[i]]$workplace_closing <- gsub(0, "No measures", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(1, "Recommend closing (or work from home)", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(2, "Require closing for some sectors or categories of workers", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(3, "require closing (or work from home) all-but-essential workplaces", datasets[[i]]$workplace_closing)
   
  datasets[[i]]$cancel_events<- gsub(0, "No measures", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(1, "Recommend cancelling", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(2, "Require cancelling", datasets[[i]]$cancel_events)
  
  datasets[[i]]$gatherings_restrictions<- gsub(0, "No measures", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(1, "Restrictions on very large gatherings", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(2, "Restrictions on gatherings between 100-1000 people", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(3, "Restrictions on gatherings between 10-100 people", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(4, "Restrictions on gatherings of less than 10 people", datasets[[i]]$gatherings_restrictions)
 
  datasets[[i]]$transport_closing<- gsub(0, "No measures", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(1, "Recommend closing", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(2, "Require closing", datasets[[i]]$transport_closing)
  
  datasets[[i]]$stay_home_restrictions<- gsub(0, "No measures", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(1, "recommend not leaving house", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(2, "require not leaving house with exceptions for daily exercise,\n grocery shopping, and “essential” trips", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(3, "Require not leaving house with minimal exceptions", datasets[[i]]$stay_home_restrictions)
  
  datasets[[i]]$internal_movement_restrictions<- gsub(0, "No measures", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(1, "Recommend closing (or significantly reduce volume/route/means of transport)", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(2, "Require closing (or prohibit most people from using it)", datasets[[i]]$internal_movement_restrictions)
 
  datasets[[i]]$international_movement_restrictions<- gsub(0, "No measures", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(1, "Screening", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(2, "Quarantine arrivals from high-risk regions", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(3, "Ban on high-risk regions", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(4, "Total border closure", datasets[[i]]$international_movement_restrictions)
  
  
  datasets[[i]]$information_campaigns<- gsub(0, "No public information campaign", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(1, "public officials urging caution about COVID-19", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(2, "coordinated public information campaign", datasets[[i]]$information_campaigns)
  
  datasets[[i]]$testing_policy<- gsub(0, "No testing policy", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(1, "Only those who both (a) have symptoms AND (b) meet specific criteria", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(2, "testing of anyone showing COVID-19 symptoms", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(3, "open public testing", datasets[[i]]$testing_policy)
  
  datasets[[i]]$contact_tracing<- gsub(0, "No testing policy", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(1, "Limited contact tracing, not done for all cases", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(2, "Comprehensive contact tracing, done for all cases", datasets[[i]]$contact_tracing)
  
  
}

#list of countries to rename in each dataset
countries <- list(countriesf=datasets$feb20,countriesm=datasets$march20,countriesa=datasets$april20,countriesmay=datasets$may20,
                  countriesjun=datasets$june20,countriesjul=datasets$july)
#renaming a few countries

for(i in 1:length(countries)){
  countries[[i]]$administrative_area_level_1 <- gsub("United States", "United States of America", countries[[i]]$administrative_area_level_1)
  countries[[i]]$administrative_area_level_1 <- gsub("Congo", "Republic of Congo", countries[[i]]$administrative_area_level_1)
  countries[[i]]$administrative_area_level_1 <- gsub("Republic of Congo, the Democratic Republic of the", "Democratic Republic of the Congo", countries[[i]]$administrative_area_level_1)
  countries[[i]]$administrative_area_level_1 <- gsub("Cote d'Ivoire", "Ivory Coast", countries[[i]]$administrative_area_level_1)
  countries[[i]]$administrative_area_level_1 <- gsub("Korea, South", "South Korea", countries[[i]]$administrative_area_level_1)
  
  names(countries[[i]])[names(countries[[i]]) == "administrative_area_level_1"] <- "subunit"
  
}

#full datasets of sf object

total <- list(totalf=NULL,totalm=NULL,totala=NULL,totalma=NULL,totaljun=NULL,totaljul=NULL)

for(i in 1:length(total)){
  total[[i]]<-merge(world,countries[[i]],by="subunit")
  total[[i]]$subunit<-factor(total[[i]]$subunit)
  total[[i]] <- total[[i]][order(total[[i]]$confirmed),] # order the data [very important!]
  
}

options(scipen=999)

#continuous information feb###########################

maps_plot <- list(feb=NULL,marc=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL)
#i have 16 plots

name_mont <- c("February.","March.","April.","May.","June.","July.")

for(i in 1:length(total)){
#confirmed number of cases 
(maps_plot[[i]][["Confirmed_cases"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = confirmed)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_continuous_tableau() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Cumulative number of confirmed cases of covid19 in",name_mont[i],sep=" "),fill="Confirmed cases.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#confirmed number of deaths 
(maps_plot[[i]][["Deaths"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = deaths)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_gradient_tableau(palette = "Red-Gold") +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title =paste("Cumulative number of deaths from covid19 in",name_mont[i],sep=" "),fill="Number of deaths.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#confirmed number of recovered cases in february
(maps_plot[[i]][["Recovered_cases"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = recovered)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_gradient_tableau(palette = "Green-Gold") +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Cumulative number of recovered patients of covid19 in",name_mont[i],sep = " "),fill="Recovered cases.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#cumulative number of tests in february
(maps_plot[[i]][["N_of_tests"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = tests)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_gradient_tableau("Orange-Gold") +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Cumulative number of tests of covid19 in",name_mont[i],sep=' '),fill="Number of tests.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#population in february
(maps_plot[[i]][["Population"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = population)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_gradient_tableau(palette="Blue-Teal") +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("World population in",name_mont[i],sep = ' '),fill="Population.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#descriptive information feb#######################


#school closures
(maps_plot[[i]][["School_closures"]] <- ggplot(data = total[[i]]) +
   geom_sf(aes(fill = school_closing)) +
   #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
   #         fill = NA, colour = "black", size = 1.5) +
   scale_fill_economist() +
   theme(panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title = paste("School closures in",name_mont[i],sep=' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#workspace closures
(maps_plot[[i]][["Workspace_closures"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = workplace_closing)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Workspace closures in",name_mont[i],sep=' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))


#canceled events
(maps_plot[[i]][["Canceled_events"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = cancel_events)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Canceled events in",name_mont[i],sep=' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#gatherings restrictions

(maps_plot[[i]][["Gathering_restr"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Gathering restrictions in",name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))


#transport closures
(maps_plot[[i]][["Transport_closures"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = transport_closing)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Transport closures in",name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#stay home restrictions

(maps_plot[[i]][["Stay_home"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Stay home restrictions in", name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#internal_movement_restrictions	

(maps_plot[[i]][["Internal_restric"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Internal movement restrictions in",name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#international_movement_restrictions		

(maps_plot[[i]][["International_restric"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Internal movement restrictions in",name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))


#information_campaigns			

(maps_plot[[i]][["Info_campaigns"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = information_campaigns)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title = paste("Information campaigns in",name_mont[i],sep = ' '),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#testing_policy				

(maps_plot[[i]][["Testing_policy"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = testing_policy)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title =paste("Testing policy in",name_mont[i],sep = " "),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))

#contact_tracing				

(maps_plot[[i]][["Contact_tracing"]] <- ggplot(data = total[[i]]) +
    geom_sf(aes(fill = contact_tracing)) +
    #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
    #         fill = NA, colour = "black", size = 1.5) +
    scale_fill_economist() +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title =paste( "Contact tracing in",name_mont[i],sep=" "),fill="Policy.",subtitle = "Choropleth map",caption=c("Source: Covid19DataHub")))


}

grid.arrange(maps_plot$feb$Confirmed_cases,maps_plot$marc$Confirmed_cases,maps_plot$apr$Confirmed_cases,maps_plot$may$Confirmed_cases,maps_plot$jun$Confirmed_cases,maps_plot$jul$Confirmed_cases, ncol=2)

#monthly comparation plots

comp_monthly <- NULL
for(i in 1:16){
  (comp_monthly[[i]] <- grid.arrange(maps_plot$feb[[i]],maps_plot$marc[[i]],maps_plot$apr[[i]],maps_plot$may[[i]],maps_plot$jun[[i]],maps_plot$jul[[i]], ncol=2))
  Sys.sleep(20)
  
}

#plots per month

maps_plot[[1]][1]

for(j in 1:length(maps_plot)){
  for(i in 1:16){
    plot(maps_plot[[j]][[i]])
    Sys.sleep(30)
  
  }
}
