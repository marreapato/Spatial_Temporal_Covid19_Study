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

world <- ne_countries(scale='medium',returnclass = 'sp')

#saving datasets
feb <-covid19(start ="2020-02-29" ,end ="2020-02-29",raw = F ) 

march <- covid19(start ="2020-03-31" ,end ="2020-03-31",raw = F ) 

april <-covid19(start ="2020-04-30" ,end ="2020-04-30",raw = F )  

may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ,raw = F) 

june <- covid19(start ="2020-06-30" ,end ="2020-06-30",raw = F ) 

july <- covid19(start ="2020-07-31" ,end ="2020-07-31",raw = F ) 

#the right way to do it
#list of datasets
datasets <- list(feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july)

#renaming a few cells in the datasets

for(i in 1:length(datasets)){
  
  datasets[[i]]$school_closing <- gsub(0, "Sem medidas", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(1, "Recomendou fechamento", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(2, "Fechamento obrigatório\n(apenas alguns níveis ou categorias)", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(3, "Fechamento obrigatório para todos os níveis", datasets[[i]]$school_closing)
  
  datasets[[i]]$workplace_closing <- gsub(0, "Sem medidas", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(1, "Recomendou fechamento (Ou home-office)", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(2, "Fechamento obrigatório para alguns setores\nou categorias de trabalho", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(3, 'Fechamento obrigatório (ou home-office),\nmenos para trabalhos considerados essenciais\n', datasets[[i]]$workplace_closing)
  
  datasets[[i]]$cancel_events<- gsub(0, "Sem medidas", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(1, "Recomendou cancelar", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(2, "Cancelamento obrigatório", datasets[[i]]$cancel_events)
  
  datasets[[i]]$gatherings_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(1, "Restrições em multidões\nmuito grandes", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(2, "Restrições em multidões\nentre 100-1000 pessoas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(3, "Restrições em multidões\nentre 10-100 pessoas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(4, "Restrições em multidões\ncom menos de 10 pessoas", datasets[[i]]$gatherings_restrictions)
  
  datasets[[i]]$transport_closing<- gsub(0, "Sem medidas", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(1, "Recomendou fechamento", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(2, "Fechamento obrigatório", datasets[[i]]$transport_closing)
  
  datasets[[i]]$stay_home_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(1, "Recomendação para ficar em casa", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(2, "Obrigação de ficar em casa exceto para\nexercício diário, compras, e viagens “essencias”", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(3, "Obrigação de ficar em casa\ncom mínimas excessões", datasets[[i]]$stay_home_restrictions)
  
  datasets[[i]]$internal_movement_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(1, "Recomendação de fechamento\n(ou redução significante no volume/\nrotas/meios de transporte)", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(2, "Fechamento Obrigatório\n(ou proibição do uso para maioria das pessoas)\n", datasets[[i]]$internal_movement_restrictions)
  
  datasets[[i]]$international_movement_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(1, "Screening", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(2, "Quarentena para recém-chegados\nde regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(3, "Banimento para regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(4, "Fechamento total de fronteiras", datasets[[i]]$international_movement_restrictions)
  
  
  datasets[[i]]$information_campaigns<- gsub(0, "Sem campanha pública de informação", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(1, "Oficiais públicos alertando\npara cautela sobre a COVID-19", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(2, "Campanha pública de informação coordenada", datasets[[i]]$information_campaigns)
  
  datasets[[i]]$testing_policy<- gsub(0, "Sem política de testagem", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(1, "Apenas os que (a) apresentaram sintomas\nE (b) atingem critérios específicos", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(2, "Testando todos com sintomas da COVID-19", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(3, "Testagem pública aberta", datasets[[i]]$testing_policy)
  
  datasets[[i]]$contact_tracing<- gsub(0, "Sem política de rastreamento", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(1, "Rastreamento de contatos limitado,\nnão feito para todos os casos", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(2, "Rastreamento de contatos,\nfeito para todos os casos", datasets[[i]]$contact_tracing)
  
  
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

}

options(scipen=999)

for(i in 1:length(total)){
  recov_case_ratio=NULL
  
  death_case_ratio=NULL
  
  case_pop_ratio=NULL
  
  recov_pop_ratio=NULL
  
  death_pop_ratio=NULL
  
  test_pop_ratio=NULL
  
  for(j in 1:nrow(total[[i]])){
    
    recov_case_ratio[j] <- ((total[[i]]$recovered[j])/(total[[i]]$confirmed[j]))
    
    death_case_ratio[j] <- ((total[[i]]$deaths[j])/(total[[i]]$confirmed[j]))
    
    case_pop_ratio[j] <- ((total[[i]]$confirmed[j])/(total[[i]]$population[j]))
    
    death_pop_ratio[j] <- ((total[[i]]$deaths[j])/(total[[i]]$population[j]))
    
    recov_pop_ratio[j] <- ((total[[i]]$recovered[j])/(total[[i]]$population[j]))
    
    test_pop_ratio[j] <- ((total[[i]]$tests[j])/(total[[i]]$population[j]))
    
  }
  total[[i]]$recov_case_ratio <- recov_case_ratio
  total[[i]]$death_case_ratio <- death_case_ratio
  total[[i]]$case_pop_ratio <- case_pop_ratio
  total[[i]]$death_pop_ratio <- death_pop_ratio
  total[[i]]$recov_pop_ratio <- recov_pop_ratio
  total[[i]]$test_pop_ratio <- test_pop_ratio
  
  total[[i]]=total[[i]][!is.na(total[[i]]$recov_case_ratio),]
  total[[i]]=total[[i]][!is.na(total[[i]]$death_case_ratio),]
  total[[i]]=total[[i]][!is.na(total[[i]]$case_pop_ratio),]
  total[[i]]=total[[i]][!is.na(total[[i]]$death_pop_ratio),]
  total[[i]]=total[[i]][!is.na(total[[i]]$recov_pop_ratio),]
  total[[i]]=total[[i]][!is.na(total[[i]]$test_pop_ratio),]
  
}

queen.r.nb <- poly2nb(total$totaljul,queen = TRUE,row.names = total$totaljul$name)
summary(queen.r.nb)
?poly2nb
#?nb2listw

lw <- nb2listw(queen.r.nb, style="W", zero.policy=TRUE)

plot(total$totaljul)
plot(total$totaljul, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(queen.r.nb, coordinates(total$totaljul), col='red', lwd=2, add=TRUE)#links

moran.test(total$totaljul$death_case_ratio,lw,zero.policy = TRUE,na.action = na.omit)