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
jan <-covid19(start ="2020-01-31" ,end ="2020-01-31",raw = F ) 

feb <-covid19(start ="2020-02-29" ,end ="2020-02-29",raw = F ) 

march <- covid19(start ="2020-03-31" ,end ="2020-03-31",raw = F ) 

april <-covid19(start ="2020-04-30" ,end ="2020-04-30",raw = F )  

may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ,raw = F) 

june <- covid19(start ="2020-06-30" ,end ="2020-06-30",raw = F ) 

july <- covid19(start ="2020-07-31" ,end ="2020-07-31",raw = F ) 

aug <-covid19(start ="2020-08-31" ,end ="2020-08-31",raw = F ) 

sep <-covid19(start ="2020-09-30" ,end ="2020-09-30",raw = F ) 

oct <-covid19(start ="2020-10-31" ,end ="2020-10-31",raw = F ) 

nov <-covid19(start ="2020-11-30" ,end ="2020-11-30",raw = F ) 

dec <-covid19(start ="2020-12-31" ,end ="2020-12-31",raw = F ) 

#the right way to do it
#list of datasets
datasets <- list(jan20=jan,feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july,aug20=aug,sep20=sep,oct20=oct,nov20=nov,dec20=dec)

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
countries <- list(countriesjan=datasets$jan20,countriesf=datasets$feb20,countriesm=datasets$march20,countriesa=datasets$april20,countriesmay=datasets$may20,
                  countriesjun=datasets$june20,countriesjul=datasets$july,countriesaug=datasets$aug20,countriessep=datasets$sep20,countriesoct=datasets$oct20,countriesnov=datasets$nov20,countriesdoc=datasets$dec20)
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

total <- list(totalj=NULL,totalf=NULL,totalm=NULL,totala=NULL,totalma=NULL,totaljun=NULL,totaljul=NULL,totalaug=NULL,totalsep=NULL,totaloct=NULL,totalnov=NULL,totaldec=NULL)


for(i in 1:length(total)){
  total[[i]]<-merge(world,countries[[i]],by="subunit")
  
}

options(scipen=999)
#monthly cases###########################

for(i in 1:length(total)){
  if(i!=1){
    total[[i]]$monthy_confirmed <-total[[i]]$confirmed-total[[i-1]]$confirmed  
    total[[i]]$monthy_recovered <-total[[i]]$recovered-total[[i-1]]$recovered
    total[[i]]$monthy_tested <-total[[i]]$tests-total[[i-1]]$tests
    total[[i]]$monthy_deaths <-total[[i]]$deaths-total[[i-1]]$deaths
  }
}

###################################
#for cumulatove numbers
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


###################################
#for monthly numbers
for(i in 1:length(total)){
  m_recov_case_ratio=NULL
  
  m_death_case_ratio=NULL
  
  m_case_pop_ratio=NULL
  
  m_recov_pop_ratio=NULL
  
  m_death_pop_ratio=NULL
  
  m_test_pop_ratio=NULL
  
  for(j in 1:nrow(total[[i]])){
    
    m_recov_case_ratio[j] <- ((total[[i]]$monthy_recovered[j])/(total[[i]]$monthy_confirmed[j]))
    
    m_death_case_ratio[j] <- ((total[[i]]$monthy_deaths[j])/(total[[i]]$monthy_confirmed[j]))
    
    m_case_pop_ratio[j] <- ((total[[i]]$monthy_confirmed[j])/(total[[i]]$population[j]))
    
    m_death_pop_ratio[j] <- ((total[[i]]$monthy_deaths[j])/(total[[i]]$population[j]))
    
    m_recov_pop_ratio[j] <- ((total[[i]]$monthy_recovered[j])/(total[[i]]$population[j]))
    
    m_test_pop_ratio[j] <- ((total[[i]]$monthy_tested[j])/(total[[i]]$population[j]))
    
  }
  total[[i]]$m_recov_case_ratio <- m_recov_case_ratio
  total[[i]]$m_death_case_ratio <- m_death_case_ratio
  total[[i]]$m_case_pop_ratio <- m_case_pop_ratio
  total[[i]]$m_death_pop_ratio <- m_death_pop_ratio
  total[[i]]$m_recov_pop_ratio <- m_recov_pop_ratio
  total[[i]]$m_test_pop_ratio <- m_test_pop_ratio
  
}

###########################################################################################


#April
coor <- coordinates(total$totala)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totala$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totala, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totala), col='red', lwd=2, add=TRUE)#links

require("RColorBrewer")

#death_pop
moran.plot(total$totala$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated MORE THAN 0.70

#monthly death pop local
local.mi.prod<-localmoran(total$totala$m_death_pop_ratio, PPV3.w)

total$totala$lmi<-local.mi.prod[,1]

total$totala$lmi.p<-local.mi.prod[,5]

total$totala$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"p<.05", ">0.5" )))

#require("RColorBrewer")

#require("sp")

(kc1=spplot(total$totala, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "April",colorkey=FALSE))
?spplot



###########################################################################################


#May
coor <- coordinates(total$totalma)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalma$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalma, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalma), col='red', lwd=2, add=TRUE)#links


#death_pop
moran.plot(total$totalma$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly death pop local
local.mi.prod<-localmoran(total$totalma$m_death_pop_ratio, PPV3.w)

total$totalma$lmi<-local.mi.prod[,1]

total$totalma$lmi.p<-local.mi.prod[,5]

total$totalma$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                          ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

(kc2=spplot(total$totalma, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "May",colorkey=FALSE))
?spplot



###########################################################################################

#June
coor <- coordinates(total$totaljun)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaljun$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaljun, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaljun), col='red', lwd=2, add=TRUE)#links

#death_pop
moran.plot(total$totaljun$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly death pop local
local.mi.prod<-localmoran(total$totaljun$m_death_pop_ratio, PPV3.w)

total$totaljun$lmi<-local.mi.prod[,1]

total$totaljun$lmi.p<-local.mi.prod[,5]

total$totaljun$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

(kc3=spplot(total$totaljun, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "June",colorkey=FALSE))
?spplot

#July
coor <- coordinates(total$totaljul)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaljul$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaljul, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaljul), col='red', lwd=2, add=TRUE)#links

#death_pop
moran.plot(total$totaljul$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated
#monthly death pop local
local.mi.prod<-localmoran(total$totaljul$m_death_pop_ratio, PPV3.w)

total$totaljul$lmi<-local.mi.prod[,1]

total$totaljul$lmi.p<-local.mi.prod[,5]

total$totaljul$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

(kc4=spplot(total$totaljul, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "July",colorkey=FALSE))
?spplot

###################
#nearest neighbours
#august
coor <- coordinates(total$totalaug)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalaug$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalaug, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalaug), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#death_pop
moran.plot(total$totalaug$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalaug$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalaug$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalaug$m_death_pop_ratio, PPV3.w)

total$totalaug$lmi<-local.mi.prod[,1]

total$totalaug$lmi.p<-local.mi.prod[,5]

total$totalaug$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



(kc5=spplot(total$totalaug, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "August",colorkey=FALSE))

###################
#nearest neighbours
#september
coor <- coordinates(total$totalsep)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalsep$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalsep, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalsep), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#case_pop
moran.plot(total$totalsep$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalsep$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalsep$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totalsep$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalsep$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalsep$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalsep$m_death_pop_ratio, PPV3.w)

total$totalsep$lmi<-local.mi.prod[,1]

total$totalsep$lmi.p<-local.mi.prod[,5]

total$totalsep$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



(kc6=spplot(total$totalsep, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "September",colorkey=FALSE))

#nearest neighbours
#october
coor <- coordinates(total$totaloct)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaloct$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaloct, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaloct), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#case_pop
moran.plot(total$totaloct$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaloct$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaloct$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case pop local
local.mi.prod<-localmoran(total$totaloct$m_case_pop_ratio, PPV3.w)

total$totaloct$lmi<-local.mi.prod[,1]

total$totaloct$lmi.p<-local.mi.prod[,5]

total$totaloct$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))


(case_oct=spplot(total$totaloct, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "October",colorkey=F))




#death_pop
moran.plot(total$totaloct$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaloct$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaloct$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totaloct$m_death_pop_ratio, PPV3.w)

total$totaloct$lmi<-local.mi.prod[,1]

total$totaloct$lmi.p<-local.mi.prod[,5]

total$totaloct$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))


(kc7=spplot(total$totaloct, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "October",colorkey=FALSE))

#november
coor <- coordinates(total$totalnov)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalnov$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalnov, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalnov), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#case_pop
moran.plot(total$totalnov$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalnov$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalnov$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#case pop local
local.mi.prod<-localmoran(total$totalnov$m_case_pop_ratio, PPV3.w)

total$totalnov$lmi<-local.mi.prod[,1]

total$totalnov$lmi.p<-local.mi.prod[,5]

total$totalnov$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))


(case_nov=spplot(total$totalnov, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "November",colorkey=F))



#death_pop
moran.plot(total$totalnov$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalnov$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalnov$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalnov$m_death_pop_ratio, PPV3.w)

total$totalnov$lmi<-local.mi.prod[,1]

total$totalnov$lmi.p<-local.mi.prod[,5]

total$totalnov$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))


(kc8=spplot(total$totalnov, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "November",colorkey=FALSE))



#december
coor <- coordinates(total$totaldec)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaldec$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaldec, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaldec), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#case_pop
moran.plot(total$totaldec$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaldec$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaldec$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#case pop local
local.mi.prod<-localmoran(total$totaldec$m_case_pop_ratio, PPV3.w)

total$totaldec$lmi<-local.mi.prod[,1]

total$totaldec$lmi.p<-local.mi.prod[,5]

total$totaldec$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))


(case_dec=spplot(total$totaldec, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "December",colorkey=F))


#death_pop
moran.plot(total$totaldec$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaldec$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaldec$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totaldec$m_death_pop_ratio, PPV3.w)

total$totaldec$lmi<-local.mi.prod[,1]

total$totaldec$lmi.p<-local.mi.prod[,5]

total$totaldec$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



(kc9=spplot(total$totaldec, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "December",colorkey = F))
(kc9=spplot(total$totaldec, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "December",with.legend = "bottom", legend.prop = 0.15))


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#legend
worldsf <- ne_countries(scale='medium',returnclass = 'sf')

july2 <- july
july2$cancel_events<- gsub(0, "Non Significant", july2$cancel_events)
july2$cancel_events <- gsub(1, "<0.05", july2$cancel_events)
july2$cancel_events <- gsub(2, "<0.01", july2$cancel_events)

names(july2)[names(july2) == "administrative_area_level_1"] <- "subunit"
julf<-merge(worldsf,july2,by="subunit")
julf$subunit<-factor(julf$subunit)
julf <- julf[order(julf$confirmed),] # order the data [very important!]


(julho_leg <- ggplot(data = julf) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("orangered","orange","white")) +theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                     legend.spacing.x = unit(0.2, 'cm'),
                                                                     axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                     panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="P-valor:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(julho_leg)

#width=1366&height=678&scale=1
tiff("moran_death.tiff", units="in", width=20, height=10, res=400)
grid.arrange(kc1,kc2,kc3,kc4,kc5,kc6,kc7,kc8,kc9,mylegend,nrow=4,ncol=3)#top="Índice de Moran local sobre o número mensal \n de mortes por habitantes.
dev.off()

tiff("moran_cases.tiff", units="in", width=12, height=6, res=400)
grid.arrange(case_oct,case_nov,case_dec,mylegend,nrow=2,ncol=3)#,top="Índice de Moran local sobre o número mensal \n de casos por habitantes."
dev.off()

novdec=grid.arrange(novplot,decplot,widths=c(0.3,0.3))

ocnov_case=grid.arrange(ocplot_case,novplot_case,decplot_case,widths=c(0.3,0.3))


