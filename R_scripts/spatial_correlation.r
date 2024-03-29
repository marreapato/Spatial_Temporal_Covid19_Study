install.packages("COVID19")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("zoo")
install.packages("sf")
install.packages("maps")
#The package rnaturalearth also provides a map of countries of the entire world
install.packages("rnaturalearth")
install.packages('rnaturalearthdata')
install.packages("rgeos")
install.packages("gridExtra")
install.packages("rgdal")
install.packages("spdep")
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
world <- ne_countries(scale='medium',returnclass = 'sf')
#obs: to check the number of hospitalized patients i'll have aggregate and sum

#saving datasets
feb <-covid19(start ="2020-02-29" ,end ="2020-02-29",raw = F ) 
march <- covid19(start ="2020-03-31" ,end ="2020-03-31",raw = F ) 
april <-covid19(start ="2020-04-30" ,end ="2020-04-30",raw = F )  
may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ,raw = F) 
june <- covid19(start ="2020-06-30" ,end ="2020-06-30",raw = F ) 
july <- covid19(start ="2020-07-31" ,end ="2020-07-31",raw = F ) 

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
  total[[i]]$subunit<-factor(total[[i]]$name)
  #total[[i]] <- total[[i]][order(total[[i]]$confirmed),] # order the data [very important!]
  
}

options(scipen=999)

#continuous information feb###########################
#pattern
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
  total[[i]] <- cbind(total[[i]],recov_case_ratio,death_case_ratio,case_pop_ratio,death_pop_ratio,recov_pop_ratio,test_pop_ratio)
  
}
#deleting NaNs
for(i in 1:length(total)){
  
  for(j in 1:nrow(total[[i]])){
    
    if(is.nan(total[[i]]$recov_case_ratio[j])==TRUE){
      total[[i]]$recov_case_ratio[j]=0
    }
    
    if(is.nan(total[[i]]$death_case_ratio[j])==TRUE){
      total[[i]]$death_case_ratio[j]=0
    }
    
  }
  
}



#patterns

for(i in 1:length(total)){
  zconfirmedpop_ratio=NULL
  zdeathspop_ratio=NULL
  ztestspop_ratio=NULL
  zrecoveredpop_ratio=NULL
  zdeaths_conf_ratio=NULL
  zrecovered_conf_ratio=NULL
  for(j in 1:nrow(total[[i]])){
    
    zconfirmedpop_ratio[j] <- (total[[i]]$case_pop_ratio[j]-min(total[[i]]$case_pop_ratio))/(max(total[[i]]$case_pop_ratio)-min(total[[i]]$case_pop_ratio))
    zdeathspop_ratio[j] <- (total[[i]]$death_pop_ratio[j]-min(total[[i]]$death_pop_ratio))/(max(total[[i]]$death_pop_ratio)-min(total[[i]]$death_pop_ratio))
    ztestspop_ratio[j] <- (total[[i]]$test_pop_ratio[j]-min(total[[i]]$test_pop_ratio))/(max(total[[i]]$test_pop_ratio)-min(total[[i]]$test_pop_ratio))
    zrecoveredpop_ratio[j] <- (total[[i]]$recov_pop_ratio[j]-min(total[[i]]$recov_pop_ratio))/(max(total[[i]]$recov_pop_ratio)-min(total[[i]]$recov_pop_ratio))
    zdeaths_conf_ratio[j] <- (total[[i]]$death_case_ratio[j]-min(total[[i]]$death_case_ratio))/(max(total[[i]]$death_case_ratio)-min(total[[i]]$death_case_ratio))
    zrecovered_conf_ratio[j] <- (total[[i]]$recov_case_ratio[j]-min(total[[i]]$recov_case_ratio))/(max(total[[i]]$recov_case_ratio)-min(total[[i]]$recov_case_ratio))
    
    
  }
  total[[i]] <- cbind(total[[i]],zconfirmedpop_ratio,zdeathspop_ratio,ztestspop_ratio,zrecoveredpop_ratio,zdeaths_conf_ratio,zrecovered_conf_ratio)
  
}

#########################################################
##########################################################

#new way?
#install.packages("ape")
library(ape)

total.dists <- as.matrix(dist(cbind(total$totaljul$latitude,total$totaljul$longitude)))
total.dists.inv <- 1/total.dists
diag(total.dists.inv) <- 0
total.dists.inv[1:5, 1:5]
total.dists.inv[is.infinite(total.dists.inv)] <- 0#getting rid of infinite values
total.dists.inv[is.na(total.dists.inv)] <- 0#getting rid of NA values
total.dists.inv[4,]#each row is a country and each column is also a country

Moran.I(total$totaljul$zdeathspop_ratio, total.dists.inv)


###############

#another way
#plot(total$totaljul$zdeathspop_ratio,total$totaljul$zrecoveredpop_ratio)
#cor.test(total$totaljul$zdeathspop_ratio,total$totaljul$zrecoveredpop_ratio)

queen.r.nb <- poly2nb(total$totaljul,queen = TRUE,row.names = total$totaljul$subunit)
summary(queen.r.nb)
#?nb2listw
lw <- nb2listw(queen.r.nb, style="W", zero.policy=TRUE)
?moran.test
lw$weights
moran.test(total$totaljul$zdeathspop_ratio,lw,zero.policy = TRUE,alternative = "greater")
moran.test(total$totaljul$zrecovered_conf_ratio,lw,zero.policy = TRUE)
#plot(density(total$totaljul$zdeathspop_ratio))
#validation????
moran.mc(nsim=185,total$totaljul$zdeathspop_ratio,lw,zero.policy = TRUE)

#moran.test(total$totaljul$transport_closing,lw,zero.policy = TRUE)

##########################################################

#moran.plot(total$totaljul$transport_closing, listw=lw)
moran.plot(total$totaljul$zdeathspop_ratio, listw=nb2listw(queen.r.nb,style="W",zero.policy = TRUE))

moran.plot(total$totaljul$icu, listw=nb2listw(queen.r.nb,style="W",zero.policy = TRUE))

#nb plot does not work with sf objects
plot(total$totaljul$geometry, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(queen.r.nb, cbind(total$totaljul$latitude,total$totaljul$longitude), col='red', lwd=2, add=TRUE)#links




####################################################

#the right way to do it

#for sp object

world <- ne_countries(scale='medium',returnclass = 'sp')

totaljul <-  NA

totaljul<-merge(world,countries$countriesjul,by="subunit")

death_pop_ratiojul <- ((totaljul$deaths)/(totaljul$population))

zdeathspop_rat <- (death_pop_ratiojul-min(!is.na(death_pop_ratiojul)))/(max(!is.na(death_pop_ratiojul))-min(!is.na(death_pop_ratiojul)))

totaljul$deaths_ratio <- zdeathspop_rat

#totaljul <- cbind(totaljul,"deaths"=death_pop_ratiojul)

totaljul=totaljul[!is.na(totaljul$deaths_ratio),]


#totaljul=totaljul[!is.na(totaljul$deaths_ratio),]
queen.r.nb <- poly2nb(totaljul,queen = TRUE,row.names = totaljul$name)
summary(queen.r.nb)
?poly2nb
#?nb2listw

lw <- nb2listw(queen.r.nb, style="W", zero.policy=TRUE)

plot(totaljul)
plot(totaljul, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(queen.r.nb, coordinates(totaljul), col='red', lwd=2, add=TRUE)#links

moran.test(totaljul$deaths_ratio,lw,zero.policy = TRUE,na.action = na.omit)

?moran.test
moran.mc(nsim=10000,totaljul$deaths_ratio,lw,zero.policy = TRUE,na.action = na.omit)

#links between polygons
#https://rspatial.org/raster/analysis/analysis.pdf
#scatter plot
nci <- moran.plot(totaljul$deaths_ratio, listw = lw)
abline(h=mean(totaljul$deaths_ratio), lt=2)#fixing
?moran.plot

#moran plot hard way, but it works
laglw <- lag.listw(lw,totaljul$deaths_ratio)
?lag.listw

plot(laglw,totaljul$deaths_ratio)
reg <- lm(  totaljul$deaths_ratio ~ laglw )
abline(reg, lwd=2)
abline(h=mean(totaljul$deaths_ratio), lt=2)
abline(v=mean(laglw,na.rm = T), lt=2)


length(totaljul$deaths_ratio)
length(lw$neighbours)
#####################

#nearest neighbours

coor <- coordinates(totaljul)
cartePPV3.knn <- knearneigh(coor, k=2) 
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = totaljul$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)

moran.plot(totaljul$deaths_ratio, PPV3.w, zero.policy=TRUE)
moran.test(totaljul$deaths_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=1000,totaljul$deaths_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

plot(totaljul, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(totaljul), col='red', lwd=2, add=TRUE)#links
###############

#coordinates (centroides)


prodMapdist<- unlist(nbdists(cartePPV3.nb, coor))


max_k1<-max(prodMapdist)
summary(prodMapdist)

prod_kd4<-dnearneigh(coor,d1=min(prodMapdist), d2=max_k1, row.names=totaljul$name)

plot(totaljul)

plot(prod_kd4, coor, add=T,col="green",lwd=2)
lw2=nb2listw(prod_kd4,zero.policy = TRUE)
moran.test(totaljul$deaths_ratio, lw2)
moran.mc(nsim=1000,totaljul$deaths_ratio,lw2,zero.policy = TRUE,na.action = na.omit)


moran.plot(totaljul$deaths_ratio, listw = nb2listw(prod_kd4))


##############################################
#knn method

local.mi.prod<-localmoran(totaljul$deaths_ratio, PPV3.w)

totaljul$lmi<-local.mi.prod[,1]

totaljul$lmi.p<-local.mi.prod[,5]

totaljul$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                      ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(totaljul, "lmi", at=summary(totaljul$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
spplot(totaljul, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Local Moran's i")
?spplot


##############################################
#polygon method

local.mi.prod<-localmoran(totaljul$deaths_ratio, lw)

totaljul$lmi<-local.mi.prod[,1]

totaljul$lmi.p<-local.mi.prod[,5]

totaljul$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                     ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(totaljul, "lmi", at=summary(totaljul$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
spplot(totaljul, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Local Moran's i")
?spplot

##############################################
