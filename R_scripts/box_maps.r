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

  #the right way to do it
#list of datasets
datasets <- list(jan20=jan,feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july,aug20=aug,sep20=sep,oct20=oct)

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
                  countriesjun=datasets$june20,countriesjul=datasets$july,countriesaug=datasets$aug20,countriessep=datasets$sep20,countriesoct=datasets$oct20)
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

total <- list(totalj=NULL,totalf=NULL,totalm=NULL,totala=NULL,totalma=NULL,totaljun=NULL,totaljul=NULL,totalaug=NULL,totalsep=NULL,totaloct=NULL)


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

###################
#nearest neighbours
#January
coor <- coordinates(total$totalj)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalj$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalj, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalj), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#death_pop
moran.plot(total$totalj$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalj$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalj$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalj$death_pop_ratio, PPV3.w)

total$totalj$lmi<-local.mi.prod[,1]

total$totalj$lmi.p<-local.mi.prod[,5]

total$totalj$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalj$death_pop_ratio - mean(total$totalj$death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
# plot in r
?dev.size
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalj,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Janeiro")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
jplot <- recordPlot()
jplot=as_grob(jplot)

jplot=as.ggplot(jplot)
###################
#nearest neighbours
#February
coor <- coordinates(total$totalf)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalf$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalf, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalf), col='red', lwd=2, add=TRUE)#links
#lots of variables missing


#Moran's Is
#cumulative first which is monthly in february
#deaths
moran.plot(total$totalf$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#cases
moran.plot(total$totalf$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totalf$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totalf$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalf$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#ratios
#deaths_case
moran.plot(total$totalf$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totalf$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalf$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#high corr

#case_pop
moran.plot(total$totalf$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#death_pop
moran.plot(total$totalf$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_pop
moran.plot(total$totalf$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests_pop
moran.plot(total$totalf$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#monthly
#death_pop
moran.plot(total$totalf$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalf$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalf$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalf$m_death_pop_ratio, PPV3.w)

total$totalf$lmi<-local.mi.prod[,1]

total$totalf$lmi.p<-local.mi.prod[,5]

total$totalf$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalf$m_death_pop_ratio - mean(total$totalf$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalf,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Fevereiro")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
fplot <- recordPlot()
fplot <- as_grob(fplot)

fplot <- as.ggplot(fplot)
#install.packages("grid")
#install.packages("ggplotify")
library("grid")
library("ggplotify")

(k=plot_grid(jplot,fplot)
#March
coor <- coordinates(total$totalm)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalm$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalm, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalm), col='red', lwd=2, add=TRUE)#links
#Moran's Is
#cumulative first
#deaths
moran.plot(total$totalm$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#cases
moran.plot(total$totalm$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totalm$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totalm$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#ratios
#deaths_case
moran.plot(total$totalm$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totalm$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#case_pop
moran.plot(total$totalm$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totalm$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#recovered_pop
moran.plot(total$totalm$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totalm$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly
#cumulative first
#deaths
moran.plot(total$totalm$monthy_deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#cases
moran.plot(total$totalm$monthy_confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totalm$monthy_recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$monthy_recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totalm$monthy_tested, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#ratios
#deaths_case
moran.plot(total$totalm$m_death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totalm$m_recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#case_pop
moran.plot(total$totalm$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totalm$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death pop local
local.mi.prod<-localmoran(total$totalm$m_death_pop_ratio, PPV3.w)

total$totalm$lmi<-local.mi.prod[,1]

total$totalm$lmi.p<-local.mi.prod[,5]

total$totalm$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalm$m_death_pop_ratio - mean(total$totalm$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalm,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Março")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
mplot <- recordPlot()
mplot <- as_grob(mplot)

mplot <- as.ggplot(mplot)

#recovered_pop
moran.plot(total$totalm$m_recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totalm$m_test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalm$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalm$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

###########################################################################################


#April
coor <- coordinates(total$totala)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totala$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totala, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totala), col='red', lwd=2, add=TRUE)#links

#Moran's Is
#cumulative first
#deaths
moran.plot(total$totala$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#cases
moran.plot(total$totala$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totala$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totala$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#ratios
#deaths_case
moran.plot(total$totala$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totala$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case_pop
moran.plot(total$totala$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totala$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated more than 0.50

####
#death pop local
local.mi.prod<-localmoran(total$totala$death_pop_ratio, PPV3.w)

total$totala$lmi<-local.mi.prod[,1]

total$totala$lmi.p<-local.mi.prod[,5]

total$totala$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

require("RColorBrewer")

#require("sp")

spplot(total$totala, "lmi", at=summary(total$totala$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
ck1=spplot(total$totala, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Abril")
?spplot


####

#recovered_pop
moran.plot(total$totala$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totala$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly
#cumulative first
#deaths
moran.plot(total$totala$monthy_deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#cases
moran.plot(total$totala$monthy_confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#recovered
moran.plot(total$totala$monthy_recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$monthy_recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totala$monthy_tested, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#ratios
#deaths_case
moran.plot(total$totala$m_death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totala$m_recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case_pop
moran.plot(total$totala$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

####
#death pop local
local.mi.prod<-localmoran(total$totala$m_death_pop_ratio, PPV3.w)

total$totala$lmi<-local.mi.prod[,1]

total$totala$lmi.p<-local.mi.prod[,5]

total$totala$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(total$totala, "lmi", at=summary(total$totala$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
spplot(total$totala, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Local Moran's i")
?spplot


####


#death_pop
moran.plot(total$totala$m_death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated MORE THAN 0.70

#monthly death pop local
local.mi.prod<-localmoran(total$totala$m_death_pop_ratio, PPV3.w)

total$totala$lmi<-local.mi.prod[,1]

total$totala$lmi.p<-local.mi.prod[,5]

total$totala$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(total$totala, "lmi", at=summary(total$totala$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
kc1=spplot(total$totala, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Abril")
?spplot



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totala$m_death_pop_ratio - mean(total$totala$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totala,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Abril")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
aplot <- recordPlot()
aplot <- as_grob(aplot)

aplot <- as.ggplot(aplot)

aplot
?grid.arrange
grid.arrange(jplot,fplot,mplot,aplot,widths=c(2,2,2,2))

plot_grid(jplot,fplot,mplot,rel_widths = 0.2)
?plot_grid
####

#recovered_pop
moran.plot(total$totala$m_recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totala$m_test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totala$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totala$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated


###########################################################################################


#May
coor <- coordinates(total$totalma)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totalma$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totalma, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totalma), col='red', lwd=2, add=TRUE)#links

#Moran's Is
#cumulative first
#deaths
moran.plot(total$totalma$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#cases
moran.plot(total$totalma$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totalma$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totalma$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#ratios
#deaths_case
moran.plot(total$totalma$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#recovered_case
moran.plot(total$totalma$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case_pop
moran.plot(total$totalma$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totalma$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated more than 0.50

#death pop local
local.mi.prod<-localmoran(total$totalma$death_pop_ratio, PPV3.w)

total$totalma$lmi<-local.mi.prod[,1]

total$totalma$lmi.p<-local.mi.prod[,5]

total$totalma$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                          ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(total$totalma, "lmi", at=summary(total$totalma$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
ck2=spplot(total$totalma, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Maio")
?spplot


####

#recovered_pop
moran.plot(total$totalma$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totalma$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly
#cumulative first
#deaths
moran.plot(total$totalma$monthy_deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#cases
moran.plot(total$totalma$monthy_confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#recovered
moran.plot(total$totalma$monthy_recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$monthy_recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totalma$monthy_tested, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)

###############################################################################################

#ratios
#deaths_case
moran.plot(total$totalma$m_death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totalma$m_recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

################################################################


#case_pop
moran.plot(total$totalma$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

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

spplot(total$totalma, "lmi", at=summary(total$totalma$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
kc2=spplot(total$totalma, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Maio")
?spplot


#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalma$m_death_pop_ratio - mean(total$totalma$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalma,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Maio")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
maplot <- recordPlot()
maplot <- as_grob(maplot)

maplot <- as.ggplot(maplot)

maplot

#recovered_pop
moran.plot(total$totalma$m_recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totalma$m_test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totalma$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totalma$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated


###########################################################################################

#June
coor <- coordinates(total$totaljun)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaljun$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaljun, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaljun), col='red', lwd=2, add=TRUE)#links

#Moran's Is
#cumulative first
#deaths
moran.plot(total$totaljun$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#cases
moran.plot(total$totaljun$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totaljun$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totaljun$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#ratios
#deaths_case
moran.plot(total$totaljun$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#recovered_case
moran.plot(total$totaljun$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case_pop
moran.plot(total$totaljun$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totaljun$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated more than 0.50

#death pop local
local.mi.prod<-localmoran(total$totaljun$death_pop_ratio, PPV3.w)

total$totaljun$lmi<-local.mi.prod[,1]

total$totaljun$lmi.p<-local.mi.prod[,5]

total$totaljun$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(total$totaljun, "lmi", at=summary(total$totaljun$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
ck3=spplot(total$totaljun, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Junho")
?spplot


####

#recovered_pop
moran.plot(total$totaljun$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totaljun$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly
#cumulative first
#deaths
moran.plot(total$totaljun$monthy_deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#cases
moran.plot(total$totaljun$monthy_confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totaljun$monthy_recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$monthy_recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totaljun$monthy_tested, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)

###############################################################################################

#ratios
#deaths_case
moran.plot(total$totaljun$m_death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totaljun$m_recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

################################################################


#case_pop
moran.plot(total$totaljun$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

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

spplot(total$totaljun, "lmi", at=summary(total$totaljul$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
kc3=spplot(total$totaljun, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Junho")
?spplot

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totaljun$m_death_pop_ratio - mean(total$totaljun$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totaljun,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Junho")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
junplot <- recordPlot()
junplot <- as_grob(junplot)

junplot <- as.ggplot(junplot)


####

#recovered_pop
moran.plot(total$totaljun$m_recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totaljun$m_test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljun$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljun$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated


###########################################################################################


#July
coor <- coordinates(total$totaljul)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = total$totaljul$name)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(total$totaljul, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(total$totaljul), col='red', lwd=2, add=TRUE)#links

#Moran's Is
#cumulative first
#deaths
moran.plot(total$totaljul$deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#cases
moran.plot(total$totaljul$confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered
moran.plot(total$totaljul$recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totaljul$tests, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$tests,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#ratios
#deaths_case
moran.plot(total$totaljul$death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#recovered_case
moran.plot(total$totaljul$recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#case_pop
moran.plot(total$totaljul$case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#death_pop
moran.plot(total$totaljul$death_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$death_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated more than 0.50

#death pop local
local.mi.prod<-localmoran(total$totaljul$death_pop_ratio, PPV3.w)

total$totaljul$lmi<-local.mi.prod[,1]

total$totaljul$lmi.p<-local.mi.prod[,5]

total$totaljul$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(total$totaljul, "lmi", at=summary(total$totaljul$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
ck4=spplot(total$totaljul, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Julho")
?spplot


####

#recovered_pop
moran.plot(total$totaljul$recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totaljul$test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#monthly
#cumulative first
#deaths
moran.plot(total$totaljul$monthy_deaths, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$monthy_deaths,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#cases
moran.plot(total$totaljul$monthy_confirmed, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$monthy_confirmed,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

#recovered
moran.plot(total$totaljul$monthy_recovered, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$monthy_recovered,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#tests
moran.plot(total$totaljul$monthy_tested, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$monthy_tested,PPV3.w,zero.policy = TRUE,na.action = na.omit)

###############################################################################################

#ratios
#deaths_case
moran.plot(total$totaljul$m_death_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_death_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#recovered_case
moran.plot(total$totaljul$m_recov_case_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$m_recov_case_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#not validated

################################################################


#case_pop
moran.plot(total$totaljul$m_case_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$m_case_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

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

spplot(total$totaljul, "lmi", at=summary(total$totaljul$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
kc4=spplot(total$totaljul, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Julho")
?spplot

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totaljul$m_death_pop_ratio - mean(total$totaljul$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totaljul,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Julho")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
julplot <- recordPlot()
julplot <- as_grob(julplot)

julplot <- as.ggplot(julplot)


####

#recovered_pop
moran.plot(total$totaljul$m_recov_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$m_recov_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated

#tests_pop
moran.plot(total$totaljul$m_test_pop_ratio, PPV3.w, zero.policy=TRUE)
moran.test(total$totaljul$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,total$totaljul$m_test_pop_ratio,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated
#width=1366&height=678
grid.arrange(ck1,ck2,ck3,ck4,top="Índice de Moran local sobre o número cumulativo \n de mortes por habitantes.")
?grid.arrange()

grid.arrange(kc1,kc2,kc3,kc4,top="Índice de Moran local sobre o número mensal \n de mortes por habitantes.")

janfe=grid.arrange(jplot,fplot,top="Boxmap sobre o número mensal \n de mortes por habitantes.",widths=c(0.3,0.3))

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

#monthly
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



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalaug$m_death_pop_ratio - mean(total$totalaug$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalaug,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Agosto")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
augplot <- recordPlot()
augplot <- as_grob(augplot)

augplot <- as.ggplot(augplot)


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



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totalsep$m_death_pop_ratio - mean(total$totalsep$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totalsep,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Setembro")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
sepplot <- recordPlot()
sepplot <- as_grob(sepplot)

sepplot <- as.ggplot(sepplot)


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



#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- total$totaloct$m_death_pop_ratio - mean(total$totaloct$m_death_pop_ratio)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(total$totaloct,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Outubro")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")
ocplot <- recordPlot()
ocplot <- as_grob(ocplot)

ocplot <- as.ggplot(ocplot)

#https://rpubs.com/quarcs-lab/spatial-autocorrelation
#install.packages("cowplot")
library(gridGraphics)
library(cowplot)

janfe=grid.arrange(jplot,fplot,top="Índice de Moran local sobre o número cumulativo \n de mortes por habitantes.",widths=c(0.3,0.3))

grid.arrange(jplot,fplot, ncol = 2, main = "Main title")
dev.off()
layout(matrix(c(1,1), nrow = 1, ncol = 1, byrow = TRUE))                              
janfe
jplot
fplot
library("ggpubr")
ggarrange(plotlist = list(jplot,fplot),widths = c(0.2,0.2),heights=c(0.1,0.1))
ggarrange(plotlist = list(mplot,aplot),widths = c(0.2,0.2),heights=c(0.1,0.1))
ggarrange(plotlist = list(maplot,junplot),widths = c(0.2,0.2),heights=c(0.1,0.1))
ggarrange(plotlist = list(julplot,augplot),widths = c(0.2,0.2),heights=c(0.1,0.1))
ggarrange(plotlist = list(sepplot,ocplot),widths = c(0.2,0.2),heights=c(0.1,0.1))
#width=1366&height=678
janfe=grid.arrange(jplot,fplot,widths=c(0.3,0.3))
mara=grid.arrange(mplot,aplot,widths=c(0.3,0.3))
majun=grid.arrange(maplot,junplot,widths=c(0.3,0.3))
julaug=grid.arrange(julplot,augplot,widths=c(0.3,0.3))
sepoc=grid.arrange(sepplot,ocplot,widths=c(0.3,0.3))

#merge here
#https://onlineconvertfree.com/merge-image/png-merge/
