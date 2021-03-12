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

options(scipen=999)

world <- ne_countries(scale='medium',returnclass = 'sf')
#obs: to check the number of hospitalized patients i'll have aggregate and sum

#saving datasets
jan <-covid19(start ="2020-01-31" ,end ="2020-01-31",raw = F ) 
feb <-covid19(start ="2020-02-29" ,end ="2020-02-29",raw = F ) 
march <- covid19(start ="2020-03-31" ,end ="2020-03-31",raw = F ) 
april <-covid19(start ="2020-04-30" ,end ="2020-04-30",raw = F )  
may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ,raw = F) 
june <- covid19(start ="2020-06-30" ,end ="2020-06-30",raw = F ) 
july <- covid19(start ="2020-07-31" ,end ="2020-07-31",raw = F ) 
ag <- covid19(start ="2020-08-31" ,end ="2020-08-31",raw = F ) 
sep <- covid19(start ="2020-09-30" ,end ="2020-09-30",raw = F ) 
oct <- covid19(start ="2020-10-31" ,end ="2020-10-31",raw = F ) 
nov <-covid19(start ="2020-11-30" ,end ="2020-11-30",raw = F ) 
dec <-covid19(start ="2020-12-31" ,end ="2020-12-31",raw = F ) 
jan21<-covid19(start ="2021-01-31" ,end ="2021-01-31",raw = F ) 
feb21<-covid19(start ="2021-02-28" ,end ="2021-02-28",raw = F ) 
#list of datasets
datasets <- list(jan20=jan,feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july,ag20=ag,sep20=sep,oc20=oct,nov20=nov,dec20=dec,jan21=jan21,feb21=feb21)

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
countries <- list(countresjan=datasets$jan20,countriesf=datasets$feb20,countriesm=datasets$march20,countriesa=datasets$april20,countriesmay=datasets$may20,
                  countriesjun=datasets$june20,countriesjul=datasets$july
                  ,countriesaug=datasets$ag20,countriessep=datasets$sep20,
                  countriesoc=datasets$oc20,countriesnov=datasets$nov20,
                  countriesdec=datasets$dec20)
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

total <- list(totaljan=NULL,totalf=NULL,totalm=NULL,totala=NULL,totalma=NULL,totaljun=NULL,totaljul=NULL,totalaug=NULL
              ,totalsep=NULL,totaloc=NULL,totalnov=NULL,totaldec=NULL)

for(i in 1:length(total)){
  total[[i]]<-merge(world,countries[[i]],by="subunit")
  total[[i]]$subunit<-factor(total[[i]]$subunit)
  total[[i]] <- total[[i]][order(total[[i]]$confirmed),] # order the data [very important!]
  
}

options(scipen=999)
#continuous information feb###########################
#pattern
for(i in 1:length(total)){
  zconfirmed=NULL
  zdeaths=NULL
  ztests=NULL
  zrecovered=NULL
  for(j in 1:nrow(total[[i]])){
    
    zconfirmed[j] <- (total[[i]]$confirmed[j]-min(total[[i]]$confirmed))/(max(total[[i]]$confirmed)-min(total[[i]]$confirmed))
    zdeaths[j] <- (total[[i]]$deaths[j]-min(total[[i]]$deaths))/(max(total[[i]]$deaths)-min(total[[i]]$deaths))
    ztests[j] <- (total[[i]]$tests[j]-min(total[[i]]$tests))/(max(total[[i]]$tests)-min(total[[i]]$tests))
    zrecovered[j] <- (total[[i]]$recovered[j]-min(total[[i]]$recovered))/(max(total[[i]]$recovered)-min(total[[i]]$recovered))
    
    
  }
  total[[i]] <- cbind(total[[i]],zconfirmed,zdeaths,ztests,zrecovered)
  
}

vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")

#continuous information

c_plots <- list(jan=NULL,fev=NULL,mar=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL,aug=NULL,
                sep=NULL,oc=NULL,nov=NULL,dec=NULL)

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

#continuous information

c_plots <- list(fev=NULL,mar=NULL,abr=NULL,may=NULL,jun=NULL,jul=NULL)
vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")

#jan

(c_plots$jan$zconfpop <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub")))

c_plots$jan$zdeathpop <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jan$ztestpop <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jan$zrecovpop <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jan$zdeathcase <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jan$zrecovcase <-ggplot(data = total$totaljan) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Janeiro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))



#fev

c_plots$fev$zconfpop <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zdeathpop <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$ztestpop <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zrecovpop <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zdeathcase <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zrecovcase <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Fevereiro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#march

c_plots$mar$zconfpop <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zdeathpop <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$ztestpop <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zrecovpop <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zdeathcase <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zrecovcase <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Março",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))




#April

c_plots$abr$zconfpop <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$abr$zdeathpop <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$abr$ztestpop <-ggplot(data = total$totala) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$abr$zrecovpop <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$abr$zdeathcase <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$abr$zrecovcase <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Abril",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))



#May
c_plots$may$zconfpop <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zdeathpop <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$ztestpop <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zrecovpop <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zdeathcase <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zrecovcase <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Maio",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))



#June
c_plots$jun$zconfpop <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zdeathpop <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$ztestpop <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position =c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zrecovpop <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zdeathcase <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zrecovcase <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Junho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))



#Julho
c_plots$jul$zconfpop <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zdeathpop <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$ztestpop <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zrecovpop <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zdeathcase <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zrecovcase <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Julho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#aug
c_plots$aug$zconfpop <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$aug$zdeathpop <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$aug$ztestpop <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position ="",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$aug$zrecovpop <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$aug$zdeathcase <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$aug$zrecovcase <-ggplot(data = total$totalaug) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Agosto",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#setembro
c_plots$sep$zconfpop <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$sep$zdeathpop <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$sep$ztestpop <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$sep$zrecovpop <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$sep$zdeathcase <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$sep$zrecovcase <-ggplot(data = total$totalsep) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Setembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#outubro
c_plots$oc$zconfpop <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$oc$zdeathpop <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position ="",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$oc$ztestpop <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$oc$zrecovpop <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$oc$zdeathcase <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$oc$zrecovcase <-ggplot(data = total$totaloc) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Outubro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#Novembro
c_plots$nov$zconfpop <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$nov$zdeathpop <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$nov$ztestpop <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$nov$zrecovpop <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position ="",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$nov$zdeathcase <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$nov$zrecovcase <-ggplot(data = total$totalnov) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Novembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#Dezembro
c_plots$dec$zconfpop <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = zconfirmedpop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$dec$zdeathpop <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = zdeathspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$dec$ztestpop <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = ztestspop_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$dec$zrecovpop <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = zrecoveredpop_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$dec$zdeathcase <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = zdeaths_conf_ratio)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$dec$zrecovcase <-ggplot(data = total$totaldec) +
  geom_sf(aes(fill = zrecovered_conf_ratio)) +
  scale_fill_gradientn(colors=i_vcolor)+
  theme(legend.position = c(-0.35,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Até Dezembro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))



#deaths
grid.arrange(c_plots$jan$zdeathpop,c_plots$fev$zdeathpop,c_plots$mar$zdeathpop,c_plots$abr$zdeathpop,c_plots$may$zdeathpop,c_plots$jun$zdeathpop,ncol=2,nrow=3)

#deaths
grid.arrange(c_plots$jul$zdeathpop,c_plots$aug$zdeathpop,c_plots$sep$zdeathpop,c_plots$oc$zdeathpop,c_plots$nov$zdeathpop,c_plots$dec$zdeathpop,ncol=2,nrow=3)

#confirmed
grid.arrange(c_plots$fev$zconfpop,c_plots$mar$zconfpop,c_plots$abr$zconfpop,c_plots$may$zconfpop,c_plots$jun$zconfpop,c_plots$jul$zconfpop,top="Casos confirmados por quantidade populacional",ncol=2,nrow=3)

#recovered
grid.arrange(c_plots$fev$zrecovpop,c_plots$mar$zrecovpop,c_plots$abr$zrecovpop,c_plots$may$zrecovpop,c_plots$jun$zrecovpop,c_plots$jul$zrecovpop,top="Recuperados por quantidade populacional",ncol=2,nrow=3)

#tests
grid.arrange(c_plots$fev$ztestpop,c_plots$mar$ztestpop,c_plots$abr$ztestpop,c_plots$may$ztestpop,c_plots$jun$ztestpop,c_plots$jul$ztestpop,top="Testes por quantidade populacional",ncol=2,nrow=3)

#recovered
grid.arrange(c_plots$fev$zrecovcase,c_plots$mar$zrecovcase,c_plots$abr$zrecovcase,c_plots$may$zrecovcase,c_plots$jun$zrecovcase,c_plots$jul$zrecovcase,top="Recuperados por quantidade de casos",ncol=2,nrow=3)

#deaths
grid.arrange(c_plots$fev$zdeathcase,c_plots$mar$zdeathcase,c_plots$abr$zdeathcase,c_plots$may$zdeathcase,c_plots$jun$zdeathcase,c_plots$jul$zdeathcase,top="Mortes por quantidade de casos",ncol=2,nrow=3)

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(d_plots$jul$school)
