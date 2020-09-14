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
  datasets[[i]]$school_closing <- gsub(2, "Fechamento obrigatório (apenas alguns níveis ou categorias)", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(3, "Fechamento obrigatório para todos os níveis", datasets[[i]]$school_closing)
  
  datasets[[i]]$workplace_closing <- gsub(0, "Sem medidas", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(1, "Recomendou fechamento (Ou home-office)", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(2, "Fechamento obrigatório para alguns setores ou categorias de trabalho", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(3, 'Fechamento obrigatório (ou home-office), menos para trabalhos considerados essenciais', datasets[[i]]$workplace_closing)
  
  datasets[[i]]$cancel_events<- gsub(0, "Sem medidas", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(1, "Recomendou cancelar", datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(2, "Cancelamento obrigatório", datasets[[i]]$cancel_events)
  
  datasets[[i]]$gatherings_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(1, "Restrições em multidões muito grandes", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(2, "Restrições em multidões entre 100-1000 pessoas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(3, "Restrições em multidões entre 10-100 pessoas", datasets[[i]]$gatherings_restrictions)
  datasets[[i]]$gatherings_restrictions<- gsub(4, "Restrições em multidões com menos de 10 pessoas", datasets[[i]]$gatherings_restrictions)
  
  datasets[[i]]$transport_closing<- gsub(0, "Sem medidas", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(1, "Recomendou fechamento", datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(2, "Fechamento obrigatório", datasets[[i]]$transport_closing)
  
  datasets[[i]]$stay_home_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(1, "Recomendação para ficar em casa", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(2, "Obrigação de ficar em casa exceto para exercício diário,\n compras, e viagens “essencias”", datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(3, "Obrigação de ficar em casa com mínimas excessões", datasets[[i]]$stay_home_restrictions)
  
  datasets[[i]]$internal_movement_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(1, "Recomendação de fechamento (ou redução significante no volume/rotas/meios de transporte)", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(2, "Fechamento Obrigatório (ou proibição do uso para maiorioa das pessoas)", datasets[[i]]$internal_movement_restrictions)
  
  datasets[[i]]$international_movement_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(1, "Screening", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(2, "Quarantena para recém-chegados de regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(3, "Banimento para regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(4, "Fechamento total de fronteiras", datasets[[i]]$international_movement_restrictions)
  
  
  datasets[[i]]$information_campaigns<- gsub(0, "Sem campanha pública de informação", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(1, "Oficiais públicos alertando para cautela sobre a COVID-19", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(2, "Campanha pública de informação coordenada", datasets[[i]]$information_campaigns)
  
  datasets[[i]]$testing_policy<- gsub(0, "Sem política de testagem", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(1, "Apenas os que (a) apresentaram sintomas E (b) atingem critérios específicos", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(2, "Testando todos com sintomas da COVID-19", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(3, "Testagem pública aberta", datasets[[i]]$testing_policy)
  
  datasets[[i]]$contact_tracing<- gsub(0, "Sem política de testagem", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(1, "Rastreamento de contatos limitado, não feito para todos os casos", datasets[[i]]$contact_tracing)
  datasets[[i]]$contact_tracing<- gsub(2, "Rastreamento de contatos, feito para todos os casos", datasets[[i]]$contact_tracing)
  
  
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
ggplot(data = total$totalm) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Número de casos confirmados até Março",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

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
?scale_fill_gradient

vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")

#continuous information

c_plots <- list(fev=NULL,mar=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL)

c_plots$fev$zconf <-ggplot(data = total$totalf) +
    geom_sf(aes(fill = zconfirmed)) +
    scale_fill_gradientn(colors=vcolor)+
    theme(panel.background = element_rect(fill = "grey"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))
  
c_plots$fev$zdeath <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$ztest <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Testes: ",subtitle = "Choropleth map",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zrecov <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Recuperados: ",subtitle = "Choropleth map",caption=c("Fonte: Covid19DataHub"))


#march

c_plots$mar$zconf <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zdeath <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$ztest <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zrecov <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#April

c_plots$apr$zconf <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$zdeath <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$ztest <-ggplot(data = total$totala) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$zrecov <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#May

c_plots$may$zconf <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zdeath <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$ztest <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zrecov <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

#June

c_plots$jun$zconf <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zdeath <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$ztest <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zrecov <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

#Julho

c_plots$jul$zconf <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zdeath <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$ztest <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zrecov <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))










#school closures
ggplot(data = total$totalf) +
  geom_sf(aes(fill = school_closing)) +
  #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
  #         fill = NA, colour = "black", size = 1.5) +
  scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Fechamento de escolas em Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub"))

ggplot(data = total$totaljun) +
  geom_sf(aes(fill = total$totalf$stay_home_restrictions)) +
  #geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
  #         fill = NA, colour = "black", size = 1.5) +
  scale_fill_manual(values=c("#ECEC2A","blue","#9EF635","#E53535")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))+labs(title ="Fechamento de escolas em Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub"))
