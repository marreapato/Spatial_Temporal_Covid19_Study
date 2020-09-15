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
  datasets[[i]]$school_closing <- gsub(2, "Fechamento obrigatório\n(apenas alguns níveis ou categorias)", datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(3, "Fechamento obrigatório para todos os níveis", datasets[[i]]$school_closing)
  
  datasets[[i]]$workplace_closing <- gsub(0, "Sem medidas", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(1, "Recomendou fechamento (Ou home-office)", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(2, "Fechamento obrigatório para alguns setores\nou categorias de trabalho", datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(3, 'Fechamento obrigatório (ou home-office),\nmenos para trabalhos considerados essenciais', datasets[[i]]$workplace_closing)
  
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
  datasets[[i]]$internal_movement_restrictions<- gsub(1, "Recomendação de fechamento\n(ou redução significante no volume/rotas/meios de transporte)", datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(2, "Fechamento Obrigatório\n(ou proibição do uso para maiorioa das pessoas)", datasets[[i]]$internal_movement_restrictions)
  
  datasets[[i]]$international_movement_restrictions<- gsub(0, "Sem medidas", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(1, "Screening", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(2, "Quarantena para recém-chegados\nde regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(3, "Banimento para regiões de alto risco", datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(4, "Fechamento total de fronteiras", datasets[[i]]$international_movement_restrictions)
  
  
  datasets[[i]]$information_campaigns<- gsub(0, "Sem campanha pública de informação", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(1, "Oficiais públicos alertando\npara cautela sobre a COVID-19", datasets[[i]]$information_campaigns)
  datasets[[i]]$information_campaigns<- gsub(2, "Campanha pública de informação coordenada", datasets[[i]]$information_campaigns)
  
  datasets[[i]]$testing_policy<- gsub(0, "Sem política de testagem", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(1, "Apenas os que (a) apresentaram sintomas\nE (b) atingem critérios específicos", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(2, "Testando todos com sintomas da COVID-19", datasets[[i]]$testing_policy)
  datasets[[i]]$testing_policy<- gsub(3, "Testagem pública aberta", datasets[[i]]$testing_policy)
  
  datasets[[i]]$contact_tracing<- gsub(0, "Sem política de testagem", datasets[[i]]$contact_tracing)
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
    theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))
  
c_plots$fev$zdeath <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$ztest <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$fev$zrecov <-ggplot(data = total$totalf) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Fevereiro",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#march

c_plots$mar$zconf <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zdeath <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$ztest <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$mar$zrecov <-ggplot(data = total$totalm) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Março",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#April

c_plots$apr$zconf <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$zdeath <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$ztest <-ggplot(data = total$totala) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$apr$zrecov <-ggplot(data = total$totala) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Abril",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#May

c_plots$may$zconf <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zdeath <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$ztest <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$may$zrecov <-ggplot(data = total$totalma) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Maio",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

#June

c_plots$jun$zconf <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zdeath <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$ztest <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jun$zrecov <-ggplot(data = total$totaljun) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = "none",panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Junho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))

#Julho

c_plots$jul$zconf <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zconfirmed)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.5,2),panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Casos confirmados: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zdeath <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zdeaths)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.5,2),panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Mortes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$ztest <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = ztests)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.5,2),panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Testes: ",caption=c("Fonte: Covid19DataHub"))

c_plots$jul$zrecov <-ggplot(data = total$totaljul) +
  geom_sf(aes(fill = zrecovered)) +
  scale_fill_gradientn(colors=vcolor)+
  theme(legend.position = c(-0.5,2),panel.background = element_rect(fill = "grey"),
        panel.border = element_rect(fill = NA))+labs(title ="Julho",fill="Recuperados: ",caption=c("Fonte: Covid19DataHub"))


#grids continuous
#save in 1600 809

?grid.arrange

#conf
grid.arrange(c_plots$fev$zconf,c_plots$mar$zconf,c_plots$apr$zconf,c_plots$may$zconf,c_plots$jun$zconf,c_plots$jul$zconf,top="Casos confirmados",ncol=2,nrow=3)

#deaths
grid.arrange(c_plots$fev$zdeath,c_plots$mar$zdeath,c_plots$apr$zdeath,c_plots$may$zdeath,c_plots$jun$zdeath,c_plots$jul$zdeath,top="Mortes confirmadas",ncol=2,nrow=3)

#tests
grid.arrange(c_plots$fev$ztest,c_plots$mar$ztest,c_plots$apr$ztest,c_plots$may$ztest,c_plots$jun$ztest,c_plots$jul$ztest,top="Quantidade de testes",ncol=2,nrow=3)

#recov
grid.arrange(c_plots$fev$zrecov,c_plots$mar$zrecov,c_plots$apr$zrecov,c_plots$may$zrecov,c_plots$jun$zrecov,c_plots$jul$zrecov,top="Quantidade de recuperados",ncol=2,nrow=3)


#descriptive information
d_plots <- list(fev=NULL,mar=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL)

#school closures
(d_plots$fev$school <- ggplot(data = total$totalf) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$school <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$school <- ggplot(data = total$totala) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$school <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$school <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$school <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position =c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

#school

grid.arrange(d_plots$fev$school,d_plots$mar$school,d_plots$apr$school,d_plots$may$school,d_plots$jun$school,d_plots$jul$school,top="Política de fechamento de escolas",ncol=2,nrow=3)

###################################################
#stay home

(d_plots$fev$stay <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = stay_home_restrictions)) +
   scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
   theme(legend.position = "none",panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$stay <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$stay <- ggplot(data = total$totala) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$stay <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$stay <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$stay <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position =c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$stay,d_plots$mar$stay,d_plots$apr$stay,d_plots$may$stay,d_plots$jun$stay,d_plots$jul$stay,top="Política de isolamento social",ncol=2,nrow=3)

###################################################
#workplace closing

(d_plots$fev$work <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = workplace_closing)) +
   scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
   theme(legend.position = "none",panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$work <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$work <- ggplot(data = total$totala) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$work <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$work <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$work <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$work,d_plots$mar$work,d_plots$apr$work,d_plots$may$work,d_plots$jun$work,d_plots$jul$work,top="Política de fechamento de locais de trabalho.",ncol=2,nrow=3)

###################################################
#canceled events

(d_plots$fev$cancel <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = cancel_events)) +
   scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
   theme(legend.position = "none",panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$cancel <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$cancel <- ggplot(data = total$totala) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$cancel <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$cancel <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = "none",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$cancel <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#00FFF3","#E53535")) +
    theme(legend.position = c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$cancel,d_plots$mar$cancel,d_plots$apr$cancel,d_plots$may$cancel,d_plots$jun$cancel,d_plots$jul$cancel,top="Política de cancelamento de eventos.",ncol=2,nrow=3)

###################################################
#gatherings_restrictions

(d_plots$fev$gatherings_restrictions <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = gatherings_restrictions)) +
   scale_fill_manual(values=c("#9EF635","#ECEC2A","#00FFF3","#E53535")) +
   theme(legend.position = "none",panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$gatherings_restrictions <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("#9EF635","grey","#ECEC2A","#00FFF3","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$gatherings_restrictions <- ggplot(data = total$totala) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("#9EF635","grey","#ECEC2A","#00FFF3","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$gatherings_restrictions <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("#9EF635","grey","#ECEC2A","#00FFF3","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$gatherings_restrictions <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("#9EF635","grey","#ECEC2A","#00FFF3","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$gatherings_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("#9EF635","grey","#ECEC2A","#00FFF3","#E53535")) +
    theme(legend.position = c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$gatherings_restrictions,d_plots$mar$gatherings_restrictions,d_plots$apr$gatherings_restrictions,d_plots$may$gatherings_restrictions,d_plots$jun$gatherings_restrictions,d_plots$jul$gatherings_restrictions,top="Política de restrição de multidões.",ncol=2,nrow=3)

###################################################
#transport_closing

(d_plots$fev$transport_closing <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = transport_closing)) +
   scale_fill_manual(values=c("#9EF635","#E53535")) +
   theme(legend.position = "",panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$transport_closing <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$transport_closing <- ggplot(data = total$totala) +
    geom_sf(aes(fill =transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$transport_closing <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$transport_closing <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$transport_closing <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = c(-0.35,2),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$transport_closing,d_plots$mar$transport_closing,d_plots$apr$transport_closing,d_plots$may$transport_closing,d_plots$jun$transport_closing,d_plots$jul$transport_closing,top="Política de fechamento de transportes.",ncol=2,nrow=3)
