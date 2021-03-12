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
countries <- list(countriesjan=datasets$jan20,countriesf=datasets$feb20,countriesm=datasets$march20,countriesa=datasets$april20,countriesmay=datasets$may20,
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


vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")

#descriptive information
d_plots <- list(jan = NULL,fev=NULL,mar=NULL,apr=NULL,may=NULL,jun=NULL,jul=NULL,aug=NULL,
                sep=NULL,oc=NULL,nov=NULL,dec=NULL)

total$totaljul$school_closing <- as.factor(total$totaljul$school_closing)
levels(total$totaljul$school_closing)
total$totaljul$school_closing <- factor(total$totaljul$school_closing,levels=c("Fechamento obrigatório para todos os níveis" ,"Fechamento obrigatório\n(apenas alguns níveis ou categorias)","Recomendou fechamento","Sem medidas"))

#school closures
(d_plots$jan$school <- ggplot(data = total$totaljan) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#00FFF3","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$fev$school <- ggplot(data = total$totalf) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$school <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$school <- ggplot(data = total$totala) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$school <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$school <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

(d_plots$jul$school <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
          legend.spacing.x = unit(0.2, 'cm'),
          axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$school)


(d_plots$jul$school <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none", legend.text=element_text(size=12),legend.box = "horizontal",
          axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

#school width=1366&height=678
grid.arrange(d_plots$jan$school,d_plots$fev$school,d_plots$mar$school,d_plots$apr$school,d_plots$may$school,d_plots$jun$school,mylegend,ncol=3,nrow=3)#,top="Política de fechamento de escolas"

(d_plots$aug$school <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$school <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$school <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$school <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$dec$school <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = school_closing)) +
    scale_fill_manual(values=c("#9EF635","#00FFF3","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$jul$school,d_plots$aug$school,d_plots$sep$school,d_plots$oc$school,d_plots$nov$school,d_plots$dec$school,mylegend,ncol=3,nrow=3)#,top="Política de fechamento de escolas"

###################################################
#stay home

total$totaljul$stay_home_restrictions <- as.factor(total$totaljul$stay_home_restrictions)
levels(total$totaljul$stay_home_restrictions)
#total$totaljul$stay_home_restrictions <- factor(total$totaljul$stay_home_restrictions,levels=c("Fechamento obrigatório para todos os níveis" ,"Fechamento obrigatório\n(apenas alguns níveis ou categorias)","Recomendou fechamento","Sem medidas"))

(d_plots$jan$stay <- ggplot(data = total$totaljan) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$fev$stay <- ggplot(data = total$totalf) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$stay <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$stay <- ggplot(data = total$totala) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$stay <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$stay <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$stay <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                                legend.spacing.x = unit(0.2, 'cm'),
                                                                                axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                                panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$stay)

    

grid.arrange(d_plots$jan$stay,d_plots$fev$stay,d_plots$mar$stay,d_plots$apr$stay,d_plots$may$stay,d_plots$jun$stay,mylegend,ncol=3,nrow=3)

(d_plots$jul$stay <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$aug$stay <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$stay <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$stay <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$stay <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$stay <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = stay_home_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$jul$stay,d_plots$aug$stay,d_plots$sep$stay,d_plots$oc$stay,d_plots$nov$stay,d_plots$dec$stay,mylegend,ncol=3,nrow=3)

###################################################
#workplace closing


(d_plots$jan$work <- ggplot(data = total$totaljan) +
   geom_sf(aes(fill = workplace_closing)) +
   scale_fill_manual(values=c("#00FFF3","#9EF635","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$fev$work <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = workplace_closing)) +
   scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$work <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$work <- ggplot(data = total$totala) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$work <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$work <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$work <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +theme(legend.position =c(1.5,0.55),legend.title=element_text(size=12),legend.text=element_text(size=14),legend.direction = "horizontal",
                                                                                legend.spacing.x = unit(0, 'cm'),
                                                                                axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                                panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$work)

grid.arrange(d_plots$jan$work,d_plots$fev$work,d_plots$mar$work,d_plots$apr$work,d_plots$may$work,d_plots$jun$work,mylegend,ncol=3,nrow=3)

(d_plots$jul$work <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$aug$work <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$work <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$work <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$work <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$work <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = workplace_closing)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position ="none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


grid.arrange(d_plots$jul$work,d_plots$aug$work,d_plots$sep$work,d_plots$oc$work,d_plots$nov$work,d_plots$dec$work,mylegend,ncol=3,nrow=3)

###################################################
#canceled events

(d_plots$jan$cancel <- ggplot(data = total$totaljan) +
   geom_sf(aes(fill = cancel_events)) +
   scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$fev$cancel <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = cancel_events)) +
   scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$cancel <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$cancel <- ggplot(data = total$totala) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$cancel <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$cancel <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$cancel <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                      legend.spacing.x = unit(0.2, 'cm'),
                                                                      axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                      panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$cancel)

grid.arrange(d_plots$jan$cancel,d_plots$fev$cancel,d_plots$mar$cancel,d_plots$apr$cancel,d_plots$may$cancel,d_plots$jun$cancel,mylegend,ncol=3,nrow=3)

(d_plots$jul$cancel <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$aug$cancel <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$cancel <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$cancel <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$cancel <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$cancel <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = cancel_events)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


grid.arrange(d_plots$jul$cancel,d_plots$aug$cancel,d_plots$sep$cancel,d_plots$oc$cancel,d_plots$nov$cancel,d_plots$dec$cancel,mylegend,ncol=3,nrow=3)

###################################################
#gatherings_restrictions

(d_plots$jan$gatherings_restrictions <- ggplot(data = total$totaljan) +
   geom_sf(aes(fill = gatherings_restrictions)) +
   scale_fill_manual(values=c("grey","#9EF635","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$fev$gatherings_restrictions <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = gatherings_restrictions)) +
   scale_fill_manual(values=c("grey","#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$gatherings_restrictions <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$gatherings_restrictions <- ggplot(data = total$totala) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$gatherings_restrictions <- ggplot(data = total$totalma) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$gatherings_restrictions <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$gatherings_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                                       legend.spacing.x = unit(0.2, 'cm'),
                                                                                       axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                                       panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$gatherings_restrictions)

grid.arrange(d_plots$jan$gatherings_restrictions,d_plots$fev$gatherings_restrictions,d_plots$mar$gatherings_restrictions,d_plots$apr$gatherings_restrictions,d_plots$may$gatherings_restrictions,d_plots$jun$gatherings_restrictions,mylegend,ncol=3,nrow=3)

(d_plots$jul$gatherings_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$aug$gatherings_restrictions <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$sep$gatherings_restrictions <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$oc$gatherings_restrictions <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$nov$gatherings_restrictions <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$dec$gatherings_restrictions <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = gatherings_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


grid.arrange(d_plots$jul$gatherings_restrictions,d_plots$aug$gatherings_restrictions,d_plots$sep$gatherings_restrictions,d_plots$oc$gatherings_restrictions,d_plots$nov$gatherings_restrictions,d_plots$dec$gatherings_restrictions,mylegend,ncol=3,nrow=3)

###################################################
#transport_closing

(d_plots$jan$transport_closing <- ggplot(data = total$totaljan) +
   geom_sf(aes(fill = transport_closing)) +
   scale_fill_manual(values=c("#9EF635","#E53535")) +
   theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))



(d_plots$fev$transport_closing <- ggplot(data = total$totalf) +
   geom_sf(aes(fill = transport_closing)) +
   scale_fill_manual(values=c("#9EF635","#E53535")) +
   theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$transport_closing <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$transport_closing <- ggplot(data = total$totala) +
    geom_sf(aes(fill =transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$transport_closing <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$transport_closing <- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$transport_closing <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                       legend.spacing.x = unit(0.2, 'cm'),
                                                                       axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                       panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$transport_closing)

grid.arrange(d_plots$jan$transport_closing,d_plots$fev$transport_closing,d_plots$mar$transport_closing,d_plots$apr$transport_closing,d_plots$may$transport_closing,d_plots$jun$transport_closing,mylegend,ncol=3,nrow=3)

(d_plots$jul$transport_closing <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$aug$transport_closing <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$transport_closing <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$transport_closing <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$transport_closing <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$transport_closing <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = transport_closing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))



grid.arrange(d_plots$jul$transport_closing,d_plots$aug$transport_closing,d_plots$sep$transport_closing,d_plots$oc$transport_closing,d_plots$nov$transport_closing,d_plots$dec$transport_closing,mylegend,ncol=3,nrow=3)

###################################################
#internal_movement_restrictions

(d_plots$jan$internal_movement_restrictions<- ggplot(data = total$totaljan) +
   geom_sf(aes(fill = internal_movement_restrictions)) +
   scale_fill_manual(values=c("#9EF635","#E53535")) +
   theme(legend.position = "none",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$fev$internal_movement_restrictions<- ggplot(data = total$totalf) +
   geom_sf(aes(fill = internal_movement_restrictions)) +
   scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$internal_movement_restrictions <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$internal_movement_restrictions<- ggplot(data = total$totala) +
    geom_sf(aes(fill =internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$internal_movement_restrictions <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$internal_movement_restrictions<- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$internal_movement_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535"))+theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                     legend.spacing.x = unit(0.2, 'cm'),
                                                                     axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                     panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$internal_movement_restrictions)

grid.arrange(d_plots$jan$internal_movement_restrictions,d_plots$fev$internal_movement_restrictions,d_plots$mar$internal_movement_restrictions,d_plots$apr$internal_movement_restrictions,d_plots$may$internal_movement_restrictions,d_plots$jun$internal_movement_restrictions,mylegend,ncol=3,nrow=3)

(d_plots$jul$internal_movement_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))


(d_plots$aug$internal_movement_restrictions <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$internal_movement_restrictions <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$internal_movement_restrictions <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$internal_movement_restrictions <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$internal_movement_restrictions <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = internal_movement_restrictions)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$jul$internal_movement_restrictions,d_plots$aug$internal_movement_restrictions,d_plots$sep$internal_movement_restrictions,d_plots$oc$internal_movement_restrictions,d_plots$nov$internal_movement_restrictions,d_plots$dec$internal_movement_restrictions,mylegend,ncol=3,nrow=3)

###################################################
#international_movement_restrictions

total$totaljul$international_movement_restrictions <- as.factor(total$totaljul$international_movement_restrictions)
levels(total$totaljul$international_movement_restrictions)
total$totaljul$international_movement_restrictions <- factor(total$totaljul$international_movement_restrictions,levels=c("Fechamento total de fronteiras","Banimento para regiões de alto risco","Quarentena para recém-chegados\nde regiões de alto risco","Screening","Sem medidas"))


(d_plots$jan$international_movement_restrictions<- ggplot(data = total$totaljan) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Janeiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))



(d_plots$fev$international_movement_restrictions<- ggplot(data = total$totalf) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$international_movement_restrictions <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","grey","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$international_movement_restrictions<- ggplot(data = total$totala) +
    geom_sf(aes(fill =international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","grey","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$international_movement_restrictions <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","grey","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$international_movement_restrictions<- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("#00FFF3","grey","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$international_movement_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535"))+theme(legend.position =c(1.5,0.55),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                                      legend.spacing.x = unit(0, 'cm'),
                                                                                      axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
                                                                                      panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

mylegend<-g_legend(d_plots$jul$international_movement_restrictions)


grid.arrange(d_plots$jan$international_movement_restrictions,d_plots$fev$international_movement_restrictions,d_plots$mar$international_movement_restrictions,d_plots$apr$international_movement_restrictions,d_plots$may$international_movement_restrictions,d_plots$jun$international_movement_restrictions,mylegend,ncol=3,nrow=3)

(d_plots$jul$international_movement_restrictions <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$aug$international_movement_restrictions <- ggplot(data = total$totalaug) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Agosto.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$sep$international_movement_restrictions <- ggplot(data = total$totalsep) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Setembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$oc$international_movement_restrictions <- ggplot(data = total$totaloc) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Outubro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$nov$international_movement_restrictions <- ggplot(data = total$totalnov) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Novembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$dec$international_movement_restrictions <- ggplot(data = total$totaldec) +
    geom_sf(aes(fill = international_movement_restrictions)) +
    scale_fill_manual(values=c("grey","#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Dezembro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))



grid.arrange(d_plots$jul$international_movement_restrictions,d_plots$aug$international_movement_restrictions,d_plots$sep$international_movement_restrictions,d_plots$oc$international_movement_restrictions,d_plots$nov$international_movement_restrictions,d_plots$dec$international_movement_restrictions,mylegend,ncol=3,nrow=3)

###################################################
#information campaigns

(d_plots$fev$information_campaigns<- ggplot(data = total$totalf) +
   geom_sf(aes(fill = information_campaigns)) +
   scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
   theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
         panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$information_campaigns <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = information_campaigns)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$information_campaigns<- ggplot(data = total$totala) +
    geom_sf(aes(fill =information_campaigns)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$information_campaigns <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =information_campaigns)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$information_campaigns<- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = information_campaigns)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$information_campaigns <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = information_campaigns)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = c(-0.295,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$information_campaigns,d_plots$mar$information_campaigns,d_plots$apr$information_campaigns,d_plots$may$information_campaigns,d_plots$jun$information_campaigns,d_plots$jul$information_campaigns,top="Política pública de informação.",ncol=2,nrow=3)

###################################################
#testing_policy

total$totaljul$testing_policy <- as.factor(total$totaljul$testing_policy)
levels(total$totaljul$testing_policy)
total$totaljul$testing_policy <- factor(total$totaljul$testing_policy,levels=c("Testagem pública aberta","Testando todos com sintomas da COVID-19","Apenas os que (a) apresentaram sintomas\nE (b) atingem critérios específicos","Sem política de testagem"))


(d_plots$fev$testing_policy<- ggplot(data = total$totalf) +
    geom_sf(aes(fill = testing_policy)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535","#00FFF3","#9EF635")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$testing_policy <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = testing_policy)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535","#00FFF3","#9EF635")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$testing_policy<- ggplot(data = total$totala) +
    geom_sf(aes(fill =testing_policy)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535","#00FFF3","#9EF635")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$testing_policy <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =testing_policy)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535","#00FFF3","#9EF635")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$testing_policy<- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = testing_policy)) +
    scale_fill_manual(values=c("#ECEC2A","#E53535","#00FFF3","#9EF635")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$testing_policy <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = testing_policy)) +
    scale_fill_manual(values=c("#00FFF3","#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = c(-0.295,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$testing_policy,d_plots$mar$testing_policy,d_plots$apr$testing_policy,d_plots$may$testing_policy,d_plots$jun$testing_policy,d_plots$jul$testing_policy,top="Política de testagem.",ncol=2,nrow=3)

###################################################
#contact_tracing

total$totaljul$contact_tracing <- as.factor(total$totaljul$contact_tracing)
levels(total$totaljul$contact_tracing)
total$totaljul$contact_tracing <- factor(total$totaljul$contact_tracing,levels=c("Rastreamento de contatos,\nfeito para todos os casos" ,"Rastreamento de contatos limitado,\nnão feito para todos os casos","Sem política de rastreamento"))


(d_plots$fev$contact_tracing<- ggplot(data = total$totalf) +
    geom_sf(aes(fill = contact_tracing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Fevereiro.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$mar$contact_tracing <- ggplot(data = total$totalm) +
    geom_sf(aes(fill = contact_tracing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Março.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$apr$contact_tracing<- ggplot(data = total$totala) +
    geom_sf(aes(fill =contact_tracing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Abril.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$may$contact_tracing <- ggplot(data = total$totalma) +
    geom_sf(aes(fill =contact_tracing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Maio.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jun$contact_tracing<- ggplot(data = total$totaljun) +
    geom_sf(aes(fill = contact_tracing)) +
    scale_fill_manual(values=c("#ECEC2A","#9EF635","#E53535")) +
    theme(legend.position = "",axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Junho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

(d_plots$jul$contact_tracing <- ggplot(data = total$totaljul) +
    geom_sf(aes(fill = contact_tracing)) +
    scale_fill_manual(values=c("#9EF635","#ECEC2A","#E53535")) +
    theme(legend.position = c(-0.295,2),axis.ticks.x=element_blank(), axis.text.x=element_blank(),panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA))+labs(title ="Julho.",fill="Política:",caption=c("Fonte: Covid19DataHub")))

grid.arrange(d_plots$fev$contact_tracing,d_plots$mar$contact_tracing,d_plots$apr$contact_tracing,d_plots$may$contact_tracing,d_plots$jun$contact_tracing,d_plots$jul$contact_tracing,top="Política de rastreamento.",ncol=2,nrow=3)
