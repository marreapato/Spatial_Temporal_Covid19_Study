install.packages("COVID19")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("gridExtra")
library(gridExtra)
library(COVID19)
library(tidyverse)
library(ggthemes)
library(ggarrange)
covid <- covid19(raw=F)
#27218 <- one
#recomendou fechar (1) e n fechou (0) serao 0
#categorias = https://covid19datahub.io/articles/doc/data.html

covid$school_closing <- ifelse(covid$school_closing==0|covid$school_closing==1,0,1)
covid$workplace_closing <- ifelse(covid$workplace_closing==0|covid$workplace_closing==1,0,1)
covid$cancel_events <- ifelse(covid$cancel_events==0|covid$cancel_events==1,0,1)
covid$gatherings_restrictions <- ifelse(covid$gatherings_restrictions!=0,1,0)#diferente
covid$transport_closing <- ifelse(covid$transport_closing==0|covid$transport_closing==1,0,1)
covid$stay_home_restrictions <- ifelse(covid$stay_home_restrictions==0|covid$stay_home_restrictions==1,0,1)
covid$internal_movement_restrictions <- ifelse(covid$internal_movement_restrictions==0|covid$internal_movement_restrictions==1,0,1)
covid$international_movement_restrictions <- ifelse(covid$international_movement_restrictions==0|covid$international_movement_restrictions==1,0,1)
covid$information_campaigns <- ifelse(covid$information_campaigns==0|covid$information_campaigns==1,0,1)
covid$testing_policy <- ifelse(covid$testing_policy!=0,1,0)#diferente
covid$contact_tracing <- ifelse(covid$contact_tracing==0|covid$contact_tracing==1,0,1)

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)

#pegando daily cases, todos paises comecam em 2020-01-01
case_daily=NULL
deaths_daily=NULL
vaccines_daily=NULL

for(i in 1:nrow(covid)){
   if(covid$date[i]!="2020-01-01"){
  case_daily[i] <- covid$confirmed[i]-covid$confirmed[i-1]
  deaths_daily[i] <- covid$deaths[i]-covid$deaths[i-1]
  vaccines_daily[i] <- covid$vaccines[i]-covid$vaccines[i-1]
     }else if(covid$date[i]=="2020-01-01"){ 
       
    vaccines_daily[i] <- covid$vaccines[i]   
    case_daily[i] <- covid$confirmed[i]
    deaths_daily[i] <- covid$deaths[i]
     
     }
  
  #if(case_daily[i]<0){
   # case_daily[i] <- 0
  #}else if(deaths_daily[i]<0){
   # deaths_daily[i] <- 0
  #}
}

ncovid <- cbind(covid,"Daily_cases"=case_daily,"Daily_deaths"=deaths_daily,"Daily_vaccines"=vaccines_daily)

#Tests
nacovid <- ncovid %>% filter(id=="GBR")
nacovid <- nacovid %>% filter(vaccines>=1)
options(scipen=999)
#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
uk <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="United Kingdom \n(cor = -0.6688; CI = (-0.7871,-0.5031); p<0.0001)",x="",y="")
uk

#Paises
par(mfrow=c(4,2))
nacovid <- ncovid %>% filter(id=="USA")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

us <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="United States \n(cor = -0.7638; CI = (-0.8557,-0.6253); p<0.0001)",x="",y="")
us
#Paises

nacovid <- ncovid %>% filter(id=="RUS")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
rus <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="Russia \n(cor = -0.8032; CI = (-0.8780,-0.6900); p<0.0001)",x="",y="")
rus

#Paises

nacovid <- ncovid %>% filter(id=="CAN")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
can <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="Canada \n(cor = -0.8718; CI = (-0.9360,-0.7517); p<0.0001)",x="",y="")
can

#Paises

nacovid <- ncovid %>% filter(id=="IRL")#IRELAND
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
irl <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="Ireland \n(cor = -0.7967; CI = (-0.8844,-0.6546); p<0.0001)",x="",y="")
irl

#Paises

nacovid <- ncovid %>% filter(id=="LTU")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
ltu <- ggplot(nacovid, aes(nacovid$Daily_cases, nacovid$vaccines)) + geom_point() +theme_few() +
  labs(title="Lithuania \n(cor = -0.7626; CI = (-0.8603,-0.6108); p<0.0001)",x="",y="")
ltu

#Paises

nacovid <- ncovid %>% filter(id=="IND")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#grid.arrange(uk,us,can,rus,irl,ltu,top="Correlação entre vacinados e casos diários",nrow=2,ncol=3)


#owid data

data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#OS DADOS DIARIOS SAO OS MESMOS.
