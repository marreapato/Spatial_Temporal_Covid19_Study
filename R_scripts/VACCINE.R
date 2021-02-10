install.packages("COVID19")
install.packages("tidyverse")
library(COVID19)
library(tidyverse)

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

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="USA")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="RUS")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="CAN")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="IRL")#IRELAND
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="LTU")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

#Paises

nacovid <- ncovid %>% filter(id=="LVA")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)




