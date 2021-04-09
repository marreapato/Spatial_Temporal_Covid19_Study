#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("gridExtra")
library(gridExtra)
library(COVID19)
library(tidyverse)
library(ggthemes)
covid <- covid19(end="2021-04-03",raw=F)
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
uk <- ggplot(nacovid) + geom_point(na.rm=T, aes(Daily_cases, vaccines))+
  labs(title="United Kingdom \n(cor = -0.8373; CI = (-0.8853, -0.7716))",x="Vaccinated people",y="Number of Cases")
uk
#Paises
#par(mfrow=c(4,2))
#dev.off()
nacovid <- ncovid %>% filter(id=="USA")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)

us <- ggplot(nacovid) + geom_point(na.rm=T, aes(Daily_cases, vaccines))+
  labs(title="United States \n(cor = -0.8013; CI = (-0.8607, -0.7203))",x="Vaccinated people",y="Number of Cases")
us
#Paises
grid.arrange(uk,us,ncol=2)

znacovid <- ncovid %>% filter(id=="RUS")
znacovid <- znacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(znacovid$vaccines,znacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
rus <- ggplot(znacovid) + geom_point(na.rm=T,aes(znacovid$Daily_cases, znacovid$vaccines)) +
  labs(title="Russia \n(cor = -0.8687; CI = (-0.9082, -0.8138))",x="Vaccinated people",y="Number of Cases")
rus
grid.arrange(uk,us,rus,ncol=2)

#Paises

nacovidl <- ncovid %>% filter(id=="ISR")
nacovidl <- nacovidl %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovidl$vaccines,nacovidl$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
can <- ggplot(nacovidl) + geom_point(na.rm=T, aes(nacovidl$Daily_cases, nacovidl$vaccines)) +
  labs(title="Israel \n(cor = -0.6374; CI = (-0.7384, -0.5085))",x="Vaccinated people",y="Number of Cases")
can
grid.arrange(rus,us,can,uk,ncol=2)

#Paises

Fnacovid <- ncovid %>% filter(id=="IRL")#IRELAND
Fnacovid <- Fnacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(Fnacovid$vaccines,Fnacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
irl <- ggplot(Fnacovid) + geom_point(na.rm=T,aes(Fnacovid$Daily_cases, Fnacovid$vaccines)) +
  labs(title="Ireland \n(cor = -0.6479; CI = (-0.7518, -0.5126))",x="Vaccinated people",y="Number of Cases")
irl
grid.arrange(uk,us,irl,rus,can,ncol=3,nrow=2)

#Paises

nacovid <- ncovid %>% filter(id=="ARE")
nacovid <- nacovid %>% filter(vaccines>=1)

#Vacina efeito de longo tempo
cor.test(nacovid$vaccines,nacovid$Daily_cases)
plot(nacovid$Daily_cases~nacovid$vaccines)
ltu <- ggplot(nacovid) + geom_point(na.rm=T,aes(nacovid$Daily_cases, nacovid$vaccines)) +
  labs(title="United Arab Emirates \n(cor = -0.6299; CI = (-0.7409, -0.4853))",x="Vaccinated people",y="Number of Cases")
ltu

grid.arrange(uk,us,can,rus,irl,ltu,nrow=2,ncol=3)



#owid data

data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#OS DADOS DIARIOS SAO OS MESMOS.
