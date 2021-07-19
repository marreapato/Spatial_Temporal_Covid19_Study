#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("gridExtra")
#install.packages("zoo")
#install.packages("forecast")
#install.packages('Metrics')
#install.packages('MLmetrics')
library(Metrics)
library(MLmetrics)
library(forecast)
library(zoo)
library(gridExtra)
library(COVID19)
library(tidyverse)
library(ggthemes)
covid <- covid19(raw=F)
#recomendou fechar (1) e n fechou (0) serao 0
#categorias = https://covid19datahub.io/articles/doc/data.html

#binarias
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
#pgando por país
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
min(case_daily)
min(deaths_daily)#valores negativos

ncovid <- cbind(covid,"Daily_cases"=case_daily,"Daily_deaths"=deaths_daily,"Daily_vaccines"=vaccines_daily)

cases_mean <- aggregate(ncovid["Daily_cases"], by=ncovid["id"], mean)
deaths_mean <- aggregate(ncovid["Daily_deaths"], by=ncovid["id"], mean)

#negativos para media
min(ncovid$Daily_cases)
#trocando negativos por media
#casos
for(i in 1:nrow(ncovid)){
  
  if(ncovid$Daily_cases[i]<0){
    
    for(j in 1:nrow(cases_mean)){
      
      if(ncovid$id[i]==cases_mean$id[j]){
        
        ncovid$Daily_cases[i] = cases_mean$Daily_cases[j] 
        
      }
      
    }
    
  }
}

ncovid$Daily_cases[ncovid$Daily_cases==cases_mean$Daily_cases]
table(ncovid$Daily_cases==cases_mean$Daily_cases)
min(ncovid$Daily_cases)

#mortes
for(i in 1:nrow(ncovid)){
  
  if(ncovid$Daily_deaths[i]<0){
    
    for(j in 1:nrow(deaths_mean)){
      
      if(ncovid$id[i]==deaths_mean$id[j]){
        
        ncovid$Daily_deaths[i] = deaths_mean$Daily_deaths[j] 
        
      }
      
    }
    
  }
}

ncovid$Daily_deaths[ncovid$Daily_deaths==deaths_mean$Daily_cases]
table(ncovid$Daily_deaths==cases_mean$Daily_deaths)
min(ncovid$Daily_deaths)

#padronizando

zconfirmedaily_stand=c(NULL)
zdeathsdaily_stand=c(NULL)

for(j in 1:nrow(ncovid)){
  
  zconfirmedaily_stand[j] <- (ncovid$Daily_cases[j]-min(ncovid$Daily_cases))/(max(ncovid$Daily_cases)-min(ncovid$Daily_cases))
  zdeathsdaily_stand[j] <- (ncovid$Daily_deaths[j]-min(ncovid$Daily_deaths))/(max(ncovid$Daily_deaths)-min(ncovid$Daily_deaths))
  
  
}

#df
covid_std <- cbind(ncovid,"std_daily_cases"=zconfirmedaily_stand,"std_daily_deaths"=zdeathsdaily_stand)

#From DF to TS
world_daily_cases <- aggregate(covid_std["Daily_cases"], by=covid_std["date"], sum)
plot(world_daily_cases, type = "l")
#dfs de restrições
restr<- aggregate(covid_std["stay_home_restrictions"], by=covid_std["date"], sum)
restr$stay_home_restrictions <- restr$stay_home_restrictions/199

?ts
#deltat seasonality semanal
#Métodos de forecasting: https://otexts.com/fpp2/simple-methods.html

#################### ARIMA(p,d,q)x(P,D,Q) ####################

#minha serie
ts_world <- zoo(world_daily_cases$Daily_cases, order.by=as.Date(as.character(world_daily_cases$date), format='%Y-%m-%d'))
ts_world

##########################################################################################
#modelos arima nao sazional

#serie menos c
ts_w <-ts(ts_world[-c((length(ts_world)-29):length(ts_world))],deltat = 1/365)
plot(ts_w)

#serie mais c
ts_world <- ts(ts_world,deltat = 1/365)
plot(ts_world)
validation=ts_world[c((length(ts_world)-29):length(ts_world))]


fit <- auto.arima(ts_w, seasonal=FALSE)
fit %>% forecast(h=30) %>% autoplot()#include=80

#########################################################################################3

#Modelos arima sazional
#serie menos c
ts_w <-ts(ts_world[-c((length(ts_world)-29):length(ts_world))],deltat = 1/7)
plot(ts_w)

#serie mais c
ts_world <- ts(ts_world,deltat = 1/7)
plot(ts_world)
validation=ts_world[c((length(ts_world)-29):length(ts_world))]


fit <- auto.arima(ts_w, seasonal=T)
fit %>% forecast(h=30) %>% autoplot()#include=80

model <- fit %>% forecast(h=30)
rmse(model$mean,validation)#root mean squared error
MAPE(model$mean, validation) * 100#mean absolute percentage error

#source("C:/Gecy/Atividades - UFBA/Aulas/MAT185/Modelo ARIMA.txt",print.eval=TRUE)
y = ts_w    		## serie utilizada
lag.entrada = c(1:15)   ## variaveis de entrada (lags de y)
prev = 30		## numero de passos a frente para previsao

l = max(lag.entrada)	## numero maximo de defasagens
n = length(y) 		## numero de observacoes

################### Construindo a matriz de entrada Y #######################
## Constroi a matriz de entrada para os lags especificados antes
Y=matrix(nrow=length(y),ncol=length(lag.entrada))

for(m in 1:length(lag.entrada))
{
  e=lag.entrada[m]
  for(i in 1:length(y))
  {
    if(i<=e)
      for(a in 1:e)
      {
        Y[a,m]=0
      }
    else Y[i,m]=y[i-e]
  }
}
########################## Ajuste do modelo ARIMA ###########################
y.teste = y[(n-prev+1):n]			## dados de teste
X.teste = Y[(n-prev+1):n,]

y = y[(l+1):(n-prev)]
X = Y[(l+1):(n-prev),]  			## matriz modelo apos eliminacao dos zeros

################ Selecao do modelo ARIMA(p,1,q)x(0,1,1) #####################
fit <- auto.arima(y)
plot(forecast(fit,h=prev))
?auto.arima

fit <- auto.arima(y,xreg=X,seasonal = T,biasadj = F)
plot(forecast(fit,h=prev,xreg = X.teste))
model <- forecast(fit,h=prev,xreg = X.teste)
rmse(model$mean,validation)#root mean squared error
MAPE(model$mean, validation) * 100#mean absolute percentage error
