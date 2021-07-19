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
#covid <- covid19(end="2021-02-12",raw=F)
#27218 <- one
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

covid_std <- cbind(ncovid,"std_daily_cases"=zconfirmedaily_stand,"std_daily_deaths"=zdeathsdaily_stand)

#From DF to TS

world_daily_cases <- aggregate(covid_std["Daily_cases"], by=covid_std["date"], sum)

plot(world_daily_cases, type = "l")

#world_daily_cases <- world_daily_cases %>% filter(date<"2021-03-06")

#minha serie
ts_w <- zoo(world_daily_cases$Daily_cases, order.by=as.Date(as.character(world_daily_cases$date), format='%Y-%m-%d'))
ts_w
ts_world <- zoo(world_daily_cases$Daily_cases, order.by=as.Date(as.character(world_daily_cases$date), format='%Y-%m-%d'))
ts_world
#serie menos 100
ts_w <-ts(ts_world[-c((length(ts_world)-100):length(ts_world))])

plot(ts_w)
class(ts_w)
class(ts(ts_w))

#serie mais 101
ts_world <- ts(ts_world)
plot(ts_world)
#predict 101 values
#Métodos de forecasting: https://otexts.com/fpp2/simple-methods.html

#começando com simples
#mean forecasting
model=meanf(ts_w,h=101,level = 0.95)

autoplot(ts_w) +
  autolayer(meanf(ts_w,h=101),
            series="Mean", PI=FALSE)

model$lower[model$lower<0] <- 0

plot(model)#interessante
validation=ts_world[c((length(ts_world)-100):length(ts_world))]

rmse(model$mean,validation)#root mean squared error
MAPE(model$mean, validation) * 100#mean absolute percentage error

plot(ts_world, col="blue", main="Forecast", type='l')
lines(model$mean, col="red", lwd=2)

#naïve #https://towardsdatascience.com/a-guide-to-forecasting-in-r-6b0c9638c261
#simple naïve, or random walk forecast
model_naive=naive(ts_w,h=101,level=0.95)
model_naive$lower[model_naive$lower<0] <- 0
plot(model_naive)
rmse(model_naive$mean,validation)#root mean squared error
MAPE(model_naive$mean, validation) * 100#mean absolute percentage error

plot(ts_world, col="blue", main="Seasonal Naive Forecast", type='l')
lines(model_naive$mean, col="red", lwd=2)

#seasonal naïve, but i do not have seasonal data
?snaive
model_snaive=snaive(ts_w,h=length(validation),level=0.95)
model_snaive$lower[model_snaive$lower<0] <- 0
plot(model_snaive)
rmse(model_snaive$mean,validation)#root mean squared error
MAPE(model_snaive$mean, validation) * 100#mean absolute percentage error

plot(ts_world, col="blue", main="Seasonal Naive Forecast", type='l')
lines(model_snaive$mean, col="red", lwd=2)


#drift method
model_drift=rwf(ts_w,h=length(validation),level=0.95,drift = T)
model_drift$lower[model_drift$lower<0] <- 0
plot(model_drift)
rmse(model_drift$mean,validation)#root mean squared error
MAPE(model_drift$mean, validation) * 100#mean absolute percentage error

plot(ts_world, col="blue", main="drift Forecast", type='l')
lines(model_drift$mean, col="red", lwd=2)

#methods https://otexts.com/fpp2/simple-methods.html
autoplot(ts_world) +
  autolayer(model,
            series="Mean", PI=FALSE) +
  autolayer(model_naive,
            series="Naïve", PI=FALSE) +
  autolayer(model_naive,
            series="Seasonal naïve", PI=FALSE) +
  autolayer(model_drift,            
            series="drift rwf", PI=FALSE)+
  ggtitle("Forecasts") +
  xlab("Ndays") + ylab("Cases") +
  guides(colour=guide_legend(title="Forecast"))

