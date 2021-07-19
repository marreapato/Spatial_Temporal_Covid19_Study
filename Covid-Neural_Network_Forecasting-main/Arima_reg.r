#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("gridExtra")
#install.packages("zoo")
#install.packages("forecast")
#install.packages('Metrics')
#install.packages('MLmetrics')
library(vars)
library(xts)
library(Metrics)
library(MLmetrics)
library(forecast)
library(zoo)
library(gridExtra)
library(COVID19)
library(tidyverse)
library(ggthemes)
#data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid <- covid19(end = "2021-04-25",raw=T)
#recomendou fechar (1) e n fechou (0) serao 0
#categorias = https://covid19datahub.io/articles/doc/data.html

#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

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

covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

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

#cases
ncovid=ncovid %>%
  mutate('roll_mean'=rollapply(ncovid$Daily_cases,7,mean,align='right',fill=NA))

ncovid$Daily_cases[ncovid$Daily_cases<0] <- ncovid$roll_mean[ncovid$Daily_cases<0] 

#deaths
ncovid=ncovid %>%
  mutate('roll_mean_d'=rollapply(ncovid$Daily_deaths,7,mean,align='right',fill=NA))

ncovid$Daily_deaths[ncovid$Daily_deaths<0] <- ncovid$roll_mean[ncovid$Daily_deaths<0] 

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
home<- aggregate(covid_std["stay_home_restrictions"], by=covid_std["date"], sum)
home$stay_home_restrictions <- home$stay_home_restrictions/199

school_closing<- aggregate(covid_std["school_closing"], by=covid_std["date"], sum)
school_closing$school_closing <- school_closing$school_closing/199

workplace_closing<- aggregate(covid_std["workplace_closing"], by=covid_std["date"], sum)
workplace_closing$workplace_closing <- workplace_closing$workplace_closing/199

cancel_events<- aggregate(covid_std["cancel_events"], by=covid_std["date"], sum)
cancel_events$cancel_events<- cancel_events$cancel_events/199

gatherings_restrictions<- aggregate(covid_std["gatherings_restrictions"], by=covid_std["date"], sum)
gatherings_restrictions$gatherings_restrictions<- gatherings_restrictions$gatherings_restrictions/199

transport_closing<- aggregate(covid_std["transport_closing"], by=covid_std["date"], sum)
transport_closing$transport_closing<- transport_closing$transport_closing/199

internal_movement_restrictions<- aggregate(covid_std["internal_movement_restrictions"], by=covid_std["date"], sum)
internal_movement_restrictions$internal_movement_restrictions<- internal_movement_restrictions$internal_movement_restrictions/199

international_movement_restrictions<- aggregate(covid_std["international_movement_restrictions"], by=covid_std["date"], sum)
international_movement_restrictions$international_movement_restrictions<- international_movement_restrictions$international_movement_restrictions/199

information_campaigns<- aggregate(covid_std["information_campaigns"], by=covid_std["date"], sum)
information_campaigns$information_campaigns<- information_campaigns$information_campaigns/199

testing_policy<- aggregate(covid_std["testing_policy"], by=covid_std["date"], sum)
testing_policy$testing_policy<- testing_policy$testing_policy/199

contact_tracing<- aggregate(covid_std["contact_tracing"], by=covid_std["date"], sum)
contact_tracing$contact_tracing<- contact_tracing$contact_tracing/199

daily_data_w <- cbind(world_daily_cases,"home"=home$stay_home_restrictions,"school_closing"=school_closing$school_closing,
                      "workplace_closing"=workplace_closing$workplace_closing,"cancel_events"= cancel_events$cancel_events,"gatherings_restrictions"=gatherings_restrictions$gatherings_restrictions,
                      "transport_closing"=transport_closing$transport_closing,"internal_movement_restrictions"=internal_movement_restrictions$internal_movement_restrictions,
                      "international_movement_restrictions"= international_movement_restrictions$international_movement_restrictions,"information_campaigns"=information_campaigns$information_campaigns,
                      "testing_policy"= testing_policy$testing_policy, "contact_tracing"=contact_tracing$contact_tracing)








#################### ARIMA(p,d,q)x(P,D,Q) ####################

#minha serie
daily_data_w$date <- as.Date(as.character(daily_data_w$date))
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-6)),]
daily_data_less_14 <- daily_data_w[c(1:(nrow(daily_data_w)-13)),]
daily_data_less_21 <- daily_data_w[c(1:(nrow(daily_data_w)-20)),]
daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-27)),]

#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]
validation_14d <- daily_data_w[c((nrow(daily_data_less_7)-6):nrow(daily_data_less_7)),]
validation_21d <- daily_data_w[c((nrow(daily_data_less_14)-6):nrow(daily_data_less_14)),]
validation_28d <- daily_data_w[c((nrow(daily_data_less_21)-6):nrow(daily_data_less_21)),]
valid_list <- list(validation_7d,validation_14d,validation_21d,validation_28d)

#modelos arima nao sazional
#ts_world <- ts(daily_data_w,deltat = 1/365)

##################################################
ts_less7 <-ts(daily_data_less_7,deltat = 1/365) 
ts_less14 <-ts(daily_data_less_14,deltat = 1/365) 
ts_less21 <-ts(daily_data_less_21,deltat = 1/365) 
ts_less28 <-ts(daily_data_less_28,deltat = 1/365) 
ts_list <- list(ts_less7,ts_less14,ts_less21,ts_less28)

?lag
models=list(NULL)

nlags=c(NULL)

errors <- list(error1=c(),error2=c(),error3=c(),error4=c())

reg_matrices <- list()

for(z in 1:4){
  lags <- c(1:245)
  i=1
  bestmodel=FALSE
  fit1=TRUE
  while(bestmodel==FALSE){
    
    #if(fit1=TRUE){
    fit <- auto.arima(diff(ts_list[[z]][,2],lag = lags[i]), xreg = as.matrix(diff(ts_list[[z]][,3:13],lag=lags[i])),seasonal = F)
    
    fc.c2 <- forecast(ts_list[[z]][,3], h = 7)
    fc.c3 <- forecast(ts_list[[z]][,4], h = 7)
    fc.c4 <- forecast(ts_list[[z]][,5], h = 7)
    fc.c5 <- forecast(ts_list[[z]][,6], h = 7)
    fc.c6 <- forecast(ts_list[[z]][,7], h = 7)
    fc.c7 <- forecast(ts_list[[z]][,8], h = 7)
    fc.c8 <- forecast(ts_list[[z]][,9], h = 7)
    fc.c9 <- forecast(ts_list[[z]][,10], h = 7)
    fc.c10 <- forecast(ts_list[[z]][,11], h = 7)
    fc.c11 <- forecast(ts_list[[z]][,12], h = 7)
    fc.c12 <- forecast(ts_list[[z]][,13], h = 7)
    
    
    newxreg <- as.matrix(cbind(fc.c2$mean, fc.c3$mean,fc.c4$mean,fc.c5$mean,fc.c6$mean,
                               fc.c7$mean,fc.c8$mean,fc.c9$mean,fc.c10$mean,fc.c11$mean,
                               fc.c12$mean))
    reg_matrices[[z]] <- newxreg
    
    fc.c1 <- forecast(fit, xreg = newxreg,h=7)
    
    if(mase(valid_list[[z]]$Daily_cases,fc.c1$mean)<1||(i>30&&smape(valid_list[[z]]$Daily_cases,fc.c1$mean)<0.20)){
      bestmodel=TRUE
      
      errors[[z]][1]=mase(valid_list[[z]]$Daily_cases,fc.c1$mean)
      errors[[z]][2]=smape(valid_list[[z]]$Daily_cases,fc.c1$mean)
      
    }
    
    if(z==1&mase(valid_list[[z]]$Daily_cases,fc.c1$mean)<2){
      bestmodel=TRUE
      
      errors[[z]][1]=mase(valid_list[[z]]$Daily_cases,fc.c1$mean)
      errors[[z]][2]=smape(valid_list[[z]]$Daily_cases,fc.c1$mean)
      
    }
    
    i=i+1
    
    nlags[z]=i
    
    print(i)
  }
  
  models[z]=fit
  
}

nlags#find best model based in lag differenciated bvalues

errors

reg_matrices

models

z=2

fit <- auto.arima(diff(ts_list[[z]][,2],lag = nlags[2]), xreg = as.matrix(diff(ts_list[[z]][,3:13],lag=nlags[2])),seasonal = F)
fc.c2 <- forecast(ts_list[[z]][,3], h = 7)
fc.c3 <- forecast(ts_list[[z]][,4], h = 7)
fc.c4 <- forecast(ts_list[[z]][,5], h = 7)
fc.c5 <- forecast(ts_list[[z]][,6], h = 7)
fc.c6 <- forecast(ts_list[[z]][,7], h = 7)
fc.c7 <- forecast(ts_list[[z]][,8], h = 7)
fc.c8 <- forecast(ts_list[[z]][,9], h = 7)
fc.c9 <- forecast(ts_list[[z]][,10], h = 7)
fc.c10 <- forecast(ts_list[[z]][,11], h = 7)
fc.c11 <- forecast(ts_list[[z]][,12], h = 7)
fc.c12 <- forecast(ts_list[[z]][,13], h = 7)


newxreg <- as.matrix(cbind(fc.c2$mean, fc.c3$mean,fc.c4$mean,fc.c5$mean,fc.c6$mean,
                           fc.c7$mean,fc.c8$mean,fc.c9$mean,fc.c10$mean,fc.c11$mean,
                           fc.c12$mean))

fc.c1 <- forecast(fit, xreg = newxreg,h=7)

mase(valid_list[[z]]$Daily_cases,fc.c1$mean)
smape(valid_list[[z]]$Daily_cases,fc.c1$mean)

ts.plot(ts_list[[z]][,2],fc.c1$mean)

plot(ts_list[[z]][,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)
