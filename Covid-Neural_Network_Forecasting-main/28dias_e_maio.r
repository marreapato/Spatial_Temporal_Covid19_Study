#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("gridExtra")
#install.packages("zoo")
#install.packages("forecast")
#install.packages('Metrics')
#install.packages('MLmetrics')
#install.packages("nnfor")
library(nnfor)
library(xts)
library(Metrics)
library(MLmetrics)
library(forecast)
library(zoo)
library(gridExtra)
library(COVID19)
library(tidyverse)
library(ggthemes)


#o modelo 5,1,1 é o melhor, vou usar os modelos do outro neste
covid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid$date <- as.Date(covid$date)

library(forecast)

covid <- covid %>% filter(covid$date<="2021-05-22")
#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)
#Dia 23 de maio terei que checar denovo
covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

#df
covid_std <- covid
#From DF to TS
world_daily_cases <- aggregate(covid_std["new_cases"], by=covid_std["date"], sum)
plot(world_daily_cases, type = "l")

daily_data_w <- world_daily_cases
#prevs
daily_data_w$date <- as.Date(as.character(daily_data_w$date))
#################
#longo

daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-56)),]
#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-55):nrow(daily_data_w)),]
##################################################
ts_less7 <-ts(daily_data_less_28,deltat = 1/365) 
fit <- Arima(ts_less7[,2],order = c(2, 1, 2),include.drift = T)
fc.c1 <- forecast(fit,h=56)

mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)
plot(fc.c1)

#longo prazo com sazonalidade

#minha serie
daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-56)),]

#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-55):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_28,frequency = 365.25/52) 

fit <- Arima(ts_less7[,2],order = c(2,0,3),seasonal = list(order=c(0,1,2),period=7),include.drift = T)
fc.c1 <- forecast(fit,h=56)
mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)

#longo prazo com sazonalidade 14 dias

#minha serie
daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-56)),]

#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-55):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_28,frequency = 365.25/26) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c1 <- forecast(fit,h=56)
mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)

#Para 31 de maio
covid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid$date <- as.Date(covid$date)
library(forecast)

covid <- covid %>% filter(covid$date<="2021-05-31")
#tratamento
#https://covid19datahub.io/articles/data.html
library(forecast)

#df_w <- data_w
covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

#df
covid_std <- covid
#From DF to TS
world_daily_cases <- aggregate(covid_std["new_cases"], by=covid_std["date"], sum)
plot(world_daily_cases, type = "l")

daily_data_w <- world_daily_cases
#prevs
daily_data_w$date <- as.Date(as.character(daily_data_w$date))

daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-31)),]
#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-30):nrow(daily_data_w)),]
##################################################
ts_less7 <-ts(daily_data_less_28,deltat = 1/365) 

##################################################

fit <- Arima(ts_less7[,2],order = c(2, 1, 2),include.drift = T)
fc.c1 <- forecast(fit,h=31)

plot(fc.c1)

fc.c1$mean

mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)

#com sazo

ts_less7 <-ts(daily_data_less_28,frequency = 7) 
fit <- Arima(ts_less7[,2],order = c(2,0,3),seasonal = list(order=c(0,1,2),period=7),include.drift = T)
fc.c1 <- forecast(fit,h=31)

plot(fc.c1)

fc.c1$mean

mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)


#com sazo2

ts_less7 <-ts(daily_data_less_28,frequency = 14) 
fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c1 <- forecast(fit,h=31)

plot(fc.c1)

fc.c1$mean

mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)

