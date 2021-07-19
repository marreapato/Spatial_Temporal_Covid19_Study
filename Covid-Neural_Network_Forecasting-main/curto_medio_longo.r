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

#o modelo 5,1,1 é o melhor, vou usar os modelos do outro neste
covid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid$date <- as.Date(covid$date)
library(forecast)

covid <- covid %>% filter(covid$date<="2021-04-25")
#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)

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
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-7)),]
#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]
##################################################
ts_less7 <-ts(daily_data_less_7,deltat = 1/365) 
fit <- Arima(ts_less7[,2],order = c(2, 1, 2),include.drift = T)
fc.c1 <- forecast(fit,h=7)
mase(validation_7d$new_cases,fc.c1$mean)
smape(validation_7d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c1$mean)
plot(fc.c1)

#14 dias
#minha serie
daily_data_less_14 <- daily_data_w[c(1:(nrow(daily_data_w)-14)),]
#Validation
validation_14d <- daily_data_w[c((nrow(daily_data_w)-13):nrow(daily_data_w)),]
##################################################
ts_less7 <-ts(daily_data_less_14,deltat = 1/365) 
fit <- Arima(ts_less7[,2],order = c(2, 1, 2),include.drift = T)
fc.c1 <- forecast(fit,h=14)

mase(validation_14d$new_cases,fc.c1$mean)
smape(validation_14d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_14d$new_cases)
rmse(validation_14d$new_cases,fc.c1$mean)
plot(fc.c1)
#################
#longo

daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-28)),]
#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-27):nrow(daily_data_w)),]
##################################################
ts_less7 <-ts(daily_data_less_28,deltat = 1/365) 
fit <- Arima(ts_less7[,2],order = c(2, 1, 2),include.drift = T)
fc.c1 <- forecast(fit,h=28)

mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)
plot(fc.c1)


#Curto prazo

#minha serie
daily_data_w$date <- as.Date(as.character(daily_data_w$date))
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-7)),]

#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]


##################################################
ts_less7 <-ts(daily_data_less_7,frequency = 365.25/52) 
fit <- Arima(ts_less7[,2],order = c(2,0,3),seasonal = list(order=c(0,1,2),period=7))
fc.c1 <- forecast(fit,h=7)

mase(validation_7d$new_cases,fc.c1$mean)
smape(validation_7d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)
#######################################3
#medio prazo

#minha serie
daily_data_less_14 <- daily_data_w[c(1:(nrow(daily_data_w)-14)),]

#Validation
validation_14d <- daily_data_w[c((nrow(daily_data_w)-13):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_14,frequency = 365.25/52) 

fit <- Arima(ts_less7[,2],order = c(2,0,3),seasonal = list(order=c(0,1,2),period=7))

fc.c1 <- forecast(fit,h=14)
mase(validation_14d$new_cases,fc.c1$mean)
smape(validation_14d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_14d$new_cases)
rmse(validation_14d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)

#longo prazo

#minha serie
daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-28)),]

#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-27):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_28,frequency = 365.25/52) 

fit <- Arima(ts_less7[,2],order = c(2,0,3),seasonal = list(order=c(0,1,2),period=7))
fc.c1 <- forecast(fit,h=28)
mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)

#############################################################################

#Com sazionalidade

#o modelo 5,1,1 é o melhor, vou usar os modelos do outro neste
covid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid$date <- as.Date(covid$date)

covid <- covid %>% filter(covid$date<="2021-04-25")
#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)

covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

#df
covid_std <- covid
#From DF to TS
world_daily_cases <- aggregate(covid_std["new_cases"], by=covid_std["date"], sum)
plot(world_daily_cases, type = "l")

daily_data_w <- world_daily_cases


#prevs
#Curto prazo

covid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid$date <- as.Date(covid$date)

covid <- covid %>% filter(covid$date<="2021-04-25")
#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)

covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

#df
covid_std <- covid
#From DF to TS
world_daily_cases <- aggregate(covid_std["new_cases"], by=covid_std["date"], sum)

daily_data_w <- world_daily_cases


#minha serie
daily_data_w$date <- as.Date(as.character(daily_data_w$date))
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-7)),]

#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_7,frequency = 365.25/26) 
fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c1 <- forecast(fit,h=7)

mase(validation_7d$new_cases,fc.c1$mean)
smape(validation_7d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)
#######################################3
#medio prazo

#minha serie
daily_data_less_14 <- daily_data_w[c(1:(nrow(daily_data_w)-14)),]

#Validation
validation_14d <- daily_data_w[c((nrow(daily_data_w)-13):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_14,frequency = 365.25/26) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)

fc.c1 <- forecast(fit,h=14)
mase(validation_14d$new_cases,fc.c1$mean)
smape(validation_14d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_14d$new_cases)
rmse(validation_14d$new_cases,fc.c1$mean)

plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)

#longo prazo

#minha serie
daily_data_less_28 <- daily_data_w[c(1:(nrow(daily_data_w)-28)),]

#Validation
validation_28d <- daily_data_w[c((nrow(daily_data_w)-27):nrow(daily_data_w)),]

##################################################
ts_less7 <-ts(daily_data_less_28,frequency = 365.25/26) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c1 <- forecast(fit,h=28)
mase(validation_28d$new_cases,fc.c1$mean)
smape(validation_28d$new_cases,fc.c1$mean)
R2_Score(fc.c1$mean,validation_28d$new_cases)
rmse(validation_28d$new_cases,fc.c1$mean)
plot(ts_less7[,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)

plot(fc.c1)
