#https://stats.stackexchange.com/questions/344089/arima-ann-hybrid-model-for-timeseries-forecasting

#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("forecast")
#install.packages('Metrics')
#install.packages('MLmetrics')
#install.packages("nnfor")
library(nnfor)
library(Metrics)
library(MLmetrics)
library(forecast)
library(tidyverse)
library(ggthemes)

#Com sazionalidade

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
set.seed(2)
ts_less7 <-ts(daily_data_less_7,frequency = 14) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c2 <- forecast(fit,h=7)
fc.c2$residuals

#####indo para o misto

(fit2 <- mlp(fc.c2$residuals,hd.auto.type = F,hd=c(5),reps = 20,lags = c(1:12),difforder = 1))
plot(fit2)
fc.c1 <- forecast(fit2,h=7)

fc.c2$mean <- fc.c1$mean+fc.c2$mean

plot(fc.c2)

mase(validation_7d$new_cases,fc.c2$mean)
smape(validation_7d$new_cases,fc.c2$mean)
R2_Score(fc.c2$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c2$mean)

#######################################################


#prevs
#medio prazo

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
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-14)),]

#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-13):nrow(daily_data_w)),]

##################################################
set.seed(2)
ts_less7 <-ts(daily_data_less_7,frequency = 14) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c2 <- forecast(fit,h=14)
fc.c2$residuals

#####indo para o misto

(fit2 <- mlp(fc.c2$residuals,model = fit2))
fc.c1 <- forecast(fit2,h=14)

fc.c2$mean <- fc.c1$mean+fc.c2$mean

plot(fc.c2)

mase(validation_7d$new_cases,fc.c2$mean)
smape(validation_7d$new_cases,fc.c2$mean)
R2_Score(fc.c2$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c2$mean)




#######################################################


#prevs
#longo prazo

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
daily_data_less_7 <- daily_data_w[c(1:(nrow(daily_data_w)-28)),]

#Validation
validation_7d <- daily_data_w[c((nrow(daily_data_w)-27):nrow(daily_data_w)),]

##################################################
set.seed(2)
ts_less7 <-ts(daily_data_less_7,frequency = 14) 

fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
fc.c2 <- forecast(fit,h=28)
fc.c2$residuals

#####indo para o misto

(fit2 <- mlp(fc.c2$residuals,model=fit2))
fc.c1 <- forecast(fit2,h=28)

fc.c2$mean <- fc.c1$mean+fc.c2$mean

plot(fc.c2)

mase(validation_7d$new_cases,fc.c2$mean)
smape(validation_7d$new_cases,fc.c2$mean)
R2_Score(fc.c2$mean,validation_7d$new_cases)
rmse(validation_7d$new_cases,fc.c2$mean)

