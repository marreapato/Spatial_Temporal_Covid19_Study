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

covid <- covid %>% filter(covid$date<="2021-04-26")
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

models=list(NULL)
##########################################################################################################################
#https://www.sciencedirect.com/science/article/pii/S1684118220300980

#número de casos modelo 1
fit <- auto.arima(ts_list[[1]][,2],seasonal = F)
models[[1]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[1]]$new_cases,fc.c1$mean)
smape(valid_list[[1]]$new_cases,fc.c1$mean)

?Arima

normal <- Arima(ts_list[[1]][,2],order = c(5, 1, 1),include.drift = T)
fc.c1 <- forecast(normal,h=7)
mase(valid_list[[1]]$new_cases,fc.c1$mean)
smape(valid_list[[1]]$new_cases,fc.c1$mean)

ts.plot(ts_list[[1]][,2],fc.c1$mean)

plot(ts_list[[1]][,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)
##############################################################################


#model n 2
fit <- auto.arima(ts_list[[2]][,2],seasonal = F)
models[[2]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[2]]$new_cases,fc.c1$mean)
smape(valid_list[[2]]$new_cases,fc.c1$mean)

#############################################################################


#model n 3
fit <- auto.arima(ts_list[[3]][,2],seasonal = F)
models[[3]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[3]]$new_cases,fc.c1$mean)
smape(valid_list[[3]]$new_cases,fc.c1$mean)


#############################################################################

#model n 4
fit <- auto.arima(ts_list[[4]][,2],seasonal = F)
models[[4]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[4]]$new_cases,fc.c1$mean)
smape(valid_list[[4]]$new_cases,fc.c1$mean)

############################################################################################

errors <- list(list(),list(),list(),list())
av_mod <- list(list(),list(),list(),list())

#best model
models
for(z in 1:4){
if(z==1){  
  for(j in 1:4){
    fit <- Arima(ts_list[[j]][,2],order = c(5,1,1),include.drift = T)
    fc.c1 <- forecast(fit,h=7)
errors[[z]][["mase"]][j]= mase(valid_list[[j]]$new_cases,fc.c1$mean)
errors[[z]][["smape"]][j]= smape(valid_list[[j]]$new_cases,fc.c1$mean)
errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
  }
  
}else if(z==2){
  for(j in 1:4){
    
    fit <- Arima(ts_list[[j]][,2],order = c(5,1,1),include.drift = T)
    fc.c1 <- forecast(fit,h=7)
    errors[[z]][["mase"]][j]= mase(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["smape"]][j]= smape(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean) 
    errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
    av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
    av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
    av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
  }
}else if(z==3){
  for(j in 1:4){
    
    fit <- Arima(ts_list[[j]][,2],order = c(1,1,5))
    fc.c1 <- forecast(fit,h=7)
    errors[[z]][["mase"]][j]= mase(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["smape"]][j]= smape(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
    av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
    av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
    av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
  }
}else if(z==4){
  for(j in 1:4){
    
    fit <- Arima(ts_list[[j]][,2],order = c(2,1,2),include.drift = T)#serie boa neste
    fc.c1 <- forecast(fit,h=7)
    errors[[z]][["mase"]][j]= mase(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["smape"]][j]= smape(valid_list[[j]]$new_cases,fc.c1$mean)
    errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
  
    errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$new_cases,fc.c1$mean)
  av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
  }
}
  
  
}
#modelo mais estável em 28 dias modelo 1
av_mod
























##################################################3
###############3333
############################3
#############################################3
########################################################

#data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
covid <- covid19(end = "2021-04-25",raw=T)
#recomendou fechar (1) e n fechou (0) serao 0
#categorias = https://covid19datahub.io/articles/doc/data.html

#tratamento
#https://covid19datahub.io/articles/data.html

#df_w <- data_w

#covid <- covid19(start = "2021-01-01",end= Sys.Date(),raw=F)

covid <- covid%>%group_by(date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

#pegando daily cases, todos paises comecam em 2020-01-01
case_daily=NULL
deaths_daily=NULL
vaccines_daily=NULL
#pgando por país
for(i in 1:nrow(covid)){
  if(i>1){
    case_daily[i] <- covid$confirmed[i]-covid$confirmed[i-1]
    deaths_daily[i] <- covid$deaths[i]-covid$deaths[i-1]
    vaccines_daily[i] <- covid$vaccines[i]-covid$vaccines[i-1]
  }else{ 
    
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
  mutate('roll_mean'=rollapply(ncovid$Daily_cases,8,mean,align='right',fill=NA))

ncovid$Daily_cases[ncovid$Daily_cases<0] <- ncovid$roll_mean[ncovid$Daily_cases<0] 

#cases
#ncovid=ncovid %>%
 # mutate('roll_mean'=rollapply(ncovid$Daily_cases,31,mean,align='right',fill=NA))

#ncovid$Daily_cases[ncovid$Daily_cases<0] <- ncovid$roll_mean[ncovid$Daily_cases<0] 

covid_std <- ncovid
#From DF to TS
world_daily_cases <- aggregate(covid_std["Daily_cases"], by=covid_std["date"], sum)
plot(world_daily_cases, type = "l")

daily_data_w <- world_daily_cases


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

models=list(NULL)

##########################################################################################################################
#https://www.sciencedirect.com/science/article/pii/S1684118220300980

#número de casos modelo 1
fit <- auto.arima(ts_list[[1]][,2],seasonal = F)
models[[1]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[1]]$Daily_cases,fc.c1$mean)
smape(valid_list[[1]]$Daily_cases,fc.c1$mean)

?Arima

normal <- Arima(ts_list[[1]][,2],order = c(5, 1, 1),include.drift = T)
fc.c1 <- forecast(normal,h=7)
mase(valid_list[[1]]$Daily_cases,fc.c1$mean)
smape(valid_list[[1]]$Daily_cases,fc.c1$mean)

ts.plot(ts_list[[1]][,2],fc.c1$mean)

plot(ts_list[[1]][,2], col="blue", main="drift Forecast", type='l')
lines(fc.c1$mean, col="red", lwd=2)
##############################################################################


#model n 2
fit <- auto.arima(ts_list[[2]][,2],seasonal = F)
models[[2]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[2]]$Daily_cases,fc.c1$mean)
smape(valid_list[[2]]$Daily_cases,fc.c1$mean)

#############################################################################


#model n 3
fit <- auto.arima(ts_list[[3]][,2],seasonal = F)
models[[3]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[3]]$Daily_cases,fc.c1$mean)
smape(valid_list[[3]]$Daily_cases,fc.c1$mean)


#############################################################################

#model n 4
fit <- auto.arima(ts_list[[4]][,2],seasonal = F)
models[[4]] <- fit
fc.c1 <- forecast(fit,h=7)

plot(fc.c1)
qqnorm(fc.c1$residuals)
plot(fc.c1$residuals)

mase(valid_list[[4]]$Daily_cases,fc.c1$mean)
smape(valid_list[[4]]$Daily_cases,fc.c1$mean)

############################################################################################

errors <- list(list(),list(),list(),list())
av_mod <- list(list(),list(),list(),list())

#best model
models
for(z in 1:4){
  if(z==1){  
    for(j in 1:4){
      fit <- Arima(ts_list[[j]][,2],order = c(5,1,1),include.drift = T)
      fc.c1 <- forecast(fit,h=7)
      errors[[z]][["mase"]][j]= mase(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["smape"]][j]= smape(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
      av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
      av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
      av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
    }
    
  }else if(z==2){
    for(j in 1:4){
      
      fit <- Arima(ts_list[[j]][,2],order = c(5,1,1),include.drift = T)
      fc.c1 <- forecast(fit,h=7)
      errors[[z]][["mase"]][j]= mase(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["smape"]][j]= smape(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean) 
      errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
      av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
      av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
      av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
    }
  }else if(z==3){
    for(j in 1:4){
      
      fit <- Arima(ts_list[[j]][,2],order = c(1,1,5))
      fc.c1 <- forecast(fit,h=7)
      errors[[z]][["mase"]][j]= mase(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["smape"]][j]= smape(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
      av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
      av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
      av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
    }
  }else if(z==4){
    for(j in 1:4){
      
      fit <- Arima(ts_list[[j]][,2],order = c(2,1,2),include.drift = T)
      fc.c1 <- forecast(fit,h=7)
      errors[[z]][["mase"]][j]= mase(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["smape"]][j]= smape(valid_list[[j]]$Daily_cases,fc.c1$mean)
      errors[[z]][["rmse"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      
      errors[[z]][["r2"]][j]= rmse(valid_list[[j]]$Daily_cases,fc.c1$mean)
      av_mod[[z]][["mase"]][j]=mean(errors[[z]][["mase"]])
      av_mod[[z]][["smape"]][j]=mean(errors[[z]][["smape"]])
      av_mod[[z]][["rmse"]][j]=mean(errors[[z]][["rmse"]])
      av_mod[[z]][["r2"]][j]=mean(errors[[z]][["r2"]])
    }
  }
  
  
}
#modelo mais estável em 28 dias modelo 1
av_mod
errors


























