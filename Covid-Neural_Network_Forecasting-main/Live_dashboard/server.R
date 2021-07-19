#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#read csv n funciona
library(shiny)
#install.packages('forecast', dependencies = TRUE)
library(forecast)
library(nnfor)
library(tidyverse)
library(Metrics)

data <- reactive({
    invalidateLater(4.32e+7/2,NULL)
    covid = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
}) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        covid <- data()
        covid$date <- as.Date(covid$date)
        
        
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
        #plot(world_daily_cases, type = "l")
        
        daily_data_w <- world_daily_cases
        
        validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]
        
        ##################################################
        ts_less7 <-ts(world_daily_cases,frequency = 14) 
        
        fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
        
        fc.c2 <- forecast(fit,h=7)
        
        set.seed(2)
        
        fit2 <- mlp(fc.c2$residuals,hd.auto.type = F,hd=c(5),reps = 20,lags = c(1:12),difforder = 1)
        
        fc.c1 <- forecast(fit2,h=7)
        
        fc.c2$mean <- fc.c1$mean+fc.c2$mean
        
        plot(fc.c2,main="Forecasts From MLP(14,5,1)-ARIMA(2,0,4)x(0,1,2)[14] Model")
        
        output$table <- renderDataTable(data.frame("1-day"=trunc(fc.c2$mean[1]),"2-days"=trunc(fc.c2$mean[2]),
                                                   "3-days"=trunc(fc.c2$mean[3]),"4-days"=trunc(fc.c2$mean[4]),
                                                   "5-days"=trunc(fc.c2$mean[5]),"6-days"=trunc(fc.c2$mean[6]),
                                                   "7-days"=trunc(fc.c2$mean[7]),"MASE"=mase(validation_7d$new_cases,fc.c2$mean)))
        
    })
    
    output$distPlot2 <- renderPlot({
        covid <- data()
        covid$date <- as.Date(covid$date)
        
        
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
        #plot(world_daily_cases, type = "l")
        
        daily_data_w <- world_daily_cases
        
        validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]
        
        ##################################################
        ts_less7 <-ts(world_daily_cases,frequency = 14) 
        
        fit <- Arima(ts_less7[,2],order = c(2,0,4),seasonal = list(order=c(0,1,2),period=14),include.drift = T)
        
        fc.c2 <- forecast(fit,h=7)
        
        #fit2 <- mlp(fc.c2$residuals,hd.auto.type = F,hd=c(5),reps = 20,lags = c(1:12),difforder = 1)
        
        #fc.c1 <- forecast(fit2,h=7)
        
        #fc.c2$mean <- fc.c1$mean+fc.c2$mean
        
        plot(fc.c2,main="Forecasts From ARIMA(2,0,4)x(0,1,2)[14] Model")
        
        output$table2 <- renderDataTable(data.frame("1-day"=trunc(fc.c2$mean[1]),"2-days"=trunc(fc.c2$mean[2]),
                                                   "3-days"=trunc(fc.c2$mean[3]),"4-days"=trunc(fc.c2$mean[4]),
                                                   "5-days"=trunc(fc.c2$mean[5]),"6-days"=trunc(fc.c2$mean[6]),
                                                   "7-days"=trunc(fc.c2$mean[7]),"MASE"=mase(validation_7d$new_cases,fc.c2$mean)))
        
    })
    
    
    
    output$distPlot3 <- renderPlot({
        covid <- data()
        covid$date <- as.Date(covid$date)
        
        
        
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
        #plot(world_daily_cases, type = "l")
        
        daily_data_w <- world_daily_cases
        
        validation_7d <- daily_data_w[c((nrow(daily_data_w)-6):nrow(daily_data_w)),]
        
        ##################################################
        ts_less7 <-ts(world_daily_cases,frequency = 14) 
        
        set.seed(2)
        
        fit <- mlp(ts_less7[,2],hd.auto.type = F,hd=c(5),reps = 20,lags = c(1:21),difforder = 1)
        
        fc.c2 <- forecast(fit,h=7)
        
        #fit2 <- mlp(fc.c2$residuals,hd.auto.type = F,hd=c(5),reps = 20,lags = c(1:12),difforder = 1)
        
        #fc.c1 <- forecast(fit2,h=7)
        
        #fc.c2$mean <- fc.c1$mean+fc.c2$mean
        
        plot(fc.c2,main="Forecasts From MLP(14,5,1) Model")
        
        output$table3 <- renderDataTable(data.frame("1-day"=trunc(fc.c2$mean[1]),"2-days"=trunc(fc.c2$mean[2]),
                                                    "3-days"=trunc(fc.c2$mean[3]),"4-days"=trunc(fc.c2$mean[4]),
                                                    "5-days"=trunc(fc.c2$mean[5]),"6-days"=trunc(fc.c2$mean[6]),
                                                    "7-days"=trunc(fc.c2$mean[7]),"MASE"=mase(validation_7d$new_cases,fc.c2$mean)))
        
    })
    
    
})
