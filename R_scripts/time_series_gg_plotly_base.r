#install.packages("COVID19")
#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("gridExtra")
# https://www.neonscience.org/dc-time-series-plot-ggplot-r
#https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
library(COVID19)
library(tidyverse)
library(ggthemes)
library(scales)
library(gridExtra)

data <- covid19()#updated data
databr <- data %>% filter(administrative_area_level_1=="Brazil") 

options(scipen = 999)

p <- ggplot(databr, aes(x=date, y=confirmed)) +
  geom_line() + 
  xlab("Date")+ylab("Confirmed number of cases in brazil")+theme_stata()
p#updated number of cases plot
p+scale_x_date(date_labels = "%b")
p+scale_x_date(date_labels = "%Y %b %d")
p+scale_x_date(date_labels = "%m-%Y")
p+scale_x_date(date_breaks = "1 month", date_labels = "%m")
# only a few configurations

#standard plot
plot(databr$date,databr$confirmed,type = "line")

#two plots in the same grid
q <- ggplot(data, aes(x=date, y=confirmed)) +
  geom_line() + 
  xlab("Date")+ylab("Confirmed number of cases in the world")+theme_stata()
grid.arrange(p, q, ncol=1)#only growing :O

#using plotly, for interactive plots

#install.packages("plotly")
library(plotly)

fig <- plot_ly(x = databr$date, y = databr$confirmed, mode = 'lines', text = paste(databr$confirmed))

fig

