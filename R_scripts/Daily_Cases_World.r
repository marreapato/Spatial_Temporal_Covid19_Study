#Roll mean
#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("gridExtra")
#média movel do mundo e dos continentes, número de mortes
library(tidyverse)
library(gridExtra)
library(zoo) # pacote com função para médias móveis

#Roll means #updated data
data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
options(scipen = 999)
#asia


#FIRST GET DAILY CASES IN THE WORLD DF_W


#data <- data %>% filter(date<="2020-12-31")

#World
data_w <- data 

df_w <- data_w%>%group_by(data_w$date)%>%
  summarise_if(is.numeric,sum,na.rm=T)

df_w=df_w %>%
  mutate('roll_mean'=rollapply(new_cases,7,mean,align='right',fill=NA))

#grouping
df_w=df_w %>%
  gather(c("new_cases","roll_mean"),key="Séries", value="Valor")


df_w <- data.frame('data'=df_w$`data_w$date`,'serie'=df_w$Séries,'val'=df_w$Valor)
#15 july
wo=ggplot(data = df_w, mapping = aes(x = data, y =val,colour=serie)) +
  geom_line(stat = "identity",size=1.1)+theme(legend.position = "none")+
  labs(title="Moving Average of Daily Covid-19 Cases In The World.",x="",y="Number Of Cases",colour="Series")+ 
  scale_color_manual(labels = c("Daily Cases", "Moving Average"),values =c("green","red") )+
  scale_x_date(date_breaks = "4 month",date_labels = "%m/%Y")#+ ylim(0, max(data$new_cases,na.rm = T))#  scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")

wo
