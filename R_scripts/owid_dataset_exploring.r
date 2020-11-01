#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("plotly")
library(plotly)
#install.packages("zoo")
library(zoo)

#updated
data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

data_br <- data %>% filter(location=="Brazil")

options(scipen = 999)

p <- ggplot(data_br, aes(x=date, y=new_cases)) +
  geom_line() + 
  xlab("Date")+ylab("Confirmed number of cases in brazil (daily)")+theme_stata()
p#updated number of cases plot
p+scale_x_date(date_breaks = "7 day", date_labels = "%m-%d")

#plotting with plotly

fig <- plot_ly(x =data_br$date, y = data_br$new_cases, mode = 'lines', text = paste("cases =",data_br$new_cases,sep = " "))

fig <- fig %>% layout(
  title = 'Daily number of cases of Covid19 in Brazil <br>Source:<a href="https://github.com/owid/covid-19-data/tree/master/public/data">Our World in Data</a>'
)

fig

#us data

data_us <- data %>% filter(location=="United States")

options(scipen = 999)

p <- ggplot(data_us, aes(x=date, y=new_cases)) +
  geom_line() + 
  xlab("Date")+ylab("Confirmed number of cases in the Us (daily)")+theme_stata()
p#updated number of cases plot
p+scale_x_date(date_breaks = "7 day", date_labels = "%m-%d")

#mapping a few variables
#plotting rworld map
#install.packages("rworldmap")
#tip:https://stackoverflow.com/questions/22625119/choropleth-world-map
library(rworldmap)
#another way

datan <- data %>% filter(date=="2020-09-02")

gtd        <- datan
gtd.recent <- gtd
#gtd.recent <- aggregate(total_cases_per_million~iso_code,gtd.recent,sum)

#join data to a map
gtdMap <- joinCountryData2Map( gtd.recent, 
                               nameJoinColumn="iso_code", 
                               joinCode="NAME" )

#plot the map
#?mapCountryData
options(scipen=999)#getting rid of scientific notation

mapCountryData( gtdMap, 
                nameColumnToPlot='total_cases_per_million', 
                catMethod="fixedWidth", 
                numCats=35,colourPalette = "heat",mapTitle = "Total cases of covid19 per million on 09/03")

mapCountryData( gtdMap, 
                nameColumnToPlot='male_smokers', 
                catMethod="fixedWidth", 
                numCats=35,colourPalette = "heat",mapTitle = "Percentage of male smokers")

mapCountryData( gtdMap, 
                nameColumnToPlot='female_smokers', 
                catMethod="fixedWidth", 
                numCats=35,colourPalette = "heat",mapTitle = "Percentage of female smokers")

mapCountryData( gtdMap, 
                nameColumnToPlot='total_deaths_per_million', 
                catMethod="fixedWidth", 
                numCats=35,colourPalette = "heat",mapTitle = "Total deaths per million on 09/02")

#better plots
cor.test(datan$gdp_per_capita,datan$total_cases_per_million)

ggplot(datan, aes(datan$gdp_per_capita,datan$total_cases_per_million)) + 
  geom_point() +
  theme_light()+stat_smooth(method="lm",se=F)

plot(rollmean(data_br$new_cases,7),type="l")
?rollmean
###############################################################

#Roll means
data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")


#asia
data_as <- data %>% filter(continent=="Asia")

asia_mean=rollmean(data_as$new_cases,7)
plot(rollmean(data_as$new_cases,7),type="l")

#df_dados <- data_as
#preciso somar

df_dados <- data_as%>%group_by(data_as$date)%>%
  summarise_if(is.numeric,sum)

df_dados=df_dados %>%
  mutate('roll_mean'=rollapply(df_dados$new_cases,7,mean,align='right',fill=NA))

#grouping
df_dados=df_dados %>%
  gather(c("new_cases","roll_mean"),key="Séries", value="Valor")

gm=ggplot(df_dados,aes(x=`data_as$date`,y=Valor,fill=Séries, colour=Séries))+
  geom_line(size=1.1)+
  labs(x="",y="Número de óbitos")
gm

dados2 <- data.frame('data'=df_dados$`data_as$date`,'serie'=df_dados$Séries,'val'=df_dados$Valor)

dados2 <- na.omit(dados2)

ggplot(data = dados2, mapping = aes(x = data, y =val,colour=serie, fill = serie)) +
  geom_line(stat = "identity")+
scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")

###

#asia
data_eu <- data %>% filter(continent=="Europe")

eu_mean=rollmean(data_eu$new_cases,7)
plot(rollmean(data_eu$new_cases,7),type="l")

#df_dados <- data_as
#preciso somar

df_dados <- data_eu%>%group_by(data_eu$date)%>%
  summarise_if(is.numeric,sum)

df_dados=df_dados %>%
  mutate('roll_mean'=rollapply(df_dados$new_cases,7,mean,align='right',fill=NA))

#grouping
df_dados=df_dados %>%
  gather(c("new_cases","roll_mean"),key="Séries", value="Valor")

gm=ggplot(df_dados,aes(x=df_dados$`data_eu$date`,y=Valor,fill=Séries, colour=Séries))+
  geom_line(size=1.1)+
  labs(x="",y="Número de óbitos")
gm

dados2 <- data.frame('data'=df_dados$`data_eu$date`,'serie'=df_dados$Séries,'val'=df_dados$Valor)

dados2 <- na.omit(dados2)

ggplot(data = dados2, mapping = aes(x = data, y =val,colour=serie, fill = serie)) +
  geom_line(stat = "identity")+
  scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")

