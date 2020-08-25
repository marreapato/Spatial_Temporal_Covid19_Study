#install.packages("COVID19")
#installing the covid19 package which contains data about the pandemic
library(COVID19)
#https://covid19datahub.io/articles/doc/data.html
#Variables explanation
#Cran: https://cran.r-project.org/web/packages/COVID19/COVID19.pdf
library(tidyverse)
#February 2020 covid19 data
worldcovid=covid19(start = '2020-03-31',end='2020-03-31')

#plotting rworld map
#install.packages("rworldmap")
#tip:https://stackoverflow.com/questions/22625119/choropleth-world-map
library(rworldmap)
newmap <- getMap(resolution = "coarse")
?getMap
plot(newmap,asp = 1)
points(worldcovid$longitude, worldcovid$latitude, col = "red", cex = .6)

#another way
gtd        <- worldcovid
gtd.recent <- gtd
gtd.recent <- aggregate(deaths~id,gtd.recent,sum)

#join data to a map
gtdMap <- joinCountryData2Map( gtd.recent, 
                               nameJoinColumn="id", 
                               joinCode="NAME" )

#plot the map
?mapCountryData
options(scipen=999)#getting rid of scientific notation

mapCountryData( gtdMap, 
                nameColumnToPlot='deaths', 
                catMethod="fixedWidth", 
                numCats=35,colourPalette = "heat",mapTitle = "Deaths by covid19 in february")

#changing the legend

wmap <- mapCountryData( gtdMap, 
                        nameColumnToPlot='deaths', 
                        catMethod="fixedWidth", 
                        numCats=10,colourPalette = "heat",mapTitle = "Deaths by covid19 in february")

do.call( addMapLegend
         , c( wmap
              , legendLabels="all"
              , legendWidth=2.2
              , legendIntervals="data"
              , legendMar = 3 ) )


?addMapLegend


##################################################################################################
#categorical method


#creating a user defined colour palette

op <- palette(c('green','yellow','orange','red'))

#find quartile breaks
cutVector <- quantile(gtdMap[["deaths"]],na.rm=TRUE)

cutVector <- cutVector[-1]#making breaks unique

#classify the data to a factor
gtdMap[["deaths"]] <- cut(gtdMap[["deaths"]], cutVector, include.lowest=TRUE)

#rename the categories
levels(gtdMap[["deaths"]]) <- c('low', 'med', 'high', 'vhigh')

#mapping
mapCountryData(gtdMap, nameColumnToPlot="deaths"
                     , catMethod='categorical'
                     , mapTitle='Deaths per country'
                     , colourPalette='palette'
                     , oceanCol='lightblue'
                     , missingCountryCol='white',borderCol = "black")

#################################################################################
#zooming in


mapCountryData(gtdMap, nameColumnToPlot="deaths"
               , catMethod='categorical'
               , mapTitle='Deaths per country in eurasia in february'
               , colourPalette='palette'
               , oceanCol='lightblue'
               , missingCountryCol='white',mapRegion='Eurasia',borderCol = "black")

######################################################################################





