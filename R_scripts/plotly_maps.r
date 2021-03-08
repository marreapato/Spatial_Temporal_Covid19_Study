#Probably the easiest way to do it

#useful to read later: https://plotly.com/r/map-subplots-and-small-multiples/

#install.packages("plotly")
library(plotly)
#install.packages("COVID19")
library(COVID19)

df <- covid19(start = "2020/08/16",end = "2020/08/16")

plot_ly(df, type='choropleth', locations=df$id, z=df$testing_policy, text=df$administrative_area_level_1, colorscale=~testing_policy)


#plotly includes data from Natural Earth

fig <- plot_ly(df, type='choropleth', locations=df$id, z=df$deaths, text=df$administrative_area_level_1, colorscale="Blues")

fig <- fig %>% colorbar(title = 'Number of deaths')#naming the colorbar

fig <- fig %>% layout(
  title = 'Cumulative number of deaths by Covid19 on August 16-2020 <br>Source:<a href="https://covid19datahub.io/">COVID-19 Data Hub</a>'
)

fig


##################################################
#more advanced options

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options

?layout

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

?plot_geo

fig <- plot_geo(df)

fig <- fig %>% add_trace(
  z = ~tests, color = ~tests, colors = 'Blues',
  text = ~administrative_area_level_1, locations = ~id, marker = list(line = l)
)

fig <- fig %>% colorbar(title = 'Tests', tickprefix = '')

fig <- fig %>% layout(
  title = 'Cumulative number of tests for Covid19 on August 16-2020 <br>Source:<a href="https://covid19datahub.io/">COVID-19 Data Hub</a>',
  geo = g
)

fig

?plot_ly

df <- covid19(start = Sys.Date(),end = Sys.Date(),raw = F)

#plotly includes data from Natural Earth

fig <- plot_ly(df, type='choropleth', locations=df$id, z=df$vaccines, text=paste("Cases =",df$confirmed,"\nVaccines =",df$vaccines,"\nDeaths =",df$deaths,sep = " "), colorscale="Red")

fig <- fig %>% colorbar(title = 'Number of Vaccines')#naming the colorbar

fig <- fig %>% layout(
  title = paste('Number of vaccinated people until',Sys.Date(),'<br>Source:<a href="https://covid19datahub.io/">COVID-19 Data Hub</a>',sep = ' ')
)

fig
