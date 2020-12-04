install.packages("COVID19")
install.packages("tidyverse")
#install.packages("zoo")
#install.packages("sf")
#install.packages("maps")
#The package rnaturalearth also provides a map of countries of the entire world
#install.packages("rnaturalearth")
#install.packages('rnaturalearthdata')
#install.packages("rgeos")
#install.packages("gridExtra")
#install.packages("rgdal")
#install.packages("spdep")
library(rgeos)
library(sf)
library(zoo)#deal with dates
library(ggthemes)
library(tidyverse)
library(COVID19)
library(rnaturalearthdata)
library(rnaturalearth)
library(maps)
library(gridExtra)
library(rgdal)
library(spdep)
#install.packages("devtools")
library(devtools)
#devtools::install_github("benjak/scanstatistics", ref = "develop")
library(scanstatistics)

# Load map data
data(NM_map)
data(NM_geo)

# Plot map with labels at centroids
ggplot() + 
  geom_polygon(data = NM_map,
               mapping = aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") +
  geom_text(data = NM_geo, 
            mapping = aes(x = center_long, y = center_lat, label = county)) +
  ggtitle("Counties of New Mexico")

#dealing with data
data(NM_popcas)

counts <- NM_popcas %>% 
  filter(year >= 1986 & year < 1990) %>%
  df_to_matrix(time_col = "year", location_col = "county", value_col = "count")

library(sp)
library(magrittr)
?spDists()
# Remove Cibola since cases have been counted towards Valencia. Ideally, this
# should be accounted for when creating the zones.
zones <- NM_geo %>%
  filter(county != "cibola") %>%
  select(seat_long, seat_lat) %>%
  as.matrix %>%
  spDists(x = ., y = ., longlat = TRUE) %>%
  dist_to_knn(k = 15) %>%
  knn_zones

mod <- glm(count ~ offset(log(population)) + 1 + I(year - 1985),
           family = poisson(link = "log"),
           data = NM_popcas %>% filter(year < 1986))

ebp_baselines <- NM_popcas %>% 
  filter(year >= 1986 & year < 1990) %>%
  mutate(mu = predict(mod, newdata = ., type = "response")) %>%
  df_to_matrix(value_col = "mu")

set.seed(1)
poisson_result <- scan_eb_poisson(counts = counts, 
                                  zones = zones, 
                                  baselines = ebp_baselines,
                                  n_mcsim = 999)
print(poisson_result)

?scan_pb_poisson

scan_pb_poisson(counts,zones,n_mcsim = 999)

####################################3
#trying with covid

covidatag <- covid19(start ="2020/04/10" ,end ="2020/04/10" ,raw = F)

covidata <- covid19(start ="2020/04/01" ,end ="2020/04/10" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$deaths)

counts <- covi_pop %>% 
  df_to_matrix(time_col = "date", location_col = "county", value_col = "count")

counts
covi_geo <- na.omit(covi_geo)
zones <- covi_geo %>%
  select(long, lat) %>%
  as.matrix %>%
  spDists(x = ., y = ., longlat = TRUE) %>%
  dist_to_knn(k = 15) %>%
  knn_zones

z=scan_pb_poisson(counts,zones,n_mcsim = 999)
z

counties <- as.character(covidatag$administrative_area_level_1)
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)



# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)
