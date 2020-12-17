install.packages("COVID19")
install.packages("tidyverse")
library(tidyverse)
library(COVID19)
#install.packages("devtools")
library(devtools)
#devtools::install_github("benjak/scanstatistics", ref = "develop")
library(scanstatistics)
#install.packages("sp")
#install.packages("magrittr")
library(sp)
library(magrittr)
#trying with covid
#January was non-significant
covidatag <- covid19(start ="2020/01/31" ,end ="2020/01/31" ,raw = F)

covidata <- covid19(start ="2020/01/01" ,end ="2020/01/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

#FEBRUARY

covidatag <- covid19(start ="2020/02/29" ,end ="2020/02/29" ,raw = F)

covidata <- covid19(start ="2020/02/01" ,end ="2020/02/29" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

feb=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#March

covidatag <- covid19(start ="2020/03/31" ,end ="2020/03/31" ,raw = F)

covidata <- covid19(start ="2020/03/01" ,end ="2020/03/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

march=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#April

covidatag <- covid19(start ="2020/04/30" ,end ="2020/04/30" ,raw = F)

covidata <- covid19(start ="2020/04/01" ,end ="2020/04/30" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

april=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#May

covidatag <- covid19(start ="2020/05/31" ,end ="2020/05/31" ,raw = F)

covidata <- covid19(start ="2020/05/01" ,end ="2020/05/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

may=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#June

covidatag <- covid19(start ="2020/06/30" ,end ="2020/06/30" ,raw = F)

covidata <- covid19(start ="2020/06/01" ,end ="2020/06/30" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

june=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#July

covidatag <- covid19(start ="2020/07/31" ,end ="2020/07/31" ,raw = F)

covidata <- covid19(start ="2020/07/01" ,end ="2020/07/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

july=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#August

covidatag <- covid19(start ="2020/08/31" ,end ="2020/08/31" ,raw = F)

covidata <- covid19(start ="2020/08/01" ,end ="2020/08/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

august=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])


#September

covidatag <- covid19(start ="2020/09/30" ,end ="2020/09/30" ,raw = F)

covidata <- covid19(start ="2020/09/01" ,end ="2020/09/30" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

sept=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])


#october

covidatag <- covid19(start ="2020/10/31" ,end ="2020/10/31" ,raw = F)

covidata <- covid19(start ="2020/10/01" ,end ="2020/10/31" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

oct=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])

#november

covidatag <- covid19(start ="2020/11/30" ,end ="2020/11/30" ,raw = F)

covidata <- covid19(start ="2020/11/01" ,end ="2020/11/30" ,raw = F)

covi_geo <- data.frame("county"=covidatag$id,"lat"=covidatag$latitude,"long"=covidatag$longitude)

covi_pop <- data.frame("date"=covidata$date,"county"=covidata$id,"population"=covidata$population,"count"=covidata$confirmed)

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
?scan_pb_poisson
counties <- as.character(covidatag$administrative_area_level_1)
counties <- counties[-193]#Virgin Islands, U.S.
# Calculate scores and add column with county names
county_scores <- score_locations(z, zones)
#county_scores <- county_scores[-c(195:199),]
county_scores %<>% mutate(counties=covidatag$administrative_area_level_1)

?score_locations
?top_clusters
# Create a table for plotting

top5 <- top_clusters(z, zones, k = 5, overlapping = FALSE)
top5
zones[[1623]]
county_scores$counties[101]

# Find the counties corresponding to the spatial zones of the 5 clusters.
top5_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5 %<>% mutate(counties = top5_counties)

nov=data.frame("Duração"=top5$duration[1],"Risk"=top5$relrisk_out[1],"MC_pvalue"=top5$MC_pvalue[1],"Counties"=top5$counties[[1]])
