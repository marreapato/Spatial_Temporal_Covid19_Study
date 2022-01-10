library(zoo)
library(plotly)
library(bslib)
library(RPostgres)
library(RPostgreSQL)
library(tidyverse)
library(DBI)
library(ggthemes)
library(Metrics)
library(MLmetrics)
library(forecast)
library(zoo)
library(nnfor)
library(tsfgrnn)
library(car)
library(lubridate)
library(chron)
library(data.table)
library(treemapify)
library(plotly)
library(airportr)
#install.packages("googleAnalyticsR")
library(googleAuthR)
library(googleAnalyticsR)
library(geobr)
library(sf)
library(spdep)
library(tmap)
#install.packages("tmap")
#install.packages("chron")
#db_name <- 'defaultdb'  #provide the name of your db

#host_db <- '157.230.188.218'  #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  

#db_port <-5432  # or any other port specified by the DBA

#db_user <- 'postgres'  

#db_password <- 'a1b2c3'

#postgres_driver <- dbDriver("PostgreSQL");

#postgres_conn <- dbConnect(drv =PostgreSQL(), dbname = db_name,host = host_db , port = 5432,user = db_user, password = db_password)

#teste <- dbListTables(postgres_conn)   #list all the tables 

#query_res <- dbGetQuery(postgres_conn, "SELECT * FROM pg_tables");

#query_res <- dbGetQuery(postgres_conn, "SELECT * FROM pg_views")

#Estes dois tem que ser rodados juntos
#dbGetQuery(postgres_conn, "SET search_path to zupper")
#dados_ads1 <- dbReadTable(postgres_conn, "zupper_google_ads")
#dbDisconnect(postgres_conn)#disconnect from database


#BUSCAS
#You must be logged into your Google Analytics account on your web browser
ga_auth()

choose <- ga_meta()
segs <- ga_segment_list()
## segment Ids and name:
segs[,c("name","id","definition")]
## choose the v3 segment
segment_for_call <- "gaid::-1"

## make the v3 segment object in the v4 segment object:
?segment_ga4

seg_obj <- segment_ga4("sessions", segment_id = segment_for_call)

#seg_vec <- segment_vector_simple(c(seg_obj,seg_obj2))

#Use the Google Analytics Management API to see a list of Google Analytics accounts you have access to
my_accounts <- google_analytics(viewId = 244642724,date_range = c("2022-01-01","2022-01-09"),metrics = c("ga:totalEvents"),
                                dimensions = c("ga:dateHourMinute","ga:eventLabel","ga:region","ga:country"),max = -1,segments = seg_obj,anti_sample = T)

##################33
dados <- my_accounts


dados$date<- as.Date(dados$dateHourMinute, "%Y%m%d")


estados <- dados %>% filter(country=="Brazil") %>% group_by(region) %>% summarize(pesquisas=n())

estados$region <- gsub("State of ","",estados$region)

estados <- estados[!estados$region=="(not set)",]

#####################################

#Mesorregioes#geo_br#apenas rode se necessario
mesos <- read_state(year=2017,simplified = T)

mesos$name_state <- iconv(mesos$name_state,from="UTF-8",to="ASCII//TRANSLIT")

mesos <- mesos[,c(3,6)]

estados$region[estados$region=="Federal District"] <- "Distrito Federal"

estados$region <- toupper(estados$region)
mesos$name_state <- toupper(mesos$name_state)


#estados
table(estados$region)
table(mesos$name_state)

estados <- as_tibble(estados)

mesos_sp <- left_join(mesos,estados, by = c("name_state" = "region"))
mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
centroids.df <- as.data.frame(coordinates(mesos_sp_sp))

mesos_sp$pesquisas <- as.numeric(mesos_sp$pesquisas)

(mp <- ggplot() +
    geom_sf(data=mesos_sp,aes(fill=mesos_sp$pesquisas), size=.15) +
    labs(subtitle="Mapa de Pesquisas por UF", size=8,fill="Pesquisa",caption = "De 2022-01-01 até 2022-01-09") +
    theme_minimal()  +
    geom_text(aes(label = mesos_sp$pesquisas, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                      axis.text.y=element_blank(),
                                                                                                                      axis.ticks.y=element_blank()))




estados <- dados %>% filter(country=="Brazil") %>% group_by(region,date) %>% summarize(pesquisas=n())


coor <- coordinates(mesos_sp_sp)
cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = mesos_sp_sp$pesquisas)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(mesos_sp_sp, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(mesos_sp_sp), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#death_pop
moran.plot(mesos_sp_sp$pesquisas, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$pesquisas,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$pesquisas,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated


#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$pesquisas, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                           ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi", at=summary(mesos_sp_sp$lmi), col.regions=brewer.pal(5,"RdBu"), main="Local Moran's")
(kc3=spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = ""))
?spplot

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- mesos_sp_sp$pesquisas - mean(mesos_sp_sp$pesquisas)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(mesos_sp_sp,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="")
box()  
legend("bottomleft", legend = c("Nenhum","AA","AB","BA","BB"),
       fill=colors,bty="n")
