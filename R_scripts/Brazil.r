#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
#install.packages("geobr")
#install.packages("spdep")
#install.packages("sf")
#install.packages("crul")
#install.packages("rgdal")
library(crul)
library(sf)
library(spdep)
library(geobr)
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
#geobr https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html

#geobr
datasets <- list_geobr()

print(datasets, n=21)
###################################
#analisarei a crime_rate
br <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
df_br <- br%>%group_by(date,state)%>%
  summarise_if(is.numeric,sum,na.rm=T)

df_br <- df_br %>% filter(state!="TOTAL")
########################################

#Mesorregioes#geo_br
mesos <- read_state(year=2020)
mesos$name_state

ggplot() +
  geom_sf(data=mesos,aes(fill=mesos$name_state), size=.15) +
  labs(subtitle="Mapa do Brasil", size=8,fill="Regiões") +
  theme_minimal() +theme(legend.position = "None")

#estados
mesos_sp <- dplyr::left_join(mesos, df_br, by = c("abbrev_state" = "state"))

#sf to sp
mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
centroids.df <- as.data.frame(coordinates(mesos_sp_sp))

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$newCases)), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Mortes por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()



ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furtos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_veic_habi))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubos de Veículos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()



ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  + scale_fill_viridis_c()+
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))



#moran using sp object
coor <- coordinates(mesos_sp_sp)

cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = mesos_sp_sp$name_intermediate)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(mesos_sp_sp, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(mesos_sp_sp), col='red', lwd=2, add=TRUE)#links
#mesos_sp_sp$soma_homi_100mil <- (mesos_sp_sp$soma_homi_100mil-min(mesos_sp_sp$soma_homi_100mil))/(max(mesos_sp_sp$soma_homi_100mil)-min(mesos_sp_sp$soma_homi_100mil))

#monthly
#death_pop
moran.plot(mesos_sp_sp$soma_homi_100mil, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$soma_homi_100mil,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$soma_homi_100mil,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$soma_homi_100mil, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$soma_homi_100mil- mean(mesos_sp_sp$soma_homi_100mil)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                    axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))



#furtos 100 mil
#death_pop
moran.plot(mesos_sp_sp$furto, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$furto,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$furto,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$furto, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$furto- mean(mesos_sp_sp$furto)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                    axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))



#roubos 100 mil
#death_pop
moran.plot(mesos_sp_sp$roubo, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$roubo,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$roubo,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$furto, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$roubo- mean(mesos_sp_sp$roubo)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                    axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))

#roubos 100 mil
#death_pop
moran.plot(mesos_sp_sp$furto_roubo_veic_habi, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$furto_roubo_veic_habi,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$furto_roubo_veic_habi,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$furto_roubo_veic_habi, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$furto_roubo_veic_habi- mean(mesos_sp_sp$furto_roubo_veic_habi)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                    axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))

#roubos 100 mil
#death_pop
moran.plot(mesos_sp_sp$furto_100mil_veic, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$furto_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$furto_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$furto_100mil_veic, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$furto_100mil_veic- mean(mesos_sp_sp$furto_100mil_veic)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                    axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))

#roubos 100 mil
#death_pop
moran.plot(mesos_sp_sp$roubo_100mil_veic, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$roubo_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$roubo_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$roubo_100mil_veic, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$roubo_100mil_veic- mean(mesos_sp_sp$roubo_100mil_veic)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","lightblue","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))

#roubos 100 mil
#death_pop
moran.plot(mesos_sp_sp$furto_roubo_100mil_veic, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$furto_roubo_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$furto_roubo_100mil_veic,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$furto_roubo_100mil_veic, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$furto_roubo_100mil_veic- mean(mesos_sp_sp$furto_roubo_100mil_veic)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","lightblue","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                                axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))
