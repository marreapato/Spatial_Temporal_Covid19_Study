#install.packages("tidyverse")
#install.packages("COVID19")
library(COVID19)
library(tidyverse)
owid <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
owid_jan <- owid %>% filter(date=="2020-01-31")
owid_feb <- owid %>% filter(date=="2020-02-29")
owid_mar <- owid %>% filter(date=="2020-03-31")
owid_ap <- owid %>% filter(date=="2020-04-30")
owid_may <- owid %>% filter(date=="2020-05-31")
owid_jun <- owid %>% filter(date=="2020-06-30")
owid_jul <- owid %>% filter(date=="2020-07-31")
owid_ag <- owid %>% filter(date=="2020-08-31")
owid_sep <- owid %>% filter(date=="2020-09-30")
owid_oct <- owid %>% filter(date=="2020-10-31")
owid_nov <- owid %>% filter(date=="2020-11-30")
owid_dec <- owid %>% filter(date=="2020-12-31")
owid_jan21 <- owid %>% filter(date=="2021-01-31")
owid_feb21 <- owid %>% filter(date=="2021-02-28")

owid_list=list(jan20=owid_jan,feb20=owid_feb,march20=owid_mar,april20=owid_ap,may20=owid_may,june20=owid_jun,july=owid_jul,
               ag=owid_ag,sep=owid_sep,oct=owid_oct,no=owid_nov,de=owid_dec,jan21=owid_jan21,feb21=owid_feb21)



#saving datasets
jan <-covid19(start ="2020-01-31" ,end ="2020-01-31",raw = F ) 
feb <-covid19(start ="2020-02-29" ,end ="2020-02-29",raw = F ) 
march <- covid19(start ="2020-03-31" ,end ="2020-03-31",raw = F ) 
april <-covid19(start ="2020-04-30" ,end ="2020-04-30",raw = F )  
may <- covid19(start ="2020-05-31" ,end ="2020-05-31" ,raw = F) 
june <- covid19(start ="2020-06-30" ,end ="2020-06-30",raw = F ) 
july <- covid19(start ="2020-07-31" ,end ="2020-07-31",raw = F ) 
ag <- covid19(start ="2020-08-31" ,end ="2020-08-31",raw = F ) 
sep <- covid19(start ="2020-09-30" ,end ="2020-09-30",raw = F ) 
oct <- covid19(start ="2020-10-31" ,end ="2020-10-31",raw = F ) 
nov <-covid19(start ="2020-11-30" ,end ="2020-11-30",raw = F ) 
dec <-covid19(start ="2020-12-31" ,end ="2020-12-31",raw = F ) 
jan21<-covid19(start ="2021-01-31" ,end ="2021-01-31",raw = F ) 
feb21<-covid19(start ="2021-02-28" ,end ="2021-02-28",raw = F ) 
#list of datasets
datasets <- list(jan20=jan,feb20=feb,march20=march,april20=april,may20=may,june20=june,july=july,ag20=ag,sep20=sep,oc20=oct,nov20=nov,dec20=dec,jan21=jan21,feb21=feb21)

#renaming a few cells in the datasets
for(i in 1:length(datasets)){
  
  datasets[[i]]$school_closing <- gsub(0, 0, datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(1, 1, datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(2, 1, datasets[[i]]$school_closing)
  datasets[[i]]$school_closing <- gsub(3, 1, datasets[[i]]$school_closing)
  
  datasets[[i]]$workplace_closing <- gsub(0,0, datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(1, 1, datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(2, 1, datasets[[i]]$workplace_closing)
  datasets[[i]]$workplace_closing <- gsub(3, 1, datasets[[i]]$workplace_closing)
  
  datasets[[i]]$cancel_events<- gsub(0, 0, datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(1, 1, datasets[[i]]$cancel_events)
  datasets[[i]]$cancel_events <- gsub(2, 1, datasets[[i]]$cancel_events)
  
  datasets[[i]]$transport_closing<- gsub(0,0, datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(1, 1, datasets[[i]]$transport_closing)
  datasets[[i]]$transport_closing<- gsub(2, 1, datasets[[i]]$transport_closing)
  
  datasets[[i]]$stay_home_restrictions<- gsub(0, 0, datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(1, 1, datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(2, 1, datasets[[i]]$stay_home_restrictions)
  datasets[[i]]$stay_home_restrictions<- gsub(3, 1, datasets[[i]]$stay_home_restrictions)
  
  datasets[[i]]$internal_movement_restrictions<- gsub(0, 0, datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(1, 1, datasets[[i]]$internal_movement_restrictions)
  datasets[[i]]$internal_movement_restrictions<- gsub(2, 1, datasets[[i]]$internal_movement_restrictions)
  
  datasets[[i]]$international_movement_restrictions<- gsub(0, 0, datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(1, 1, datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(2, 1, datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(3, 1, datasets[[i]]$international_movement_restrictions)
  datasets[[i]]$international_movement_restrictions<- gsub(4, 1, datasets[[i]]$international_movement_restrictions)
  
  names(datasets[[i]])[names(datasets[[i]]) == "iso_alpha_3"] <- "iso_code"
  
  
}


for(i in 1:length(datasets)){
  datasets[[i]]<-merge(owid_list[[i]],datasets[[i]],by="iso_code")
}

#School  closures mar world, europe, sa, na
continents=list(w=c(sc=list(),work=list()),eu=c(sc=list(),work=list()),sa=c(sc=list(),work=list()),na=c(sc=list(),work=list()))
for(i in 1:length(datasets)){
  
  continents$w$sc[[i]]=data.frame(table(datasets[[i]]$school_closing))
  
  continents$eu$sc[[i]]=as.data.frame(table(datasets[[i]]$school_closing[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$sc[[i]]=data.frame(table(datasets[[i]]$school_closing[datasets[[i]]$continent=="South America"]))
  
  continents$na$sc[[i]]=data.frame(table(datasets[[i]]$school_closing[datasets[[i]]$continent=="North America"]))
  
  continents$w$work[[i]]=data.frame(table(datasets[[i]]$workplace_closing))
  
  continents$eu$work[[i]]=data.frame(table(datasets[[i]]$workplace_closing[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$work[[i]]=data.frame(table(datasets[[i]]$workplace_closing[datasets[[i]]$continent=="South America"]))
  
  continents$na$work[[i]]=data.frame(table(datasets[[i]]$workplace_closing[datasets[[i]]$continent=="North America"]))
  
  continents$w$events[[i]]=data.frame(table(datasets[[i]]$cancel_events))
  
  continents$eu$events[[i]]=data.frame(table(datasets[[i]]$cancel_events[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$events[[i]]=data.frame(table(datasets[[i]]$cancel_events[datasets[[i]]$continent=="South America"]))
  
  continents$na$events[[i]]=data.frame(table(datasets[[i]]$cancel_events[datasets[[i]]$continent=="North America"]))
  
  #########
  
  continents$w$transp[[i]]=data.frame(table(datasets[[i]]$transport_closing))
  
  continents$eu$transp[[i]]=data.frame(table(datasets[[i]]$transport_closing[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$transp[[i]]=data.frame(table(datasets[[i]]$transport_closing[datasets[[i]]$continent=="South America"]))
  
  continents$na$transp[[i]]=data.frame(table(datasets[[i]]$transport_closing[datasets[[i]]$continent=="North America"]))
  
  #########
  
  continents$w$home[[i]]=data.frame(table(datasets[[i]]$stay_home_restrictions))
  
  continents$eu$home[[i]]=data.frame(table(datasets[[i]]$stay_home_restrictions[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$home[[i]]=data.frame(table(datasets[[i]]$stay_home_restrictions[datasets[[i]]$continent=="South America"]))
  
  continents$na$home[[i]]=data.frame(table(datasets[[i]]$stay_home_restrictions[datasets[[i]]$continent=="North America"]))
  
  #########
  
  continents$w$internal[[i]]=data.frame(table(datasets[[i]]$internal_movement_restrictions))
  
  continents$eu$internal[[i]]=data.frame(table(datasets[[i]]$internal_movement_restrictions[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$internal[[i]]=data.frame(table(datasets[[i]]$internal_movement_restrictions[datasets[[i]]$continent=="South America"]))
  
  continents$na$internal[[i]]=data.frame(table(datasets[[i]]$internal_movement_restrictions[datasets[[i]]$continent=="North America"]))
  
  #########
  
  continents$w$international[[i]]=data.frame(table(datasets[[i]]$international_movement_restrictions))
  
  continents$eu$international[[i]]=data.frame(table(datasets[[i]]$international_movement_restrictions[datasets[[i]]$continent=="Europe"]))
  
  continents$sa$international[[i]]=data.frame(table(datasets[[i]]$international_movement_restrictions[datasets[[i]]$continent=="South America"]))
  
  continents$na$international[[i]]=data.frame(table(datasets[[i]]$international_movement_restrictions[datasets[[i]]$continent=="North America"]))
  
}

continents$w$international[[3]]#october
school = as.vector(NA)
work = as.vector(NA)
events = as.vector(NA)
transp = as.vector(NA)
home = as.vector(NA)
internal = as.vector(NA)
international = as.vector(NA)
place = as.vector(NA)
date = c("01-2020","02-2020","03-2020","04-2020","05-2020","06-2020","07-2020","08-2020","09-2020","10-2020","11-2020","12-2020","01-2021","02-2021")
#building table world
for(i in 1:length(continents$w$sc)){
  
 school[i] = continents$w$sc[[i]]$Freq[[2]]
 work[i] = continents$w$work[[i]]$Freq[[2]]
 events[i] = continents$w$events[[i]]$Freq[[2]]
 transp[i] = continents$w$transp[[i]]$Freq[[2]]
 home[i] = continents$w$home[[i]]$Freq[[2]]
 internal[i] = continents$w$internal[[i]]$Freq[[2]]
 international[i] = continents$w$international[[i]]$Freq[[2]]
 place[i] = "world"
}

world = data.frame("date"=date,"school"=school,"work"=work,"events"=events,"transp"=transp,
                   "home"=home,"internal"=internal,"international"=international,"location"=place)

continents$na$international[[12]]

#south america
#school
continents$sa$sc[[1]]
continents$sa$sc[[2]]
continents$sa$sc[[14]]
#work
continents$sa$work
#events
continents$sa$events
#transports
continents$sa$transp
#home
continents$sa$home
#internal
continents$sa$internal
#international
continents$sa$international

#n of countries
table(datasets$june20$continent)

school = c(0,0,rep(12,times = 12))
work = c(0,0,11,11,11,12,11,11,12,12,12,12,11,11)
events = c(0,0,12,12,12,11,11,12,12,12,12,12,11,11)
transp = c(0,0,8,10,10,11,11,10,8,8,8,6,8,8)
home = c(0,0,12,12,12,12,11,11,10,10,10,12,11,11)
internal = c(0,0,12,12,12,11,11,11,9,8,8,9,7,8)
international = c(0,0,rep(12,times = 12))
place = as.vector(rep("South America",times = 14))

southam = data.frame("date"=date,"school"=school,"work"=work,"events"=events,"transp"=transp,
                   "home"=home,"internal"=internal,"international"=international,"location"=place)
#europe
table(datasets$june20$continent)
#school
continents$eu$sc
#work
continents$eu$work[1]
#events
continents$eu$events[1]
#transports
continents$eu$transp[3]
#home
continents$eu$home[1]
#internal
continents$eu$internal[1]
#international
continents$eu$international[1]

school = c(0)
work = c(0)
transp = c(0,0)
#building table world
for(i in 2:length(continents$w$sc)){
  
  school[i] = continents$eu$sc[[i]]$Freq[[2]]
  work[i] = continents$eu$work[[i]]$Freq[[2]]
  events[i] = continents$eu$events[[i]]$Freq[[2]]
  home[i] = continents$eu$home[[i]]$Freq[[2]]
  internal[i] = continents$eu$internal[[i]]$Freq[[2]]
 
}

for(i in 1:length(continents$w$sc)){
  international[i] = continents$eu$international[[i]]$Freq[[2]]
  place[i] = "Europe"
  
}
for(i in 3:length(continents$w$sc)){
  transp[i] = continents$eu$transp[[i]]$Freq[[2]]
  
}


europe = data.frame("date"=date,"school"=school,"work"=work,"events"=events,"transp"=transp,
                     "home"=home,"internal"=internal,"international"=international,"location"=place)


#building table world
for(i in 1:length(continents$w$sc)){
  
  school[i] = continents$sa$sc[[i]]$Freq[[2]]
  work[i] = continents$sa$work[[i]]$Freq[[2]]
  events[i] = continents$sa$events[[i]]$Freq[[2]]
  transp[i] = continents$sa$transp[[i]]$Freq[[2]]
  home[i] = continents$sa$home[[i]]$Freq[[2]]
  internal[i] = continents$sa$internal[[i]]$Freq[[2]]
  international[i] = continents$sa$international[[i]]$Freq[[2]]
  place[i] = "south america"
}
