library(tidyverse)

#reading the data

recovered=read.csv("data_recovered.csv",header = F)[,-1]
deaths=read.csv("data_deaths.csv",header = F)[,-1]
confirmed=read.csv("data_confirmed.csv",header = F)[,-1]

