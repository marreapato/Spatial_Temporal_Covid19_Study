#install.packages("tidyverse")
#install.packages("readxl")
library(tidyverse)
library(readxl)

polm <- data.frame(read_xlsx("pol_meas.xlsx"))

polm[polm$Localidade=="Mundo",c(3:12)]=polm[polm$Localidade=="Mundo",c(3:12)]/195
polm[polm$Localidade=="Europa",c(3:12)]=polm[polm$Localidade=="Europa",c(3:12)]/44
polm[polm$Localidade=="América do Sul",c(3:12)]=polm[polm$Localidade=="América do Sul",c(3:12)]/12
polm[polm$Localidade=="América do Norte e Central",c(3:12)]=polm[polm$Localidade=="América do Norte e Central",c(3:12)]/35

n_polm <- polm %>% gather("Mês","Proporção de medidas restritivas", Janeiro:Outubro)
