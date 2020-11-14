#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("lubridate")
library(tidyverse)
library(readxl)
library(lubridate)
polm <- data.frame(read_xlsx("pol_meas.xlsx"))

polm[polm$Localidade=="Mundo",c(3:12)]=polm[polm$Localidade=="Mundo",c(3:12)]/195
polm[polm$Localidade=="Europa",c(3:12)]=polm[polm$Localidade=="Europa",c(3:12)]/44
polm[polm$Localidade=="América do Sul",c(3:12)]=polm[polm$Localidade=="América do Sul",c(3:12)]/12
polm[polm$Localidade=="América do Norte e Central",c(3:12)]=polm[polm$Localidade=="América do Norte e Central",c(3:12)]/35

n_polm <- polm %>% gather("Mês","Proporção de medidas restritivas", Janeiro:Outubro)
n_polm[n_polm$Mês=="Janeiro",3]="01/2020"
n_polm[n_polm$Mês=="Fevereiro",3]="02/2020"
n_polm[n_polm$Mês=="Março",3]='03/2020'
n_polm[n_polm$Mês=="Abril",3]="04/2020"
n_polm[n_polm$Mês=="Maio",3]="05/2020"
n_polm[n_polm$Mês=="Junho",3]="06/2020"
n_polm[n_polm$Mês=="Julho",3]="07/2020"
n_polm[n_polm$Mês=="Agosto",3]="08/2020"
n_polm[n_polm$Mês=="Setembro",3]="09/2020"
n_polm[n_polm$Mês=="Outubro",3]="10/2020"
n_polm$Mês <- my(n_polm$Mês)

ggplot(data = n_polm[n_polm$Localidade=="Europa",], mapping = aes(x = Mês, y =`Proporção de medidas restritivas`,colour=Medidas)) +
  geom_line(stat = "identity",size=1.1)+
  labs(title="Na Europa.",x="",y="Proporção de medidas.",colour="Localidade")+ 
#  scale_color_manual(values =c("green","red") )+
 scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")+ylim(0.0,1.0)
?geom_line
