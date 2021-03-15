#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("lubridate")
#install.packages("ggthemes")
#install.packages("gridExtra")
library(tidyverse)
library(readxl)
library(lubridate)
library(ggthemes)
library(gridExtra)
polm <- data.frame(read_xlsx("pol_meas.xlsx"))

polm[polm$Localidade=="Mundo",c(3:16)]=polm[polm$Localidade=="Mundo",c(3:16)]/195
polm[polm$Localidade=="Europa",c(3:16)]=polm[polm$Localidade=="Europa",c(3:16)]/45
polm[polm$Localidade=="América do Sul",c(3:16)]=polm[polm$Localidade=="América do Sul",c(3:16)]/12
polm[polm$Localidade=="América do Norte e Central",c(3:16)]=polm[polm$Localidade=="América do Norte e Central",c(3:16)]/35

n_polm <- polm %>% gather("Mês","Proporção de medidas restritivas", Janeiro:Fevereiro2021)
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
n_polm[n_polm$Mês=="Novembro",3]="11/2020"
n_polm[n_polm$Mês=="Dezembro",3]="12/2020"
n_polm[n_polm$Mês=="Janeiro2021",3]="01/2021"
n_polm[n_polm$Mês=="Fevereiro2021",3]="02/2021"

n_polm$Mês <- my(n_polm$Mês)

(eu=ggplot(data = n_polm[n_polm$Localidade=="Europa",], mapping = aes(x = Mês, y =`Proporção de medidas restritivas`,colour=Medidas)) +
    geom_line(stat = "identity",size=1.1)+
    labs(title="Na Europa.",x="",y="Proporção de países.",colour="Localidade")+ 
    scale_color_manual(values =c("green","red","dark blue","yellow","purple","magenta","black") )+
    scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")+ylim(0.0,1.0)+
    theme(legend.position = "none",
          panel.border = element_rect(fill = NA)))

?geom_line

(wo=ggplot(data = n_polm[n_polm$Localidade=="Mundo",], mapping = aes(x = Mês, y =`Proporção de medidas restritivas`,colour=Medidas)) +
    geom_line(stat = "identity",size=1.1)+
    labs(title="No mundo.",x="",y="Proporção de países.",colour="Localidade")+ 
    scale_color_manual(values =c("green","red","dark blue","yellow","purple","magenta","black") )+
    scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")+ylim(0.0,1.0)+
    theme(legend.position ="none",
          panel.border = element_rect(fill = NA)))

(sa=ggplot(data = n_polm[n_polm$Localidade=="América do Sul",], mapping = aes(x = Mês, y =`Proporção de medidas restritivas`,colour=Medidas)) +
    geom_line(stat = "identity",size=1.1)+
    labs(title="Na América do Sul.",x="",y="Proporção de países.",colour="Localidade")+ 
    scale_color_manual(values =c("green","red","dark blue","yellow","purple","magenta","black") )+
    scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")+ylim(0.0,1.0)+
    theme(legend.position = "none",
          panel.border = element_rect(fill = NA)))

(na=ggplot(data = n_polm[n_polm$Localidade=="América do Norte e Central",], mapping = aes(x = Mês, y =`Proporção de medidas restritivas`,colour=Medidas)) +
    geom_line(stat = "identity",size=1.1)+
    labs(title="América do Norte e Central.",x="",y="Proporção de países.",colour="Localidade")+ 
    scale_color_manual(values =c("green","red","dark blue","yellow","purple","magenta","black") )+
    scale_x_date(date_breaks = "2 month",date_labels = "%m/%Y")+ylim(0.0,1.0)+
    theme(legend.position =c(0,-1),legend.title=element_text(size=14),legend.text=element_text(size=15),legend.box.just ="top", legend.key = element_rect(fill=alpha("transparent",0.02)),
          legend.background = element_rect(fill=alpha('transparent', 0.02)),panel.border = element_rect(fill = "transparent")))
#width=1366&height=678
grid.arrange(wo,eu,sa,na,nrow=3,ncol=2)
