rm(list=ls())
#Cargo de librerias.
library(dplyr)
library(ggplot2)
library(readr)

#Cargar datos.
pe <- read.csv("camb.csv")
attach(pe)
head(pe)

# ggplot de líneas (valores totales)
ggplot(data=pe,
             mapping= aes(x=fecha, y=ingresp, group=1))+
  theme_grey()+
  geom_line(col="blue",size=1)+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::percent,
                     limits=c(-.5,.5))+
  labs(y="venta",
       x="fecha",
       title="Gráfica #",
       subtitle="Precio dolar",
       caption="Fuente: Bloomberg")


rm(list=ls())
#Cargo de librerias.
library(dplyr)
library(ggplot2)
library(readxl)
#Cargar datos.
pe <- read_excel("PE&OLES Ingresos por segmento.xlsx")
attach(pe)
head(pe)

# ggplot de líneas (valores totales)

ggplot(data=pe,
       mapping= aes(x=periodo, y=ingreso, group=1))
ggplot(economics, aes(x=periodo))+ 
  geom_line(aes(y = ingreso), color = "darkred") + 
  geom_line(aes(y = oro), color="steelblue", linetype="twodash")+
  
  theme_grey()+
  geom_line(col="blue",size=1)+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,1500))+
  labs(y="Ingreso",
       x="Trimestre",
       title="Gráfica 1",
       subtitle="Precio dolar",
       caption="Fuente: Bloomberg")

#Grafica 1:

etiquetas <- 
  labs (title= "Gráfica 1",
        subtitle="BUFO PIJA",
        x="Trimestre",
        y="Ingreso",
        caption= "Fuente: Bloomberg")

#Preparar datos

library(ggplot2)                   # Load ggplot2
library(reshape2)

pe_long <- melt(data=pe, id="periodo")    # Transforming data to long format
head(pe_long)       

ggplot(pe_long,                    # Drawing ggplot2 plot
       aes(x = periodo,
           y = value,
           color = variable)) +
  geom_line()

ggplot(data=pe,
       mapping= aes(x=periodo, group=1))+
  geom_line(aes(y = oro), color = "darkred", size=1)+ 
  geom_line(aes(y = plata), color="steelblue", size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,650))+
  etiquetas
