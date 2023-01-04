rm(list=ls())
#Cargo de librerias.
library(dplyr)
library(ggplot2)
library(readxl)

### Gráfica 1: Ingresos  #####
# 1. Cargar datos.
pe <- read_excel("01_1PE&OLES Ingresos por segmento.xlsx")
attach(pe)
head(pe)

# 2. Preparar datos.
pe <- pe %>%
  mutate(plomo_sum=concentrado_de_plomo+plomo)

pe_reshaped <- data.frame(time = pe$periodo,
                             values = c(pe$plata, pe$oro, pe$zinc, pe$plomo_sum, pe$cobre),
                          metal = c(rep("Plata", nrow(pe)),
                                  rep("Oro", nrow(pe)),
                                  rep("Zinc", nrow(pe)),
                                  rep("Plomo", nrow(pe)),
                                  rep("Cobre", nrow(pe))))
head(pe_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 1.",
        subtitle="Ingresos de Industrias Peñoles por segmento en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe_reshaped, aes(x=time, y=values, group=metal,colour=metal))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,550))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))
  

detach(pe)
rm(list=ls())

### Gráfica 2: Ingresos por oro #####
# 1. Cargar datos.
pe2 <- read_excel("01_2comparativa pe_gm.xlsx")
attach(pe2)
head(pe2)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 2.",
        subtitle="Ingresos de oro en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe2, aes(x=periodo, y=oro, group=empresa,colour=empresa))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,550))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe2)
rm(list=ls())

### Gráfica 3: Ingresos por plata #####
# 1. Cargar datos.
pe2 <- read_excel("01_2comparativa pe_gm.xlsx")
attach(pe2)
head(pe2)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 3.",
        subtitle="Ingresos de plata en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe2, aes(x=periodo, y=plata, group=empresa,colour=empresa))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,450))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe2)
rm(list=ls())

### Gráfica 4: Ingresos por cobre #####
# 1. Cargar datos.
pe2 <- read_excel("01_2comparativa pe_gm.xlsx")
attach(pe2)
head(pe2)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 4.",
        subtitle="Ingresos de cobre en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe2, aes(x=periodo, y=cobre, group=empresa,colour=empresa))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,3000))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe2)
rm(list=ls())


### Gráfica 5: Ingresos por zinc #####
# 1. Cargar datos.
pe2 <- read_excel("01_2comparativa pe_gm.xlsx")
attach(pe2)
head(pe2)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 5.",
        subtitle="Ingresos de zinc en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe2, aes(x=periodo, y=zinc, group=empresa,colour=empresa))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,225))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe2)
rm(list=ls())

### Gráfica 6: Ingresos por plomo #####
# 1. Cargar datos.
pe2 <- read_excel("01_2comparativa pe_gm.xlsx")
attach(pe2)
head(pe2)

#Modificar datos.
pe2 <- pe2 %>%
  mutate(plomo_sum=concentrado_de_plomo+plomo)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Gráfica 6.",
        subtitle="Ingresos de plomo en millones de USD.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe2, aes(x=periodo, y=plomo_sum, group=empresa,colour=empresa))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(0,300))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe2)
rm(list=ls())



### Gráfica 7: Razones liquidez####
pe3 <- read_excel("01_3PE&OLES razones r.xlsx")
attach(pe3)
head(pe3)

# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                          values = c(pe3$ratio_corriente, pe3$prueba_del_acido),
                          razon_financiera = c(rep("Ratio corriente", nrow(pe3)),
                                    rep("Prueba del acido", nrow(pe3))))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Liquidez.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=razon_financiera,colour=razon_financiera))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::comma,
                     limits=c(0,6.5))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))


detach(pe3)
rm(list=ls())

### Gráfica 8: Razones de actividad####
pe3 <- read_excel("01_3PE&OLES razones r.xlsx")
attach(pe3)
head(pe3)

# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$dias_de_ventas_pendientes_rotacion, 
                                      pe3$dias_inventario_rotacion,
                                      pe3$dias_de_rotacion_de_cuentas_por_pagar),
                           razon_financiera = c(rep("Diaas: Ventas pendientes", nrow(pe3)),
                                                rep("Diaas: Inventario", nrow(pe3)),
                                                rep("Diaas: Cuentas por pagar", nrow(pe3))))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Actividad (rotación).",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=razon_financiera,colour=razon_financiera))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::comma,
                     limits=c(0,160))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))


detach(pe3)
rm(list=ls())
### Gráfica 9: Razones de apalancamiento####
pe3 <- read_excel("01_3PE&OLES razones r.xlsx")
attach(pe3)
head(pe3)
# Grafica 9.1.
# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$razon_deuda))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Deuda/Capital.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=1))+
  geom_line(col="blue",size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::comma,
                     limits=c(0,40))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

# Grafica 9.2.
# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$cobertura_interes))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Cobertura interés.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=1))+
  geom_line(col="orange",size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::comma,
                     limits=c(-5,35))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

# Grafica 9.3.
# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$cobertura_deuda))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Cobertura deuda.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=1))+
  geom_line(col="purple",size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::comma,
                     limits=c(0,3.5))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))

detach(pe3)
rm(list=ls())

### Gráfica 10: Rentabilidad####
pe3 <- read_excel("01_3PE&OLES razones r.xlsx")
attach(pe3)
head(pe3)

# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$retorno_sobre_el_capital_comun, 
                                      pe3$retorno_en_activos,
                                      pe3$retorno_de_capital),
                           razon_financiera = c(rep("Retorno sobre el capital común", nrow(pe3)),
                                                rep("Retorno en activos", nrow(pe3)),
                                                rep("Retorno de capital", nrow(pe3))))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Rentabilidad.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=razon_financiera,colour=razon_financiera))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::percent,
                     limits=c(-.1,.20))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))


detach(pe3)
rm(list=ls())

### Gráfica 11: Margenes####
pe3 <- read_excel("01_3PE&OLES razones r.xlsx")
attach(pe3)
head(pe3)

# 2. Preparar datos.
pe3_reshaped <- data.frame(time = pe3$periodo,
                           values = c(pe3$margen_bruto, 
                                      pe3$margen_operacional,
                                      pe3$margen_beneficio_neto),
                           razon_financiera = c(rep("Margen bruto", nrow(pe3)),
                                                rep("Margen operacional", nrow(pe3)),
                                                rep("Margen beneficio neto", nrow(pe3))))
head(pe3_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Margenes.",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe3_reshaped, aes(x=time, y=values, group=razon_financiera,colour=razon_financiera))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::percent,
                     limits=c(-.20,.45))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))


detach(pe3)
rm(list=ls())


### Gráfica 12: ingresos beneficio neto####
pe <- read_excel("01_4PE&OLES ER.xlsx")
attach(pe)
head(pe)

# 2. Preparar datos.
pe_reshaped <- data.frame(time = pe$periodo,
                          values = c(pe$ingreso,pe$beneficio_bruto, pe$beneficio_neto),
                          rubro = c(rep("Ingreso", nrow(pe)),
                                    rep("Beneficio bruto",nrow(pe)),
                                    rep("Beneficio neto", nrow(pe))))
head(pe_reshaped)

# 3. Etiquetas.
etiquetas <- 
  labs (title= "Ingresos, beneficios brutos y netos en millones de USD",
        x=" ",
        y=" ",
        caption= "Fuente: Bloomberg")

ggplot(pe_reshaped, aes(x=time, y=values, group=rubro,colour=rubro))+
  geom_line(size=1)+
  theme_grey()+
  geom_hline(yintercept=0)+
  scale_y_continuous(labels=scales::dollar,
                     limits=c(-200,1300))+
  etiquetas+
  theme(axis.text.x=element_text(angle = 90))


detach(pe)
rm(list=ls())
