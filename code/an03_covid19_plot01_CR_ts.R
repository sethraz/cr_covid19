#Scritp para gráficación Pandemia COVID-19 en CR

#-------------------------------------------------------------------------------
#Librerias
library(ggplot2)
library(reshape2)

#-------------------------------------------------------------------------------
#Lectura de datos
f.name <- './data/minsa/minsa_cr_covid_ts.csv' #file.choose()

dat.covid <- read.table(f.name, header=TRUE, sep=",",
                         #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                         na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

dat.covid$fecha <- as.POSIXct(dat.covid$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo
str(dat.covid)

#----------------------------------------------------
#Ultima fecha en los registros
aaaa <- format(as.Date(dat.covid[dim(dat.covid)[1],"fecha"],format="%Y-%m-%d"), format = "%Y")
mm   <- format(as.Date(dat.covid[dim(dat.covid)[1],"fecha"],format="%Y-%m-%d"), format = "%m")
dd   <- format(as.Date(dat.covid[dim(dat.covid)[1],"fecha"],format="%Y-%m-%d"), format = "%d")

#aaaa <- "2020"
#mm <- "05"
#dd <- "18"

#----------------------------------------------------
#Data Preparación
dat.sub <- dat.covid[, c("fecha","casos_muertes","casos_recuperados","cc_activos","casos_confirmados")]

dat.s <- melt(dat.sub, id=c("fecha"))
dat.s

#----------------------------------------------------
#Paleta de colores
col.covid <- c("#101010","#1a9850","#fee08b","#FF1D15")
#----------------------------------------------------

#Gráfico 01

covid.ts.plot <- ggplot(dat.s) +
  geom_line (aes(x = fecha, y = value, colour= variable), size=1) +
  geom_point(aes(x = fecha, y = value, colour= variable), size=2) +
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 11),
        panel.background = element_blank(),
        legend.position = "bottom",) +
  labs(x = "Fecha", y = expression("Cantidad de casos") #, 
       #title = paste0("Costa Rica: Evolución de la enfermedad COVID-19")
       ) +
  scale_color_manual(name= "Tipo de casos", values=col.covid,
                     labels = c("Decesos","Recuperados","Activos","Confirmados")) +
  scale_x_datetime(date_labels = "%d-%m-%Y") #+ 
#scale_y_continuous(trans = 'log10')
  
plot(covid.ts.plot)
  
#----------------------------------------------------
#Guarda el gráfico
nom.plot1 <- paste0("./graf/cr_covid19_ts_",aaaa,mm,dd,".pdf")
nom.plot2 <- paste0("./graf/cr_covid19_ts_",aaaa,mm,dd,".png")

dev.print(pdf,
          file=nom.plot1,
          width=8, height=4, pointsize = 10) #, pointsize=0.5 Setea el tamaño del punto

dev.print(png,
          file = nom.plot2,
          #device=png, width=3240, res=300)
          width=2160, res=300)

dev.off()

#----------------------------------------------------
#Gráfico en Escala Logaritmica

covid.ts.plot.log <- ggplot(dat.s) +
  geom_line (aes(x = fecha, y = value, colour= variable), size=1) +
  geom_point(aes(x = fecha, y = value, colour= variable), size=2) +
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 14),
        panel.background = element_blank(),
        legend.position = "bottom",) +
  labs(x = "Fecha", y = expression("Cantidad de casos"), 
       title = paste0("Costa Rica: Evolución de la enfermedad COVID-19")) +
  scale_color_manual(name= "Tipo de casos", values=col.covid,
                     labels = c("Decesos","Recuperados","Activos","Confirmados")) +
  scale_x_datetime(date_labels = "%d-%m-%Y") + 
  scale_y_continuous(trans = 'log10')

plot(covid.ts.plot.log)

nom.plot1 <- paste0("./graf/cr_covid19_ts_log_",aaaa,mm,dd,".pdf")
nom.plot2 <- paste0("./graf/cr_covid19_ts_log",aaaa,mm,dd,".png")

dev.print(pdf,
          file=nom.plot1,
          width=8, height=4, pointsize = 10) #, pointsize=0.5 Setea el tamaño del punto

dev.print(nom.plot2,
          #device=png, width=3240, res=300)
          device=png, width=2160, res=300)

dev.off()

#---------------------------------------------
#Remueve todas las variables
rm(list = setdiff(ls(), lsf.str()))



#--------------------------------------------
#Grafico de incremento de los casos

  
covid.ts.plot <- ggplot(dat.covid) +
  geom_line (aes(x = fecha, y = delta), size=1) +
  geom_point(aes(x = fecha, y = delta), size=2) +
  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 11),
        panel.background = element_blank(),
        legend.position = "bottom",) +
  labs(x = "Fecha", y = expression("Incremento diario de los casos") #, 
       #title = paste0("Costa Rica: Evolución de la enfermedad COVID-19")
  ) +
  #scale_color_manual(name= "Tipo de casos", values=col.covid,
  #                   labels = c("Decesos","Recuperados","Activos","Confirmados")) +
  scale_x_datetime(date_labels = "%d-%m-%Y") #+ 
#scale_y_continuous(trans = 'log10')

plot(covid.ts.plot)

#----------------------------------------------------
#Guarda el gráfico
nom.plot1 <- paste0("./graf/cr_covid19_ts_delta",aaaa,mm,dd,".pdf")
nom.plot2 <- paste0("./graf/cr_covid19_ts_delta",aaaa,mm,dd,".png")

dev.print(pdf,
          file=nom.plot1,
          width=8, height=4, pointsize = 10) #, pointsize=0.5 Setea el tamaño del punto

dev.print(png,
          file = nom.plot2,
          #device=png, width=3240, res=300)
          width=2160, res=300)

dev.off()

sd(dat.covid$delta)
mean(dat.covid$delta)
