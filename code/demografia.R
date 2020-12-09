#Gráfica Piramide Poblacional de Costa Rica
#Autor: Omar Barrantes Sotela

# Librerias
library(stats, psych)
library(ggplot2)
library(xtable)
library(grid)
require(doBy)
require(psych)
require(stats)
library(plotrix)

## Lectura de archivos o tablas de datos

#CR
poblacion <-
  read.table("./data/inec/PoblacionCR2020.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

poblacion

## Subset (usa COD y aaaa(año))
pob <- subset(poblacion, subset=((COD==1) & (aaaa==2020)))
pob_H <- subset(pob, subset=Genero=="Hombres")
pob_M <- subset(pob, subset=Genero=="Mujeres")

hombre <- (pob_H$pob/sum(pob_H$pob))*100
mujer <- (pob_M$pob/sum(pob_M$pob))*100

agegrps<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
           "45-49","50-54","55-59","60-64","65-69","70-74","75 y más")

#pdf('./graf/demografia/piram40101_2017.pdf')
#pdf('./graf/demografia/piram20101_2017.pdf')

piracr <- pyramid.plot(hombre,mujer, labels=agegrps, 
             main="Costa Rica: Pirámide Poblacional 2020", gap=2, show.values=TRUE,
             laxlab=c(0,2,4,6,8,10,12,14),raxlab=c(0,2,4,6,8,10,12,14),  #Etiquetas en los ejes derecho e izquierdo
             top.labels=c("Hombre","Edades","Mujer"))

#----------------------------------------------------
#Guarda el gráfico
nom.plot1 <- paste0('./graf/demografia/piramCR_2020.pdf')
nom.plot2 <- paste0('./graf/demografia/piramCR_2020.png')

dev.print(pdf,
          file=nom.plot1,
          width=9, height=6, pointsize = 10) #, pointsize=0.5 Setea el tamaño del punto

dev.print(nom.plot2,
          #device=png, width=3240, res=300)
          device=png, width=2160, height=1620, res=300)

dev.off()

