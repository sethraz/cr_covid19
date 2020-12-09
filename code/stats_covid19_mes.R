#Scritp para establecer el modelo de contagio para el COVID-19

#-------------------------------------------------------------------------------
#Librerias

#library(forecast)
#library(fable) #Nueva version forescast
#library(feasts)
#library(tsibbledata)
#library(lubridate)

#library(astsa) # ** SEE FOOTNOTE
library(stats)
#library(zoo)
#library(xts)

#Librerias espaciales
library(rgdal)
library(raster)
library(sp)

#-------------------------------------------------------------------------------
#Lectura de datos
filename <- './data/minsa/minsa_cr_covid_ts.csv' #file.choose()
dat <- read.table(filename, header=TRUE, sep=",",
                  #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                  na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

#--------------------------------------------------------
#Carga shapefile de cantones de Costa Rica
cr_cantones <- readOGR("./data/shp/IGN_5_limitecantonal_5k.shp","IGN_5_limitecantonal_5k", 
                       use_iconv=TRUE, encoding="UTF-8")

cr <- readOGR("./data/shp/cr_covid19_null.shp","cr_covid19_null", 
              use_iconv=TRUE, encoding="UTF-8")

#--------------------------------------------------------
# EPSG 8908 CR-SIRGAS / CRTM05
#proj4string(cr)
#   "+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 


#--------------------------- ----------------------------------------------------
#Lectura de datos estadísticas

filename00 <- './data/minsa/minsa_cr_cantones_covid_ts.csv' #file.choose()
dat.delta <- read.table(filename00, header=TRUE, sep=",",
                        #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                        na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

dat.delta$fecha <- as.POSIXct(dat.delta$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo

#--------------------------------

filename01 <- './data/minsa/Stats_canton.csv' #file.choose()
dat.stats <- read.table(filename01, header=TRUE, sep=",",
                        #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                        na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

dat.stats$fecha <- as.POSIXct(dat.stats$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo

#str(dat.stats)
#dat.stats

#---------------------------------------------
#---------------------------------------------
#Calculo de estadisticos

#Rango de fecha
rango <- c(1,15,30) #sELECCIONA EL RANGO ESTABLECIDO
i <- 3
mes <- "04"
mes.n <- as.integer(mes)+1
aaaa <- 2020

#Filtro por mes
dat.delta.m <- dat.delta[format.Date(dat.delta$fecha, "%m")== mes,]
dat.delta.m$fecha <- as.Date(dat.delta.m$fecha)  # Formato de tiempo
dat.delta.m
str(dat.delta.m)


#Incorpora casos anterior a la fecha y en la fecha
#Caso inicial
dat.delta.ini   <- subset(dat.delta.m, fecha == as.Date(paste0(aaaa,"-",mes,"-01", format="%Y-%m-%d")))
dat.delta.ini

dat.delta.final <-subset(dat.delta.m, fecha== as.Date(paste0(aaaa,"-",mes.n,"-01", format="%Y-%m-%d"))-1)
dat.delta.final

##Calcula las estadisticas por mes

dat.stats[,"cc_con00"] <- 0
#dat.stats[,"cc_con00"] <- dat.delta.ini[,"casos_confirmados"] #Casos confirmados ayer
dat.stats[,"cc_con01"] <- dat.delta.final[,"casos_confirmados"] #Casos confirmados hoy

#Limpia las variables
remove(dat.delta.ini, dat.delta.final, dat.delta, dat.delta.m)
#dat.stats

#---------------------------------------------
#Estima el incremento de nuevos casos
for (i in 1:dim(dat.stats)[1]){
  if (!is.na(dat.stats[i,"cc_con01"]) & !is.na(dat.stats[i,"cc_con00"])) {
    if ((dat.stats[i,"cc_con01"] >= dat.stats[i,"cc_con00"])) {
      dat.stats[i,"cc_delta"] <- dat.stats[i,"cc_con01"]-dat.stats[i,"cc_con00"]
    }  
    else {dat.stats[i,"cc_delta"] <- NA}
  }
  
}

#---------------------------------------------  
#Prevalencia
#La prevalencia es una proporción (P = A/A+B) y aunque también se le denomina como tasa de prevalencia, en realidad no lo es porque falta el tiempo, por ello también se le conoce como pseudotasa. La prevalencia mide la proporción de personas que se encuentran enfermas al momento de evaluar el padecimiento en la población, por lo tanto, no hay tiempo de seguimiento.

#Prevalencia puntual. Esta prevalencia es la más común y, como un ejemplo podría conocerse el número de individuos internados en un hospital por un cuadro agudo de asma; aquí el número de pacientes internados por un cuadro agudo de asma es el numerador, mientras que el denominador será el total de pacientes internados en el hospital respectivo

dat.stats[,"preval_p"] <- (dat.stats[,"cc_con01"] / dat.stats[,"pop_total"]) * 100
dat.stats


#---------------------------------------------  
#Tasa de Incidencia

#Esta medida cumple con el requisito de una tasa, es decir, tiene 3 componentes: numerador = A, denominador = A + B y t = tiempo. Generalmente para obtener esta medida se necesita tener un grupo de individuos que no tengan la enfermedad que se estudia, algunos de los cuales después de un tiempo determinado (por ejemplo, meses o años) pasan del estado de salud al de enfermedad. En esta medida, el numerador lo constituyen los individuos que enfermaron (A) y el denominador, los que no lo hicieron (A + B).

#Por último, en este tipo de incidencia el tiempo (t) es fijo para todos los individuos, es decir, independientemente del momento en que enfermaron, todos tuvieron el mismo tiempo de seguimiento; por ello, el tiempo se elimina de la ecuación y solo queda A/AB.

#La forma de analizar una tasa de incidencia acumulada (IA) es como una proporción, la cual puede multiplicarse por 100. Para ejemplificar: si se ha realizado el seguimiento a 100 individuos sanos durante un mes y se enfermaron 20, entonces se tendría que la IA en un mes fue de 20 % (IA = 20/100; IA = 0.2 × 100; IA = 20 %).

#El tiempo de no exposición en los individuos que enferman es variable en esta tasa de incidencia y depende del momento en que enferman durante el periodo de estudio, situación que debería descontarse del tiempo total de seguimiento, sin embargo, no se toma en cuenta para el cálculo final.

dat.stats[,"ti_acc"] <- (dat.stats[,"cc_delta"] / dat.stats[,"pop_total"]) * 100
#dat.stats

#---------------------------------------------  
#Prevalencia

#Por último, es necesario conocer la relación que guardan la prevalencia, la incidencia y la duración promedio de la enfermedad. La fórmula de la prevalencia es:

#Se usa T de 14. Días de duración más larga reportada de duración del COVID-19

dat.stats[,"preval"] <- (dat.stats[,"ti_acc"] * 14)
#dat.stats

#---------------------------------------------  
#Tasa de ataque x 1000 hab

dat.stats[,"t_ataque"] <- (dat.stats[,"cc_delta"] * 1000) / (dat.stats[,"pop_total"] - dat.stats[,"cc_con00"])
#dat.stats


#---------------------------------------------  
#Limpia variables

dat.stats[, 10:13][dat.stats[, 10:13] == 0] <- NA
dat.stats

#---------------------------------------------  
#---------------------------------------------  
#Combina la información de las estadísticas con el Shapefile de cantones de CR

library(tigris)
#geo_join(cr_cantones, dat.stats, 'cod_canton', 'cod_canton', by = NULL, how = "left")
cr.cantones <- geo_join(cr_cantones, dat.stats, by = 'cod_canton', how = "left")

drops <- c('cod_canton.1','canton.1') # list of col names
cr.cantones <- cr.cantones[,!(names(cr.cantones) %in% drops)] #remueve las columnas

#---------------------------------------------
#Convierte a nueva proyección

proj4string(cr.cantones)
cr.cantones.sirgas <- spTransform(cr.cantones, 
                                  CRS("+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

#plot(cr.cantones.sirgas)
#cr.cantones.sirgas@data
cr.cantones.sirgas <- cr.cantones.sirgas[order(cr.cantones.sirgas$cod_canton),]

#---------------------------------------------
# Save el shapefile with the data merge

file.out <- paste0("cr_cantones_stats_covid19_",rango[i],"d","_",aaaa,"_",mes)
writeOGR(cr.cantones.sirgas, "./data/shp", layer= file.out , driver="ESRI Shapefile", overwrite_layer=T)

#---------------------------------------------
#Remueve todas las variables
rm(list = setdiff(ls(), lsf.str()))