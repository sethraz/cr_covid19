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
library(dplyr)
library(rgdal)

#-------------------------------------------------------------------------------
#Lectura de datos
filename <- './data/minsa/minsa_cr_cantones_covid_ts.csv' #file.choose()

dat.canton <- read.table(filename, header=TRUE, sep=",",
                         #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                         na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

dat.canton$fecha <- as.POSIXct(dat.canton$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo

#Log transformation para casos nuevos
dat.canton$cc.0 <-   dat.canton$cc_nuevos
dat.canton$cc.0[dat.canton$cc.0 < 1] <- 0.01
dat.canton$log.cc <- log10(dat.canton$cc.0)
dat.canton

#-------------------------------------------------------
#Indicadores a asociar

#B?sicos en Salud 2013
#ind <- read.table('./data/minsa/tab/indicadores_basicos_salud_2013.csv', header=TRUE, sep=",",
#                        colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
#                        na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

#Indice Progreso Social
ind <- read.table('./data/minsa/tab/indicadores_basicos_cantonales2020.csv', header=TRUE, sep=",",
                  #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                  na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

#Remueve los valores NA y los pasa a valores de 0
ind[is.na(ind)] <- 0
summary(ind)

#--------------------------------------------------------
#Carga shapefile de cantones de Costa Rica

cr_cantones <- readOGR("./data/shp/IGN_5_limitecantonal_5k.shp","IGN_5_limitecantonal_5k", 
                       use_iconv=TRUE, encoding="UTF-8")

proj4string(cr_cantones)

#cr <- readOGR("./data/shp/cr_covid19_null.shp","cr_covid19_null", 
#              use_iconv=TRUE, encoding="UTF-8")


#--------------------------------------------------------
#Combina la información de las estadísticas con el Shapefile de cantones de CR

library(tigris)
#geo_join(cr_cantones, dat.stats, 'cod_canton', 'cod_canton', by = NULL, how = "left")

ini.f <- as.Date(min(dat.canton$fecha)-86400, format = "%Y-%m-%d")
ini.date <- min(dat.canton$fecha)
fin.date <- max(dat.canton$fecha)
frango <- as.integer(fin.date-ini.date)
irango <- 150 #54 #0 Si es desde el primer día, 54 desde el segundo mes
ss <- 1 #Se requiere salida por dia

#Configura el tipo de analisis
tipo.a <- c('casos_acc','casos_nvs')##
tt <- tipo.a[2]  ## <- SETEAR ******

#---------------------------------------------------------
#Genera union de datos para trabajo

#Por favor indicar el día
#i <- frango # POR DEFECTO ****ULTIMO DIA******

for (i in irango:frango){
  #i <- 150 #0
  aaaa <- format(as.Date(ini.f+(i),format="%Y-%m-%d"), format = "%Y")
  mm   <- format(as.Date(ini.f+(i),format="%Y-%m-%d"), format = "%m")
  dd   <- format(as.Date(ini.f+(i),format="%Y-%m-%d"), format = "%d")
  
  
  ff <-   format(as.Date(ini.f+(i),format="%Y-%m-%d"), format = "%Y_%m_%d") 
  ff <- as.character(ff)
  
  dat.canton.s <- subset(dat.canton, fecha == ini.date+(i*86400)) #i por segundos en un d?a
  dat.canton.s
  
  cr.cantones.0 <- geo_join(cr_cantones, dat.canton.s, by = 'cod_canton', how = "left") 
  cr.cantones.0
  
  #i=0
  #Limpia la tabla
  drops <- c('id','cod_canton.1','canton.1') # list of col names
  cr.cantones.0 <- cr.cantones.0[,!(names(cr.cantones.0) %in% drops)] #remueve las columnas

  #Agrega los indicadores de salud por canton
  cr.cantones.0 <- geo_join(cr.cantones.0, ind , by = 'cod_canton', how = "left") 
  
  #Limpia variables que no se usarán
  #rm(cr_cantones,dat.canton,dat.canton.s)
  rm(dat.canton.s)
  
  #Limpia tabla
  drops <- c("ori_toponi","cod_provin.1","provincia.1","cod_canton.1","canton.1")# list of col names
  cr.cantones.0 <- cr.cantones.0[,!(names(cr.cantones.0) %in% drops)] #remueve las columnas
  
  #Organiza las entidades por cod_canton y fecha
  cr.cantones.0  <- cr.cantones.0[order(cr.cantones.0$cod_canton,cr.cantones.0$fecha),] 
  cr.cantones.0@data
  str(cr.cantones.0@data)
  
  cr.cantones.0@data
  str(cr.cantones.0@data)

#---------------------------------------------------------------
  #Salva archivo base
  file.out <- paste0("cantones_covid19_ind_",aaaa,mm,dd)
  
  #Si archivo no existe crearlo
  if(!file.exists(paste0("./data/shp/ind_dd/",file.out))) {
    writeOGR(cr.cantones.0, "./data/shp/ind_dd", layer= file.out , driver="ESRI Shapefile", overwrite_layer=T)
  }
  
  #---------------------------------------------------------------
  #Inicia procedimiento para el calculo del I Moran Bivariate
  
  for (md.0 in 1:6){
  
    if (md.0 == 1){
      #modelo 1
      # Variables to use in the correlation: 
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m1"
        #  md.0 <- 1
        x <- cr.cantones.0@data[,"dpob2020"] # Densidad de poblacion
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
        modelo <- "m1"
        #  md.0 <- 1
        x <- cr.cantones.0@data[,"dpob2020"] # Densidad de poblacion
        y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
    }
    
    if (md.0 == 2){
      #modelo 2
      # Variables to use in the correlation: 
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m2"
        #md.0 <- 2
        x <- cr.cantones.0@data[,"idsc2017"] # Indice de Desarrollo Social Cantonal idsc2017 
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
          modelo <- "m2"
          #md.0 <- 2
          x <- cr.cantones.0@data[,"idsc2017"] # Indice de Desarrollo Social Cantonal idsc2017 
          y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
      }
  
    if (md.0 == 3){
      #modelo 3
      # Variables to use in the correlation: 
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m3"
        #  md.0 <- 3
        x <- cr.cantones.0@data[,"ips2019"] # Indice de Progreso Social (ips2019)
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
          modelo <- "m3"
          #  md.0 <- 3
          x <- cr.cantones.0@data[,"ips2019"] # Indice de Progreso Social (ips2019)
          y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
    }
  
    if (md.0 == 4){
      #modelo 4
      # Variables to use in the correlation: 
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m4"
        #md.0 <- 4
        x <- cr.cantones.0@data[,"nhb2019"] # Necesidades humanas básicas 2019
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
          modelo <- "m4"
          #md.0 <- 4
          x <- cr.cantones.0@data[,"nhb2019"] # Necesidades humanas básicas 2019
          y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
    }
  
    if (md.0 == 5){
      #modelo 5
      # Variables to use in the correlation: 
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m5"
        #md.0 <- 5
        x <- cr.cantones.0@data[,"tvacuna2019"] # Tasa de vacunacion  2019
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
          modelo <- "m5"
          #  md.0 <- 5
          x <- cr.cantones.0@data[,"tvacuna2019"] # Tasa de vacunacion  2019
          y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
    }

    if (md.0 == 6){
      #modelo 6
      # Variables to use in the correlation: 
      #-----------------------------
      #-----------------------------
      if (tt == tipo.a[1]) {
        modelo <- "m6"
        #md.0 <- 6
        x <- cr.cantones.0@data[,"den_rdvial"] # Densidad vial 2020
        y <- cr.cantones.0@data[,"casos_confirmados"] # Casos confirmados  
        } else {
          modelo <- "m6"
          #md.0 <- 6
          x <- cr.cantones.0@data[,"den_rdvial"] # Densidad vial 2020
          y <- cr.cantones.0@data[,"log.cc"] # Casos nuevos confirmados en (log)
        }
    }
    
    print(paste0("Inicia proceso..... I Moran Bivariate: fecha ",aaaa,"-",mm,"-",dd," . Modelo: ",md.0))
    source("./code/an02_covid19_bivariate_IMoran.R")
  }
  
  #---------------------------------------------------------------------------------  

  #str(cr.cantones.0@data)
  
  remove(cr.cantones.0)
  

#-----------------------------------------------------------------------------------
print("Hecho.\n")

}




