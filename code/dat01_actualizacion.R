#Scritp para actualización Datos de Costa Rica COVID-19
#A partir de los Datos del OGES

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
#library(rgdal)
#library(raster)
#library(sp)
library(dplyr)
library(readxl)
#library(pkgbuild)



#-------------------------------------------------------------------------------
#cr <- readOGR("./data/shp/cr_covid19_null.shp","cr_covid19_null", 
#              use_iconv=TRUE, encoding="UTF-8")

#--------------------------- ----------------------------------------------------
#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip",destfile="ne_10m_admin_1_states_provinces.zip")

#Establece el mes y día
#aaaa <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y")
#mm <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%m")
#dd <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%d")

aaaa <- "2020"
mm <- "12"
dd <- "08"


date.cc <- as.POSIXct(paste0(aaaa,"-",mm,"-",dd," 19:00"), format="%Y-%m-%d %H:%M") # Formato de tiempo
date.c1 <- as.POSIXct(paste0(aaaa,"-",mm,"-",dd), format="%Y-%m-%d") # Formato de fecha

date.c0 <- date.cc-86400 #Dia anterior
#date.c0


metodo <- 0 # 1 si se descarga y 0 si se usa de forma local.
key <- ',' #Separador de lista
deci <- '.' #Separador de decimal

if (metodo==1){
#------------------------------------------------------------------
  #---------------------------------------------------------------------------
  #Download Data desde OGES CSV
  #http://geovision.uned.ac.cr/oges/archivos_covid/07_20/07_20_CSV_GENERAL.csv
  
  url.00  <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_POSITIVOS.csv")
  try(download.file(url.00, destfile = paste0("./data/OGES/",mm,"_",dd,"_CSV_POSITIVOS.csv"), mode="wb"))
  
  url.00b <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_RECUP.csv")
  try(download.file(url.00b, destfile = paste0("./data/OGES/",mm,"_",dd,"_CSV_RECUP.csv"), mode="wb"))
  
  url.01 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_GENERAL.csv")
  mtry.01 <- try(download.file(url.01, destfile = paste0("./data/OGES/",mm,"_",dd,"_CSV_GENERAL.csv"),mode="wb"))
  
  url.02 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_FALLECIDOS.csv")
  mtry.02 <- try(download.file(url.02, destfile = paste0("./data/OGES/",mm,"_",dd,"_CSV_FALLECIDOS.csv"),mode="wb"))
  
  #Abre datos desde OGES

  #Casos confirmados
  url  <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_POSITIVOS.csv") 
  #Casos recuperados
  url1 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_RECUP.csv")

  #Casos fallecidos
  url2 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_FALLECIDOS.csv")

  #Casos fallecidos
  url3 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_CSV_GENERAL.csv")
  
  
  mtry <-   try(dat.oges  <- read.csv(url, header=TRUE, sep=key,
                                      na.strings="NA", dec=deci, strip.white=TRUE))

  mtry1 <-  try(dat.oges1 <- read.csv(url1, header=TRUE, sep=key,
                                      na.strings="NA", dec=deci, strip.white=TRUE))

  mtry2 <-  try(dat.oges2 <- read.csv(url2, header=TRUE, sep=key,
                                      na.strings="NA", dec=deci, strip.white=TRUE))

  mtry3 <-  try(dat.oges3 <- read.csv(url3, header=TRUE, sep=key,
                                      na.strings="NA", dec=deci, strip.white=TRUE))

  } else {
          #Opcion con archivo descargado
          fp0 <- paste0("./data/OGES/",mm,"_",dd,"_CSV_POSITIVOS.csv")
          fp1 <- paste0("./data/OGES/",mm,"_",dd,"_CSV_RECUP.csv")
          fp2 <- paste0("./data/OGES/",mm,"_",dd,"_CSV_FALLECIDOS.csv")
          fp3 <- paste0("./data/OGES/",mm,"_",dd,"_CSV_GENERAL.csv")
          
          mtry  <-  try(dat.oges  <- read.csv(fp0))
          mtry1 <-  try(dat.oges1 <- read.csv(fp1))  
          mtry2 <-  try(dat.oges2 <- read.csv(fp2))  
          mtry3 <-  try(dat.oges3 <- read.csv(fp3))
  
    }

mtry
mtry1

class(mtry)


if ((class(mtry) != "try-error") & (class(mtry1) != "try-error") & (class(mtry2) != "try-error") ) {
  if (metodo==1) {
      dat.oges  <- read.csv(url,header=TRUE, sep=key,
                            na.strings="NA", dec=deci, strip.white=TRUE)
      dat.oges1 <- read.csv(url1,header=TRUE, sep=key,
                            na.strings="NA", dec=deci, strip.white=TRUE)
      dat.oges2 <- read.csv(url2,header=TRUE, sep=key,
                            na.strings="NA", dec=deci, strip.white=TRUE)
  } else {
      dat.oges  <- read.csv(fp0)
      dat.oges1 <- read.csv(fp1)  
      dat.oges2 <- read.csv(fp2)
      }
  
  
  head(dat.oges)
  casos.totales <- sum(dat.oges[,dim(dat.oges)[2]])
  
  casos.totales.recup <- sum(dat.oges1[,dim(dat.oges1)[2]])
  
  casos.totales.decesos <- sum(dat.oges2[,dim(dat.oges2)[2]])
  
  #Casos totales del día
  print("--------------------------------------------------------")
  print(paste0("Cantidad de casos confirmados totales: ",casos.totales))
  print(paste0("Cantidad de casos recuperados totales: ",casos.totales.recup))
  print(paste0("Cantidad de casos fallecidos totales: ",casos.totales.decesos))
  print("--------------------------------------------------------")
  
  #-------------------------------------------------------------
  #Limpia la tabla para realizar unión
  
  #dat.oges.sub <- subset(dat.oges, cod_canton != 999)
  #dat.oges.sub1 <- subset(dat.oges1, cod_canton != 999)
  #dat.oges.sub2 <- subset(dat.oges2, cod_canton != 999)
  
  #dim(dat.oges.sub) #Deben ser 82 cantones
  
  #Organiza la tabla en función del código del cantón
  #dat.oges.sub  <- dat.oges.sub[order(dat.oges.sub$cod_canton),]
  #dat.oges.sub1 <- dat.oges.sub1[order(dat.oges.sub1$cod_canton),]
  #dat.oges.sub2 <- dat.oges.sub2[order(dat.oges.sub2$cod_canton),]
  
  dat.oges1
  
  dat.oges.sub  <- dat.oges[order(dat.oges$cod_canton),]
  dat.oges.sub1 <- dat.oges1[order(dat.oges1$cod_canton),]
  dat.oges.sub2 <- dat.oges2[order(dat.oges2$cod_canton),]
  
  #----------------------------------------
  #Revisa la longitud de la data e introduce una fila para complerar 84
  if (dim(dat.oges.sub)[1]==83) { 
    x <- matrix(data = 0, nrow = 1, ncol = dim(dat.oges.sub)[2])  
    x <- as.data.frame(x)
    names(x) <- names(dat.oges.sub) 
    dat.oges.sub  <- rbind(dat.oges.sub,x) 
    } 

  if (dim(dat.oges.sub1)[1]==83) { 
    x <- matrix(data = 0, nrow = 1, ncol = dim(dat.oges.sub1)[2])  
    x <- as.data.frame(x)
    names(x) <- names(dat.oges.sub1) 
    dat.oges.sub1  <- rbind(dat.oges.sub1,x) 
  } 
  
  if (dim(dat.oges.sub2)[1]==83) { 
    x <- matrix(data = 0, nrow = 1, ncol = dim(dat.oges.sub2)[2])  
    x <- as.data.frame(x)
    names(x) <- names(dat.oges.sub2) 
    dat.oges.sub2  <- rbind(dat.oges.sub2,x) 
  } 
  #----------------------------------------  


  #--------------------------- ----------------------------------------------------
  #Lectura de datos formato ECG-UNA
  
  filename00 <- './data/minsa/minsa_cr_cantones_covid_dd.csv' #file.choose()
  dat.cantones <- read.table(filename00, header=TRUE, sep=",",
                             #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                             na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv
  
  #str(dat.cantones)
  
  #Organiza la tabla en función del código del cantón
  dat.cantones <- dat.cantones[order(dat.cantones$cod_canton),]
  dat.cantones
  
  #Setea la fecha del día de la corrida
  #fecha <- paste0("2020-04-11 19:00") # Si se desea colocar el día específico
  dat.cantones$fecha <- date.cc
  
  #Copia los datos desde OGES a ECG-UNA Tabla
  dat.cantones$casos_confirmados <- dat.oges.sub[,dim(dat.oges.sub)[2]]
  dat.cantones$casos_recuperados <- dat.oges.sub1[,dim(dat.oges.sub1)[2]]
  dat.cantones$casos_muertes     <- dat.oges.sub2[,dim(dat.oges.sub2)[2]]
  
  dat.cantones$casos_activos <- dat.cantones$casos_confirmados - 
                                dat.cantones$casos_muertes -
                                dat.cantones$casos_recuperados
    
  #---------------------------------------
  #Convierte NA to 0
  dat.cantones$casos_muertes[is.na(dat.cantones$casos_muertes) == 0]
  #dat.cantones
  
  #---------------------------------------
  #Convierte los valores de 0 a NA
  #dat.cantones[is.na(dat.cantones)] <- 0
  #dat.cantones$casos_confirmados[dat.cantones$casos_confirmados == 0] <- ""
  #dat.cantones$casos_muertes[dat.cantones$casos_muertes == 0] <- ""
  #dat.cantones$casos_recuperados[dat.cantones$casos_recuperados == 0] <- ""
  
  #dat.cantones
  
  #---------------------------------------
  #mantiene la estructura de los campo
  dat.cantones$casos_confirmados <- as.integer(dat.cantones$casos_confirmados)
  dat.cantones$casos_muertes <- as.integer(dat.cantones$casos_muertes)
  dat.cantones$casos_recuperados <- as.integer(dat.cantones$casos_recuperados)
  #dat.cantones
  
  #---------------------------------------------------------------------------
  #Sobre escribe la tabla CSV con los valores actualizados
  
  #write.table(dat.cantones, file = filename00, sep=",", row.names=FALSE, na = "", quote=FALSE) 
  #print("Actualización Hecha: Archivo CR CANTONES COVID dd\n")
  
  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------
  #Actualiza la tabla acumulada
  
  
  filename01 <- './data/minsa/minsa_cr_cantones_covid_ts.csv' #file.choose()
  dat.cantones.ts <- read.table(filename01, header=TRUE, sep=",",
                                #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                                na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv
  
  #mantiene la estructura de los campo
  dat.cantones.ts$fecha <- as.POSIXct(dat.cantones.ts$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo
  
  last.id <- dat.cantones.ts[dim(dat.cantones.ts)[1],"id"]
  last.id
  
  #dat.cantones$casos_confirmados <- as.integer(dat.cantones$casos_confirmados)
  #dat.cantones$casos_muertes <- as.integer(dat.cantones$casos_muertes)
  #dat.cantones$casos_recuperados <- as.integer(dat.cantones$casos_recuperados)
  #str(dat.cantones.ts)
  
  #Consulta si el proceso ya fue realizado, mediante un filtro de fecha
  dat.ss <- subset(dat.cantones.ts, fecha == date.cc)
  #dat.ss
  
  #datos día anterior
  dat.da <- subset(dat.cantones.ts, fecha == date.c0)
  #dat.da
  
  #Actualiza valores de casos nuevos
  dat.cantones$cc_nuevos <- dat.cantones$casos_confirmados - dat.da$casos_confirmados
  
  #dim(dat.ss)[1] #Registro en la fecha
  
  #Si no hay registros en la fecha realiza el proceso
  if (dim(dat.ss)[1] == 0){
    
    dat.cantones$id <-  last.id + dat.cantones$id # Actualiza el id
    
    dat.new <- dplyr::bind_rows(dat.cantones.ts, dat.cantones)# rbind both data.frame
    
    #Sobre escribe la tabla CSV con los valores actualizados
    write.table(dat.new, file = filename01, sep=",", dec=".", row.names=FALSE, na = "", quote=FALSE)
    print("Actualización Hecha: Archivo CANTONES COVID ts\n")
    
    dat.cantones$id <- seq(1:84) #Recalcula los valores de id
    write.table(dat.cantones, file = filename00, sep=",", dec=".",row.names=FALSE, na = "", quote=FALSE) 
    print("Actualización Hecha: Archivo CANTONES COVID ts\n")
    
  }
  
  #View(dat.new)
  
}

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#Procesa datos Generales desde OGES
#Abre datos desde OGES CSV

if (class(mtry3) != "try-error") {
  
  wb <- paste0("./data/OGES/",mm,"_",dd,"_CSV_GENERAL.csv")
  
  dat.oges.general <- read.csv(wb,header=TRUE, sep=key,
                               na.strings="NA", dec=deci, strip.white=TRUE)
  
  dat.oges.general
  #Convierte en el formato de fecha
  #cambia los nombres de la tabla
  str(dat.oges.general)
  
  names(dat.oges.general) <-   c("Fecha","positivos","nue_posi","conf_lab","conf_nexo","muj_posi","hom_posi",
                                 "extranj_posi","costar_posi","investig_posi","adul_posi",
                                 "am_posi","menor_posi","eda_ignor_posi","edmin_posi","edmax_posi","promedio",
                                 "descartados","nue_descar",
                                 "fallecidos","nue_falleci","muj_fall","hom_fall","emin_fall","emax_fall","promedio_fall",
                                 "adult_fall","am_fall","menor_fall",
                                 "hospital","nue_hospi",
                                 "salon","nue_salon","UCI","nue_UCI","remin_UCI",
                                 "remax_UC","promedio_UCI",
                                 "RECUPERADOS","NUE_RECUP","MUJ_RECUP",
                                 "HOM_RECUP","ADUL_RECUP","AM_RECUP","MENOR_RECUP","EDA_IGNO_RECUP",
                                 "PROMEDIO_RECUP",
                                 "EDMIN_RECUP","EDMAX_RECUP",
                                 "Muestras","MUEST_NEW","DIA_COVID19",
                                 "afec_posi","afec_recu","afec_fall","activos","nue_acti","muj_acti","hom_acti","adul_acti",
                                 "am_acti","menor_acti","eda_igno_acti","edmin_acti","edmax_acti","promedio_acti", "letalidad",
                                 "por_recup","por_activ","transpor",
                                 "HSJD_UCI","HSJD_SALON","CEACO_UCI","CEACO_SALON","HCG_UCI","HCG_SALON","HMEX_UCI",
                                 "HMEX_SALON","HNN_UCI","HNN_SALON","HEB_UCI","HEB_SALON"
)
  
  dat.oges.general$letalidad  <- as.double(dat.oges.general$letalidad)
  
  dat.oges.general$Fecha <- as.POSIXct(dat.oges.general$Fecha, format="%d/%m/%Y")
  #dat.oges.general$Fecha <- as.POSIXct(dat.oges.general$Fecha, format="%Y-%m-%d") 
  str(dat.oges.general)
  
  #Solo casos con fecha
  dat.oges.general <- dat.oges.general[complete.cases(dat.oges.general[ , "Fecha"]),]
  
  
  #Solo toma el valor de la última fecha
  dat.oges.general.s <- subset(dat.oges.general, Fecha == date.c1)
  dat.oges.general.s
  str(dat.oges.general.s)
  
  #---------------------------------------
  #Convierte NA to 0
  dat.oges.general.s[is.na(dat.oges.general.s) == 0]
  dat.oges.general.s
  
  #------------------------------------------
  #Carga la tabla ECG-UNA minsa
  
  filename02 <- './data/minsa/minsa_cr_covid_dd.csv' #file.choose()
  dat.cr <- read.table(filename02, header=TRUE, sep=",",
                       #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                       na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv
  
  #dat.cr 
  dat.cr$letalidad <- as.double(dat.cr$letalidad)
  #str(dat.cr)
  
  #Determina el ID
  date.ini <- as.POSIXct("2020-03-04 19:00", format="%Y-%m-%d %H:%M")
  
  
  #---------------------------------------------------------------------------------
  #Determina los campos (Siempre usa el última fecha)
  
  dat.cr$id <- as.numeric(date.cc - date.ini)
  
  dat.cr$ObjectId01 <- 1
  
  dat.cr$fecha              <- date.cc
  
  dat.cr$casos_descartados    <- dat.oges.general.s[dim(dat.oges.general.s)[1], "descartados"]
  dat.cr$casos_confirmados    <- dat.oges.general.s[dim(dat.oges.general.s)[1], "positivos"]
  
  dat.cr$conf_lab             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "conf_lab"]
  dat.cr$conf_nexo            <- dat.oges.general.s[dim(dat.oges.general.s)[1], "conf_nexo"]
  
  dat.cr$casos_muertes        <- dat.oges.general.s[dim(dat.oges.general.s)[1], "fallecidos"]
  
  dat.cr$casos_recuperados    <- dat.oges.general.s[dim(dat.oges.general.s)[1], "RECUPERADOS"]
  
  dat.cr$cc_hospit            <- dat.oges.general.s[dim(dat.oges.general.s)[1], "hospital"]
  
  dat.cr$cc_ci                <- dat.oges.general.s[dim(dat.oges.general.s)[1], "UCI"]
  
  dat.cr$casos_mujeres        <- dat.oges.general.s[dim(dat.oges.general.s)[1], "muj_posi"]
  
  dat.cr$casos_hombres        <- dat.oges.general.s[dim(dat.oges.general.s)[1], "hom_posi"]
  
  dat.cr$casos_extranjeros    <- dat.oges.general.s[dim(dat.oges.general.s)[1], "extranj_posi"]
  
  dat.cr$casos_nacionales     <- dat.oges.general.s[dim(dat.oges.general.s)[1], "costar_posi"]
  
  dat.cr$casos_investigacion  <- dat.oges.general.s[dim(dat.oges.general.s)[1], "investig_posi"]
  
  dat.cr$cc_adult             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "adul_posi"]
  
  dat.cr$cc_adult_may         <- dat.oges.general.s[dim(dat.oges.general.s)[1], "am_posi"]
  
  dat.cr$cc_menores           <- dat.oges.general.s[dim(dat.oges.general.s)[1], "menor_posi"]
  
  dat.cr$cc_edad_nc           <- dat.oges.general.s[dim(dat.oges.general.s)[1], "eda_ignor_posi"]
  
  dat.cr$delta                <- dat.oges.general.s[dim(dat.oges.general.s)[1], "positivos"] -
                                 dat.oges.general[(dim(dat.oges.general)[1]-1), "positivos"]   
                                 #Casos actuales - #Casos anteriores     
  
  dat.cr$cc_activos            <- dat.oges.general.s[dim(dat.oges.general.s)[1], "positivos"] -
                                  dat.oges.general.s[dim(dat.oges.general.s)[1], "fallecidos"] -
                                  dat.oges.general.s[dim(dat.oges.general.s)[1], "RECUPERADOS"]
  
  dat.cr$muestras             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "Muestras"]
  
  dat.cr$dia_covid19          <- dat.oges.general.s[dim(dat.oges.general.s)[1], "DIA_COVID19"]
  
  dat.cr$cr_muj               <- dat.oges.general.s[dim(dat.oges.general.s)[1], "MUJ_RECUP"]
  dat.cr$cr_hom               <- dat.oges.general.s[dim(dat.oges.general.s)[1], "HOM_RECUP"]
  dat.cr$cr_adult             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "ADUL_RECUP"]
  dat.cr$cr_adult_may         <- dat.oges.general.s[dim(dat.oges.general.s)[1], "AM_RECUP"]
  dat.cr$cr_menores           <- dat.oges.general.s[dim(dat.oges.general.s)[1], "MENOR_RECUP"]
  dat.cr$cr_edad_nc           <- dat.oges.general.s[dim(dat.oges.general.s)[1], "EDA_IGNO_RECUP"]
  
  dat.cr$cd_muj             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "muj_fall"]
  dat.cr$cd_hom             <- dat.oges.general.s[dim(dat.oges.general.s)[1], "hom_fall"]
  
  
  dat.cr$letalidad          <- dat.oges.general.s[dim(dat.oges.general.s)[1], "letalidad"]
  
  dat.cr$muestr_n           <- dat.oges.general.s[dim(dat.oges.general.s)[1], "MUEST_NEW"]
  
  dat.cr$cc_salon	          <- dat.oges.general.s[dim(dat.oges.general.s)[1], "salon"]
  
  dat.cr$cc_hospi_nv	              <- dat.oges.general.s[dim(dat.oges.general.s)[1], "nue_hospi"]
  dat.cr$cc_salon_nv	              <- dat.oges.general.s[dim(dat.oges.general.s)[1], "nue_salon"]
  dat.cr$cc_uci_nv	                <- dat.oges.general.s[dim(dat.oges.general.s)[1], "nue_UCI"]
  
  
  dat.cr
  dim(dat.cr)
  #---------------------------------------------------------------------------------
  
  #Sobre escribe la tabla CSV con los valores actualizados
  write.table(dat.cr, file = filename02, sep=",", row.names=FALSE, na = "", quote=FALSE)
  
  print("Actualización Hecha: Archivo CR COVID dd \n")
  
  #---------------------------------------------------------------------------
  #CR Covid-19
  #Actualiza la tabla acumulada
  
  
  filename03 <- './data/minsa/minsa_cr_covid_ts.csv' #file.choose()
  dat.cr.ts <- read.table(filename03, header=TRUE, sep=",",
                          #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                          na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv
  
  #mantiene la estructura de los campo
  dat.cr.ts$fecha <- as.POSIXct(dat.cr.ts$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo
  
  #dat.cantones$casos_confirmados <- as.integer(dat.cantones$casos_confirmados)
  #dat.cantones$casos_muertes <- as.integer(dat.cantones$casos_muertes)
  #dat.cantones$casos_recuperados <- as.integer(dat.cantones$casos_recuperados)
  str(dat.cr.ts)
  
  drops <- c('ObjectId01') # list of col names
  dat.cr <- dat.cr[,!(names(dat.cr) %in% drops)] #remueve las columnas en la lista anterior
  
  #dat.cr$object01 <- dat.cr$id
    
  #Consulta si el proceso ya fue realizado, mediante un filtro de fecha
  dat.ss <- subset(dat.cr.ts, fecha == date.cc)
  dat.ss
  
  dim(dat.ss)[1] #Registro en la fecha
  
  #Si no hay registros en la fecha realiza el proceso
  if (dim(dat.ss)[1] == 0){
    dat.cr.new <- dplyr::bind_rows(dat.cr.ts, dat.cr)# rbind both data.frame
    #Sobre escribe la tabla CSV con los valores actualizados
    write.table(dat.cr.new, file = filename03, sep=",", row.names=FALSE, na = "", quote=FALSE)
    print("Actualización Hecha: Archivo CR TS COVID\n")
  }
  
} else { message("File doesn't exist, please check")}
#---------------------------------------------------------------------------
print("Hecho.\n")


#---------------------------------------------
#Remueve todas las variables
rm(list = setdiff(ls(), lsf.str()))


