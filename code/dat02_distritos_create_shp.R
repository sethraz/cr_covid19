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
library(rgdal)
library(raster)
library(sp)
library(dplyr)
library(readxl)

#-------------------------------------------------------
#Establece el mes y día
#aaaa <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%Y")
#mm <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%m")
#dd <- format(as.Date(Sys.Date(),format="%Y-%m-%d"), format = "%d")

#-------------------------------------------------------
#Setea la fecha de manera manual
aaaa <- "2020"
mm <- "12"
dd <- "08"

date.cc <- as.POSIXct(paste0(aaaa,"-",mm,"-",dd), format="%Y-%m-%d") # Formato de tiempo

#Abre datos desde OGES
#-------------------------------------------------------
#url.01 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_INFORMACION_DISTRITAL.xlsx")

url.01 <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/",mm,"_",dd,"/",mm,"_",dd,"_EXCEL_GENERAL.xlsx")

#mtry.01 <- try(download.file(url.01, destfile = paste0("./data/OGES/",mm,"_",dd,"_INFORMACION_DISTRITAL.xlsx"),mode="wb"))

mtry.01 <- try(download.file(url.01, destfile = paste0("./data/OGES/",mm,"_",dd,"_EXCEL_GENERAL.xlsx"),
                             mode="wb"))

#wb <- paste0("./data/OGES/",mm,"_",dd,"_INFORMACION_DISTRITAL.xlsx")
wb <- paste0("./data/OGES/",mm,"_",dd,"_EXCEL_GENERAL_add.xlsx")
df <- read_excel(wb, sheet = 4,skip = 2)
df

str(df)
names(df) 

#-------------------------------------------------------
###Ciclo que registra los datos
frango <- dim(df)[1]
frango


#Crea la data.frame
rep.dat <- data.frame(matrix(ncol=6,nrow=0, 
              dimnames=list(NULL, c("cod_dis","fecha","cc_pacc","cc_recup","cc_deces","cc_activ"))))

rep.dat$fecha <- as.POSIXct(rep.dat$fecha, format="%Y-%m-%d")

#Pasa los datos a la data frame
for (i in 1:frango){
  #t01 <- try(as.integer(substr(df[i,"Distrito"], 1, 5))) #Casos con cod y nombres juntos
  t01 <- try(as.integer(df[i,"cod_dis"]))
  
  if (class(t01) != "try-error") {
    rep.dat[i,"cod_dis"]   <-  as.integer(df[i,"cod_dis"])
    rep.dat[i,"cc_pacc"]   <-  as.integer(df[i,"ACUMULADOS"])
    rep.dat[i,"cc_recup"]  <-  as.integer(df[i,"RECUPERADOS"])
    rep.dat[i,"cc_deces"]  <-  as.integer(df[i,"FALLECIDOS"])
    rep.dat[i,"cc_activ"]  <-  as.integer(df[i,"ACTIVOS"])
    
    rep.dat[i,"fecha"] <- date.cc
    
  }
  else rep.dat[i,"cod_dis"] <- NA
}

#Pasa de Na a 0 (caso de las columnas)
rep.dat$cc_pacc[is.na(rep.dat$cc_pacc)] <- 0
rep.dat$cc_deces[is.na(rep.dat$cc_deces)] <- 0
rep.dat$cc_recup[is.na(rep.dat$cc_recup)] <- 0
rep.dat$cc_activ[is.na(rep.dat$cc_activ)] <- 0

#----------------------------------------------
#Elimina las entradas sin Codigo
rep.dat <- na.omit(rep.dat) #Elmina filas sin datos
rep.dat <- rep.dat[with(rep.dat, order(cod_dis)),] #Ordena según codigo de distrito
rep.dat

#View(rep.dat)
#-----------------------------------------------------------------------------
## En caso de que sea tabla
##Lectura de datos de la tabla base
#filename00 <- './data/minsa/minsa_cr_distritos_null.csv' #file.choose()
#dat.dis <- read.table(filename00, header=TRUE, sep=",",
#                     #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
#                     na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

#dat.dis
#coordinates(dat.dis) <- c("longitude", "latitude")
#str(dat.dis)


#-----------------------------------------------------------------------------
#Carga shapefile de cantones de Costa Rica
dat.dis <- readOGR("./data/shp/cr_distrito_null.shp","cr_distrito_null", 
                       use_iconv=TRUE, encoding="UTF-8")

proj4string(dat.dis)


#Combina la información de las estadísticas con el Shapefile de cantones de CR
library(tigris)

#Realiza el Join
dat.distritos <- geo_join(dat.dis, rep.dat, by_sp='codigo_dta', by_df = 'cod_dis', how = "left") 

#Organiza los datos
#dat.distritos@data <-  dat.distritos@data[with(dat.distritos@data, order(cod_dis)),] 

#Ajusta el id y el nombre de la fila
#dat.distritos@data$objectid <- 1:dim(dat.distritos@data)[1]
#rownames(dat.distritos@data) <- 1:dim(dat.distritos@data)[1]

#dat.distritos@data

#remueve las columnas no deseadas
drops <- c('objectid_1','cod_dis.1') # list of col names
dat.distritos@data <- dat.distritos@data[,!(names(dat.distritos@data) %in% drops)] #remueve las columnas

str(dat.distritos@data)

    
for (j in 1:dim(dat.distritos)[1]) {
  dat.distritos@data[j,"fecha"] <- date.cc  
}


#-----------------------------------------------------------------------------
#CALCULO de CASOS NUEVOS
#Carga shapefile de casos del día anterior

file.2 <- paste0("cr_distrito_covid19_",as.character(as.Date(date.cc)-1))
#file.2 <- paste0("cr_distrito_covid19_",'2020-07-19')
file.2 <- gsub("-", "_", file.2)

dat.dia.ant <- readOGR(paste0("./data/shp/cr_covid19_dist/",file.2,".shp"),file.2, 
                       use_iconv=TRUE, encoding="UTF-8")

#Casos nuevos
dat.distritos$cc_nuevos <- dat.distritos$cc_pacc - dat.dia.ant$cc_pacc 

#View(dat.distritos@data)

#-----------------------------------------------------------------------------

#View(dat.distritos@data)

#Convierte a nueva proyección
#proj4string(dat.distritos) <- CRS('+proj=longlat +datum=WGS84 +no_defs')
proj4string(dat.distritos)

dat.distritos.sirgas <- spTransform(dat.distritos, 
      CRS("+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

proj4string(dat.distritos.sirgas)

#plot(dat.distritos.sirgas)

#---------------------------------------------
# Save el shapefile with the data merge

file.out <- paste0("cr_distrito_covid19_today")
file.out2 <- paste0("cr_distrito_covid19_",aaaa,'_',mm,'_',dd)

#Para proceso de actualizacion
writeOGR(dat.distritos.sirgas, "./data/shp/cr_covid19_dist", 
         layer= file.out , driver="ESRI Shapefile", overwrite_layer=T)

#Para registro historico
writeOGR(dat.distritos.sirgas, "./data/shp/cr_covid19_dist", 
         layer= file.out2 , driver="ESRI Shapefile", overwrite_layer=T)

