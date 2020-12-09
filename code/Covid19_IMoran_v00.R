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
filename <- './data/minsa/minsa_cr_cantones_covid_ts.csv' #file.choose()

dat.canton <- read.table(filename, header=TRUE, sep=",",
                  #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                  na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

dat.canton$fecha <- as.POSIXct(dat.canton$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo

#-------------------------------------------------------

ind.salud <- read.table('./data/minsa/tab/indicadores_basicos_salud_2013.csv', header=TRUE, sep=",",
                        #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                        na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

ind.salud

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
ss <- 1 #Se requiere salida por dia


#---------------------------------------------------------
#Genera union de datos para trabajo

#Por favor indicar el día 
i <- frango # POR DEFECTO ****ULTIMO DIA******

ff <-   format(as.Date(ini.f+(i),format="%Y-%m-%d"), format = "%Y_%m_%d") 
ff <- as.character(ff)

dat.canton.s <- subset(dat.canton, fecha == ini.date+(i*86400)) #i por segundos en un día
dat.canton.s

cr.cantones.0 <- geo_join(cr_cantones, dat.canton.s, by = 'cod_canton', how = "left") 

#i=0
#Limpia la tabla
drops <- c('id','cod_canton.1','canton.1') # list of col names
cr.cantones.0 <- cr.cantones.0[,!(names(cr.cantones.0) %in% drops)] #remueve las columnas

#Agrega los indicadores de salud por canton
cr.cantones.0 <- geo_join(cr.cantones.0, ind.salud , by = 'cod_canton', how = "left") 

#Limpia tabla
drops <- c("ori_toponi","cod_provin.1","provincia.1","cod_canton.1","canton.1")# list of col names
cr.cantones.0 <- cr.cantones.0[,!(names(cr.cantones.0) %in% drops)] #remueve las columnas

#Organiza las entidades por cod_canton y fecha
cr.cantones.0  <- cr.cantones.0[order(cr.cantones.0$cod_canton,cr.cantones.0$fecha),] 

cr.cantones.0@data
#---------------------------------------------------------

xy <- coordinates(cr.cantones.0)

plot(cr.cantones.0)
#points(xy)
#text(cr.cantones.0, 'canton', cex=0.5)


#Adyacencia de los polígonos
library(spdep)

w <- poly2nb(cr.cantones.0, row.names=cr.cantones.0$cod_canton)
class(w)
summary(w)


#Gráfico de los enlaces entre polígonos
plot(cr.cantones.0, col='white', border='blue', lwd=2) +
  plot(w, xy, col='red', lwd=2, add=TRUE)

#---------------------------------------------------------
#matriz de ponderaciones espaciales
wm <- nb2mat(w, style='B')
wm

#Coloca los códigos de los cantones en la matriz de ponderacion espacial
#row.names(wm) <- cr.cantones.0$cod_canton
#colnames(wm) <- cr.cantones.0$cod_canton

#Opción 2 con los nombres de los cantones
row.names(wm) <- cr.cantones.0$canton
colnames(wm) <- cr.cantones.0$canton



#---------------------------------------------------------
#I’ Moran

y <- cr.cantones.0$casos_confirmados
ybar <- mean(y)


ww <-  nb2listw(w, style='B')
ww

moran(scale(y), ww, n=length(ww$neighbours), S0=Szero(ww))

moran(y, ww, n=length(ww$neighbours), S0=Szero(ww))

Szero(ww)

#Pruebas de significancia estadística para el I’ Moran
moran.test(scale(y), ww, randomisation=FALSE)


moran.mc(scale(y), ww, nsim=99)


#Diagrama de dispersión de I’ Moran

n <- length(y)
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))

ms <- ms[ms[,3] > 0, ] #Se remueven los valor iguales a 0.

#Se calculan los valores de los promedios de los vecinos.
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'y espacialmente retardado')
head(ams)

#Opción 2 con los nombres de los cantones
row.names(ams) <- cr.cantones.0$canton
ams

#Gráfico de I Moran

rwm <-  mat2listw(wm, style='W')
rwm

moran.plot(y, rwm, labels = T)

nombre.pIM <- "./UE_IMoran.pdf"
dev.print(pdf,
          file=nombre.pIM,
          width=8, height=7, pointsize = 18) #, pointsize=0.5 Setea el tama?o del punto
dev.off()
system2('pdfcrop', c(nombre.f5,nombre.f5))



