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
library(rvest)
library(magrittr)
#library(zoo)
#library(xts)

#Librerias espaciales
library(dplyr)
library(rgdal)
library(raster)
library(sp)
library(ggplot2)
library(spdep)
library(sf)
library(rgeos)
library(stringr)

#-------------------------------------------------------------------------------
#Lectura de datos

#--------------------------------------------------------
#Carga shapefile de cantones de Costa Rica
#cr.cantones.0 <- readOGR("./data/shp/cantones_covid19_ind.shp","cantones_covid19_ind", 
#                       use_iconv=TRUE, encoding="UTF-8")
#proj4string(cr_cantones.0)
#--------------------------------------------------------

#**** Si se ejecuta desde como fuente inicia aqui****

#cr.cantones.0$fecha <- as.POSIXct(cr.cantones.0$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo

#str(cr.cantones.0@data)


#cr <- readOGR("./data/shp/cr_covid19_null.shp","cr_covid19_null", 
#              use_iconv=TRUE, encoding="UTF-8")

#--------------------------------------------------------------
#Incorpora los elementos de la fecha

#aaaa <- format(as.Date(cr.cantones.0@data[1,"fecha"],format="%Y-%m-%d"), format = "%Y")
#mm   <- format(as.Date(cr.cantones.0@data[1,"fecha"],format="%Y-%m-%d"), format = "%m")
#dd   <- format(as.Date(cr.cantones.0@data[1,"fecha"],format="%Y-%m-%d"), format = "%d")



#------------------------------------------------------------
#------------------------------------------------------------
#LISA map of bivariate Moran's I spatial correlation

#---------------------------------------------------------
#Script con las funciones de I Moran y Simulate I Moran (Bivariate)
source("./code/IMoran_fun.R")


#---------------------------------------------------------
# Crea los directorios de las salidas
dir.out.plot <- paste0("./graf/IMoran/",tt,"/",modelo,"/")

#Revisa si existe el directorio de salida graficos
if (!file.exists(dir.out.plot)){
  dir.create(dir.out.plot) #Crea directorio si no existe
} 

#Revisa si existe el directorio de salida tablas
dir.out.tab <- paste0("./data/minsa/tab/",tt,"/")

#Revisa si existe el directorio de salida
if (!file.exists(dir.out.tab)){
  dir.create(dir.out.tab) #Crea directorio si no existe
} 

#---------------------------------------------------------
# Adjacency Matrix (Queen)

#Se evalua si la matriz de adyacencia existe
#if (i==0 | i==150){
if (i==150){
  nb <- poly2nb(cr.cantones.0, row.names=cr.cantones.0@data[,4]) #Cod_canton
  summary(nb)
  lw <- nb2listw(nb, style = "B", zero.policy = T)
  W  <- as(lw, "symmetricMatrix")
  W  <- as.matrix(W/rowSums(W))
  W[which(is.na(W))] <- 0
  
  #Coloca los c?digos de los cantones en la matriz de ponderacion espacial
  row.names(W) <- cr.cantones.0@data[,4] #Cod_canton
  colnames(W)  <- cr.cantones.0@data[,4] #Cod_canton
  
  #Opci?n 2 con los nombres de los cantones
  #row.names(W) <- cr.cantones.0$canton
  #colnames(W)  <- cr.cantones.0$canton
  
}


#======================================================
# Calculating the index and its simulated distribution
# for global and local values

m   <- moran_I(x, y, W) #Variable explicativa (X), Variable respuesta (y) y Matriz de pesos (W)
m[[1]] # global value

m_i <- m[[2]]  # local values
m_i

mod.1 <- simula_moran(x, y, W)

#Valores I Moran Locales
local_sims <- mod.1$local_sims
local_sims

#Para obtener el p-valor
global_sims <- mod.1$global_sims
p.value <- sum(abs(global_sims) > abs( m[[1]][1] )) / length(global_sims)  
p.value
  
# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig        <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

#----------------------------------------------------
#Construye IMoran Serie de Tiempo
destfile = c(paste0("./data/minsa/tab/",tt,"/IMoran_ts_",modelo,".csv"))
#rm(imoran.ts)
imoran.ts <- data.frame(cr.cantones.0@data[1,14],m[[1]],p.value)
names(imoran.ts) <- c("fecha","imoran","pvalue")

if(!file.exists(destfile)){
  write.csv(imoran.ts, file = destfile, row.names=FALSE,quote=FALSE)  
  
} else{
  dfRead <- read.csv(destfile) # read the file 
  dfRead$fecha <- as.POSIXct(dfRead$fecha, format="%Y-%m-%d %H:%M:%S") # Formato de tiempo
  
  all <- rbind(dfRead,imoran.ts)
  all
  # get only the non duplicate rows from the new data.frame
  nonDuplicate <- all[!duplicated(all)&c(rep(FALSE, dim(dfRead)[1]), rep(TRUE, dim(imoran.ts)[1])), ]
  nonDuplicate
  #Organiza registros
  nonDuplicate <- nonDuplicate[order(nonDuplicate$fecha, decreasing = FALSE ),] 
  nonDuplicate
  # append the file with the non duplicate rows
  write.table(nonDuplicate,destfile, row.names=F,na="NA",append=T,sep=",",quote=FALSE, col.names=F)
  }
  
remove(imoran.ts,destfile,dfRead,all,nonDuplicate) #Limpia las variables

#----------------------------------------------------

#Grafico de I Moran

# Identifying the LISA patterns
xp <- (x-mean(x))/sd(x)
yp <- (y-mean(y))/sd(y)


# Preparing for plotting Map
cr.cantones.sf     <- st_as_sf(cr.cantones.0)
cr.cantones.sf$sig <- sig

patron <- as.character( interaction(xp > 0, W%*%yp > 0) ) 
patron

patron <- patron %>% 
  str_replace_all("TRUE","Alto") %>% 
  str_replace_all("FALSE","Bajo")
patron[cr.cantones.sf$sig==0] <- "No significativo"
cr.cantones.sf$patron <- patron

# Plotting
map.1 <- ggplot(data=cr.cantones.sf, aes(fill=patron), color="NA") +
  geom_sf() +
  scale_fill_manual(name = "Patrón", 
                    values = c("red", "pink", "light blue", "dark blue", "grey95"),
                    labels = c("Alto - Alto","Alto - Bajo","Bajo - Alto","Bajo - Bajo","No signif.")) + 
  theme_minimal()

plot(map.1)

nom.plot1.map <- paste0("./graf/IMoran/",tt,"/",modelo,"/covid19_lisa_",modelo,"_map_",aaaa,mm,dd,".pdf")
nom.plot2.map <- paste0("./graf/IMoran/",tt,"/",modelo,"/covid19_lisa_",modelo,"_map_",aaaa,mm,dd,".png")

dev.print(pdf,
          file=nom.plot1.map,
          width=5, height=4, pointsize = 10) #, pointsize=0.5 Setea el tama?o del punto

dev.print(nom.plot2.map,
          #device=png, width=3240, res=300)
          device=png, width=2160, height=1080, res=300)

dev.off()

#-----------------------------------
#Scatter Plot

y_lag <- W%*%yp
ams <- cbind.data.frame(y_lag, xp, patron)
colnames(ams) <- c('y_lag', 'x', 'patron')
head(ams)
str(ams)

#Solo etiqueta aquellas unidades significativas
etiquetas <- array()
for (i in 1:length(patron) ) {
  if (patron[i] == "Alto.Alto"){
    etiquetas[i] <- as.character(cr.cantones.sf[i,"canton"])}
  else {etiquetas[[i]] <- ""}
}
etiquetas

#----------------------------------------
#Gr?fico cl?sico
#plot(ams[,2],ams[,1], xlab="x", ylab="Casos confirmados (Lagged)", 
#     main=paste0("I Moran: ", sprintf(m[[1]], fmt = '%#.3f')) )
#reg <- lm(ams[,1] ~ ams[,2])
#abline(reg, lwd=2)

#abline(h=mean(ams[,2]), lt=2)N
#abline(v=mean(yp), lt=2)
#text(ams, row.names(ams), pos=4, offset=0.3, cex=0.5)
#text(ams, etiquetas, pos=4, offset=0.1, cex=0.8)

#ams

#----------------------------------------------------
#Establece paleta de colores
colores <- c("#FF1D15","#56B4E9","#F1AB86","#0072B2","#636363")

#Cambia la expression de acuerdo al modelo usado 
expre.theme <- c(expression("Densidad de población " * (hab/km^2)), expression("IDS 2017"),
                 expression("IPS 2019"), expression("NBH 2019"), 
                 expression("Tasa de vacunación 2019"),expression("Densidad vial 2020"))

p01 <- ggplot(ams, aes(x, y_lag, colour=patron)) +
        geom_point() + 
        scale_colour_manual(name= "Patrón", values=colores,
                              labels = c("Alto - Alto","Alto - Bajo","Bajo - Alto","Bajo - Bajo", "No signif.")) +
        geom_smooth(method = 'lm', se = F, color="black", size=0.8) + 
        geom_hline(yintercept = 0, linetype = 'dashed') + 
        geom_vline(xintercept = 0, linetype = 'dashed') +
        theme_bw() +
        #labs(x = expression("Densidad de poblaci?n " * (hab/km^2)), y = "Casos confirmados (Retraso espacial)",
        #labs(x = expression("IDS 2017"), y = "Casos confirmados (Retraso espacial)",
        #labs(x = expression("IPS 2019"), y = "Casos confirmados (Retraso espacial)",
        #labs(x = expression("NBH 2019"), y = "Casos confirmados (Retraso espacial)", 
        #labs(x = expression("Tasa de vacunaci?n 2019"), y = "Casos confirmados (Retraso espacial)",
        labs(x = expre.theme[md.0], y = "Casos confirmados (Retraso espacial)", 
             title= paste0("I Moran: ", sprintf(m[[1]], fmt = '%#.3f')))+
        geom_text(
        label=etiquetas, 
        nudge_x = 0.1, nudge_y = 0.1, 
        check_overlap = F, show.legend = FALSE)

plot(p01)

nom.plot1 <- paste0("./graf/IMoran/",tt,"/",modelo,"/covid19_lisa_",modelo,"_sp_",aaaa,mm,dd,".pdf")
nom.plot2 <- paste0("./graf/IMoran/",tt,"/",modelo,"/covid19_lisa_",modelo,"_sp_",aaaa,mm,dd,".png")

dev.print(pdf,
          file=nom.plot1,
          width=8, height=4, pointsize = 10) #, pointsize=0.5 Setea el tama?o del punto

dev.print(nom.plot2,
          #device=png, width=3240, res=300)
          device=png, width=2160, height=1080, res=300)

dev.off()




#---------------------------------------------
# Save el shapefile with the data merge
str(cr.cantones.sf)

#Prepara temporal 
tmp <-as(cr.cantones.sf, 'Spatial')
tmpid <- sapply(slot(tmp, "polygons"), function(x) slot(x, "ID"))
row.names(tmp) <- tmpid


file.out <- paste0("cantones_covid19_IMoran_",modelo,"_",aaaa,mm,dd)
writeOGR(tmp, paste0("./data/shp/imoran/",tt,"/",modelo), layer= file.out , driver="ESRI Shapefile", overwrite_layer=T)

rm(tmp, tmpid, cr.cantones.sf)
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
print("Hecho.\n")

