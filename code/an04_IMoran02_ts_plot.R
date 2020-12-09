#Librerias
library(forecast)

library(fable) #Nueva version forescast
library(feasts)
library(tsibbledata)
library(lubridate)

library(astsa) # ** SEE FOOTNOTE
library(stats)
library(zoo)
library(xts)
library(moments)

require(dplyr)
#----------------------------------------
library(ggplot2)
library(ggthemes)

#----------------------------------------
##Funciones

#Evalua si un numero es par o impar
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

#Establece limite mínimo y máximo
min.max <- function(dmin,dmax){
  if (is.odd(dmin)) {
    dmin <- (dmin )/10
  } else {dmin <- (dmin)/10}
  if (is.odd(dmax)) {
    dmax <- (dmax )/10
  } else {dmax <- (dmax)/10}
  
  list(lmin = dmin, lmax  = dmax)
}

#-------------------------------------------------------------------------------
#Lectura de datos
#f.name <- c('./data/minsa/tab/casos_acc/IMoran_ts_m1.csv',
#            './data/minsa/tab/casos_acc/IMoran_ts_m2.csv',
#            './data/minsa/tab/casos_acc/IMoran_ts_m3.csv',
#            './data/minsa/tab/casos_acc/IMoran_ts_m4.csv',
#            './data/minsa/tab/casos_acc/IMoran_ts_m5.csv',
#            './data/minsa/tab/casos_acc/IMoran_ts_m6.csv')

f.name <- c('./data/minsa/tab/casos_nvs/IMoran_ts_m1.csv',
            './data/minsa/tab/casos_nvs/IMoran_ts_m2.csv',
            './data/minsa/tab/casos_nvs/IMoran_ts_m3.csv',
            './data/minsa/tab/casos_nvs/IMoran_ts_m4.csv',
            './data/minsa/tab/casos_nvs/IMoran_ts_m5.csv',
            './data/minsa/tab/casos_nvs/IMoran_ts_m6.csv')


for (i in 1:length(f.name)) {
  dat <- read.table(f.name[i], header=TRUE, sep=",",
                         #colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
                         na.strings="NA", dec=".", strip.white=TRUE) # BCCR_dolares.csv

  dat$fecha <- as.POSIXct(dat$fecha, format="%Y-%m-%d %H:%M") # Formato de tiempo
  dat

  #Máximo y mínimo
  lmin.1 <- min(dat$imoran, na.rm = T)
  lmax.1 <- max(dat$imoran, na.rm = T)

  lmin.1 <- round(floor(lmin.1 * 10)  , digits = 1)
  lmax.1 <- round(ceiling(lmax.1 * 10), digits =1)

  lmin.2 <- min(dat$pvalue, na.rm = T)
  lmax.2 <- max(dat$pvalue, na.rm = T)

  lmin.2 <- round(floor(lmin.2 * 10)  , digits = 1)
  lmax.2 <- round(ceiling(lmax.2 * 10), digits =1)

  limites1 <-  min.max(lmin.1,lmax.1)
  limites2 <-  min.max(lmin.2,lmax.2)

  limites1[1]
  limites1[2]
#-----------------------------------------------------
#Gráficos

#Rango para el gráfico
  l.range = c(as.numeric(limites1[1]),as.numeric(limites1[2]),
            as.numeric(limites2[1]),as.numeric(limites2[2]))

  #range = round(floor(lmin.1 * 100) / 100 - 0.1, digits = 1): (ceiling(lmax.1 * 100) / 100 + 0.1
  range = c(min(l.range),max(l.range))
  range

  IM.ts.plot <- ggplot(dat, aes(x = fecha, y = imoran)) +
      geom_line(size=1) +
      labs(xlab= "", ylab = expression("I Moran")) +
      #theme_light() +
      #theme_tufte() +
      #theme_classic() +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
      text = element_text(size = 14),
      panel.background = element_blank()) +
      labs(x = "Fecha", y = expression("Estadístico (I. Global)"), 
           title = paste0("I. Moran para el modelo ",i)) +
      #scale_x_datetime(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "1 month") +
      coord_cartesian(ylim =range) +
      scale_x_datetime(date_breaks = "15 days", date_labels = "%d-%m", date_minor_breaks = "1 day") +
      geom_point() +
  
      geom_point(mapping = aes(y = pvalue), size =1, color ="Red") +
      geom_line(mapping  = aes(y = pvalue), size =1, color ="Red") +
      #scale_x_date(name = "Fecha", labels = NULL) +
      scale_y_continuous(sec.axis = sec_axis(~./5, name = "p-valor (%)", 
             labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
      theme(
            #axis.title.y = element_text(color = "grey"),
            axis.title.y.right = element_text(color = "Red"),
            text = element_text(size = 14))


  plot(IM.ts.plot)
  #nom.plot   <- paste0("./graf/IMoran/casos_acc/imoran_ts_m",i,".pdf")
  #nom.plot.1 <- paste0("./graf/IMoran/casos_acc/imoran_ts_m",i,".png")
  
  nom.plot   <- paste0("./graf/IMoran/casos_nvs/imoran_ts_m",i,".pdf")
  nom.plot.1 <- paste0("./graf/IMoran/casos_nvs/imoran_ts_m",i,".png")
  
  dev.print(pdf,
          file=nom.plot,
          width=8, height=4, pointsize = 10) #, pointsize=0.5 Setea el tama?o del punto
  
  dev.print(nom.plot.1,
            #device=png, width=3240, res=300)
            device=png, width=2160, height=1080, res=300)
  
  dev.off()

}
