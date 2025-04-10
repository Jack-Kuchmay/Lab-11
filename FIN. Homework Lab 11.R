install.packages("sp")
install.packages("terra")
install.packages("raster")
install.packages("gstat")
install.packages("animation")
install.packages("RColorBrewer")
library("sp")
library("terra")
library("raster")
library("gstat")
library("animation")
library("RColorBrewer")

setwd("C:/Users/jacks/OneDrive/Documents/AGR 333 Labs/AGR333 Lab 11")
getwd()

DEM <- rast('Shale_Hills_DEM.tif')

ws <- vect('./Watershed_Boundary/wshd_nad83_edit.shp')

plot(DEM,
     xlab = "longitude", ylab = "latitude",
     main = "DEM"
)
plot(ws, add = TRUE)
sm <- read.csv("SM_10cm_2010.csv")  

library(dplyr)

sm <- sm %>% select(-SID)

coordinates(sm) <- ~ X+Y

plot(DEM,
     xlab = "Easting (m)", ylab = "Northing (m)",
     main = "Shale Hills Elevation(m)"
)
plot(sm, add = T, pch = 16)

par(xpd = TRUE) 
legend('topright', legend = 'Soil Moisture Station', pch = 16)
par(xpd = FALSE) 

display.brewer.pal(3, 'GnBu')
col.pal <- brewer.pal(3, 'GnBu')

head(sm)

intervals <- seq(min(sm@data[,1]), max(sm@data[,1]), length = 8)

intervals

idx.col <- cut(sm@data[,1], intervals, include.lowest = T)

idx.col




col.pal[idx.col]

plot(DEM, col = brewer.pal(9, 'Greys'),
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col=col.pal[idx.col], add = T)
plot(ws, add=T)

leg <- paste0(round(intervals[-8], digits = 2), '-',
              round(intervals[-1], digits = 2))
legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')

intervals <- quantile(sm@data[,1], seq(0,1,length=8), na.rm = T)

intervals <- quantile(sm@data[,2], seq(0,1,length=8), na.rm = T)
intervals
idx.col <- cut(sm@data[,2], intervals, include.lowest = T)
idx.col
col.pal[idx.col]
plot(DEM, col = brewer.pal(9, 'Greys'),
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col=col.pal[idx.col], add = T)
plot(ws, add=T)
leg <- paste0(round(intervals[-8], digits = 2), '-',
              round(intervals[-1], digits = 2))
legend('bottomright', legend = leg, pch = 16, col = col.pal, title = 'Soil Moisture [%]')


ppt <- read.csv("SHCZO_ppt_2010.csv")







ani.options(interval = .6) 

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

for(i in 1:ncol(sm@data)){
  intervals <- quantile(sm@data[,i], seq(0,1,length=8), na.rm = T)
  intervals
  idx.col <- cut(sm@data[,i], intervals, include.lowest = T)
  idx.col
  col.pal[idx.col]
  plot(DEM, col = brewer.pal(9, 'Greys'),
       xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
  plot(sm, pch = 16, col=col.pal[idx.col], add = T)
  plot(ws, add=T)
  leg <- paste0(round(intervals[-8], digits = 2), '-',
                round(intervals[-1], digits = 2))
  legend('bottomright', legend = leg, pch = 16, col = col.pal, title = 'Soil Moisture [%]')
  
  barplot(height = ppt$Total_Precip_mm, names = ppt$TmStamp)
  idx.v <- which(ppt$TmStamp == gsub('X', '', colnames(sm@data)[i]))
  abline(v = idx.v, col = 'red') 
  ani.pause()
}


