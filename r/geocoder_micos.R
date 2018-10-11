setwd("C:/Users/silvia/Documents/AzueroSpiderMonkeys")

require(ggmap)
require(sp)
library(maptools)
library(raster)
require(dismo)
require(sdmtools)
library(plyr)
library(magrittr)

loc<-read.csv(file="AbsenceLocations.csv",header=F, sep=",") 
colnames(loc) <- c("point", "locality", "municipality", "province", "ateles")

#con provincia
loc2<-paste0(loc$locality, ", ", loc$province, ", Panama") %>% as.data.frame

colnames(loc2)<-"localityfull"
cord2<-dismo::geocode(loc2$localityfull, oneRecord=TRUE, progress="text")
#Sin provincia
loc3<-as.data.frame(paste(loc$localidad, "Panama", sep=", "))
colnames(x=loc3)<-"localidades"
cord3<-geocode(loc3$localidades,oneRecord=TRUE,progress="text")

alt <- getData('alt', country='PAN')
adm <- getData('GADM', country='PAN', leve=2)
mar<-(adm[adm$NAME_1=="Herrera",])
mar<-(adm[adm$NAME_1 %in% c("Herrera","Veraguas","Los Santos") ,])
maralt<-crop(alt,mar)

puntos<-as.data.frame(cbind(cord2$longitude,cord2$latitude))
colnames(puntos)<-c("lon","lat")
nodat<-which(is.na(puntos$lon)==TRUE) # encuentra NA
puntos2<-puntos[-nodat,] #elimina NA
nodat<-which(puntos2$lon>-80) # encuentra -80
puntos2<-puntos2[-nodat,] #elimina -80
#nodat<-which(puntos2$lon<= -82) # encuentra -80
#puntos2<-puntos2[-nodat,] #elimina -82


coordinates(puntos2)<-c("lon","lat") # 
latlon<- CRS("+proj=longlat +datum=WGS84")
proj4string(puntos2)<- latlon

persp(maralt, exp=0.3,phi=35, xlab="Longitude", ylab="Latitude", zlab="Elevation")

plot(maralt)
plot(mar,add=T)
plot(puntos2,add=T)

