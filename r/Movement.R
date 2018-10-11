rm(list = ls())
setwd("~/Achamek")

library(move)

#prepare data
data<-as.data.frame(read.csv("RelocEastGroup.csv", header=T))

data$t<-paste(data$Date, data$Time)
data$t<-as.character(data$t)
data$t<-as.POSIXct(data$t, format="%Y-%m-%d %H:%M", tz="UTC")

coordinates(data)<-c("POINT_X", "POINT_Y")
proj4string(data)<-CRS("+proj=utm +zone=19S +datum=WGS84")
data.t<-spTransform(data, CRS("+proj=longlat datum=WGS84"))
proj<-CRS("+proj=utm +zone=19S +datum=WGS84")
##create move stack object
##move stack by individuals
move<-move(x=data.t$POINT_X, y=data.t$POINT_Y, time=data.t$t, data=as.data.frame(cbind(data.t$Individual, data.t$Sex)),
                proj=proj, animal=data.t$Individual)

##save move object as data frame
library(data.table)
move.df<-summary(move)
df.move<-do.call(rbind, move.df)
df<-data.frame()
for (i in 1:ncol(df.move)){
  df<-do.call(rbind, df.move[,i])
  filename <- paste("moveEast_stats", i, sep="") 
  write.table(df, file=paste(filename, ".csv", sep=""))
}


#plot tracks as lines using google maps
library(ggmap)
require(rgdal)

move.t <- spTransform(x=move, CRSobj="+proj=aeqd", center=TRUE)
move_df <- as(move.t, "data.frame")
m <- get_map(bbox(extent(move.t)), zoom=21, maptype="terrain", source="google", color='color')
ggmap(m)+geom_path(data=move_df, aes(x=coords.x1, y=coords.x2))

#plot without google
a<-c(1,3,5:8, 17, 20) #individuals with more than 20 relocs
plot(move[[a]], type="l", xlab="lon", ylab="lat", col=c("brown2","brown2","aquamarine4",
                                                        "brown2","brown2","aquamarine4",
                                                        "brown2","brown2"), lwd=1)
legend(x="bottomleft", legend=c("Female", "Male"), col=c("brown2", "aquamarine4"), lwd=2,
       bty="n", cex=0.9)

##analyse basic movement metrics
stat<-as.data.frame(read.csv("MoveStats_East.csv", header=T))
as.factor(stat$Sex)
par(mfrow=c(1,2))
boxplot(stat$AverDist ~ stat$Sex, names=c("Females", "Males"), boxwex=0.4, 
        ylab="Distance (m)", xlim = c(0.5, 2.5), cex.lab=1.6, cex.axis=1.4, outline=F)
boxplot(stat$SDDist ~ stat$Sex, names=c("Females", "Males"), boxwex=0.4, 
        ylab="Step length (m)", xlim = c(0.5, 2.5))

boxplot(stat$AverSpeed ~ stat$Sex, names=c("Females", "Males"), boxwex=0.4, 
        ylab="Speed (m/s)", xlim = c(0.5, 2.5), cex.lab=1.6, cex.axis=1.4, outline=F)
boxplot(stat$VarSpeed ~ stat$Sex, names=c("Females", "Males"), boxwex=0.4, 
        ylab="Speed (m/s)", xlim = c(0.5, 2.5), ylim=c(0,0.02))

#With adehabitatLT
library(adehabitatLT)

##with bursts defined by individual
traj<-as.ltraj(xy = data[,c("POINT_X","POINT_Y")], date = data$t, id = data$Individual)
plot(traj, id=c("Darnell", "Emma", "FFM", "Harriet", "Jody", "Lech", "Molly", "Tilly"))

#with bursts defined by day
traj<-as.ltraj(xy = data[,c("POINT_X","POINT_Y")], date = data$t, id = data$Individual, 
               burst=factor(data$Burst))
plot(traj, id=c("Darnell", "Emma", "FFM", "Harriet", "Jody", "Lech", "Molly", "Tilly"),
                final=F)
#transform to data type I, time not recorded or necessary
traj1<-typeII2typeI(traj)

#convert list (traj1) to data.frame
traj1.df<-summary(traj)
df<-data.frame()
for (i in 1:nrow(summary(traj1))){
  a<-cbind(traj1[[i]], rep(traj1.df$id[i], times=nrow(traj1[[i]])))
  df<-rbind(df, a)
}

df<-cbind(df, factor(data$Sex))
colnames(df[,11:12])<-c("Name","Sex")
##plot basic metrics
boxplot(df$dist ~ df[,12], names=c("Females", "Males"), boxwex=0.4, 
        ylab="Step length (m)", xlim = c(0.5, 2.5), ylim=c(0,400))
factor(df[,11])
par(mfrow=c(1,2))
boxplot(df$dist ~ df[,11], ylab="Distance (m)",ylim=c(0,400))

#histogram angles
hist(df$rel.angle[which(df[,12]=="F")], breaks="Sturges", xlab="Relative Angle", main="Females")
hist(df$rel.angle[which(df[,12]=="M")], breaks="Sturges", xlab="Relative Angle", main="Males")

testang.ltraj(traj1, "relative")
acfang.ltraj(traj1, lag=5)

#rediscretize trajectory to standard step lenght
traj1r<-redisltraj(traj1, 100)
plot(traj1r, id=c("Darnell", "Emma", "FFM", "Harriet", "Jody", "Lech", "Molly", "Tilly"))  
traj1r<-which.ltraj(traj1r, "nrow>4")
sliwinltr(traj1r[which(Nb.reloc>=5)], function(x) mean(cos(x$rel.angle)), type="locs", step=2)  
  
  
  