############
###############
####################
##################### --------- FIGURES 
rm(list=ls())

library(ggplot2)
library(data.table)
library(dplyr)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(lme4)
library(nlme)
library(ggplot2)

##########
####
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
data1=read.csv("CAFI_BAI.csv")
names(data1)

data2=data1%>% 
  rowwise()%>%
  mutate(log.bai=log1p(bai))  ###log1p keeps 0 RRW values at 0 
hist(data2$log.bai)
names(data2)
head(data2)
tail(data2)
##########
data2=as.data.frame(data2)
data2$site=as.factor(data2$site)
data2$plot=as.factor(data2$plot)
head(data2)

########### REMOVE TREES THAT DIED AFTER 1997
datG=subset(data2, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
                  tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
                  tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
                  tree!="TM6.61"& tree!="TM8.94")

#####
head(datG)
datG[] <- lapply(datG, function(x) if(is.factor(x)) factor(x) else x)
str(datG)

############### -------- FIGURE 1
#datG2=subset(data3,year>1949)
datG2=na.omit(datG)
head(datG2)
str(datG2)
names(datG2)[names(datG2) == "ï..year"] <- "year"


dat.site <-  
  ddply(datG2, c("year","status","site"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))

head(dat.site)

dat.freq=read.csv("CAFI_freq_death.csv")
str(dat.freq)
names(dat.freq)[names(dat.freq) == "ï..year"] <- "year"

dat.freq=as.data.table(dat.freq)
invisible(dat.freq[site == "199-201", site := "1067"])
invisible(dat.freq[site == "233", site := "1079"])
invisible(dat.freq[site == "298-300", site := "1101"])
invisible(dat.freq[site == "307-309", site := "1104"])
invisible(dat.freq[site == "91-93", site := "1031"])
invisible(dat.freq[site == "94-96", site := "1032"])
invisible(dat.freq[site == "97-99", site := "1033"])
invisible(dat.freq[site == "61-63", site := "1021"])


dat.site2=merge(dat.site,dat.freq,by=c("year","site"))
head(dat.site2)


###########
################## ------ mean and SE across all sites 
#########
####
dat.siteD <-  
  ddply(datG2, c("year","status"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))
head(dat.siteD)

##### calcualte N based on lvi and dead trees 
dat.siteE <-  
  ddply(datG2, c("year"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))
head(dat.siteE)
###############
baiA_all=subset(dat.siteD,status=="living")
baiA_all[] <- lapply(baiA_all, function(x) if(is.factor(x)) factor(x) else x)

baiM_all=subset(dat.siteD,status=="dying")
baiM_all[] <- lapply(baiM_all, function(x) if(is.factor(x)) factor(x) else x)

baiA1=subset(baiA_all,year<1997 & year>=1957)
baiA1b=subset(baiA_all,year<=1957 & year>=1946)
baiA2=subset(baiA_all,year>=1996)
baiA3=subset(baiA_all,year<=1946)

#####
##########
####################------green 
##### FIG 1A

par(family="Helvetica")
par(mar = c(5,5.9,2.5,6))
plot(baiA1$meanB~baiA1$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#006600")
par(new=T)
plot(baiA2$meanB~baiA2$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#66CC33")###"#CCCC33"
par(new=T)
plot(baiA1b$meanB~baiA1b$year, type="l", ylab="",lwd=2,col="#003300",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiA3$meanB~baiA3$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)

rect(1997,-100,2013,420, col= rgb(red=0.5,blue=0,green=1,alpha=0.1),border=NA)
rect(1958,-100,1996,420, col= rgb(red=0,blue=0,green=1,alpha=0.2),border=NA)
rect(1946,-100,1957,420, col= rgb(red=0.2,blue=0.2,green=0.5,alpha=0.2),border=NA)


### arrows
arrows(baiA_all$year, 
       baiA_all$meanB-baiA_all$seB, 
       baiA_all$year, 
       baiA_all$meanB+baiA_all$seB, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
par(new=T)

plot(baiA1$meanB~baiA1$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#006600")
par(new=T)
plot(baiA2$meanB~baiA2$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#66CC33")
par(new=T)
plot(baiA1b$meanB~baiA1b$year, type="l", ylab="",lwd=2,col="#003300",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiA3$meanB~baiA3$year, type="l", ylab="",lwd=2,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)

#####
#######_-----DEAD TREES 
baiM1=subset(baiM_all,year<1997 & year>=1957)
baiM1b=subset(baiM_all,year<=1957 & year>=1946)
baiM2=subset(baiM_all,year>=1996)
baiM3=subset(baiM_all,year<=1946)

par(new=T)

plot(baiM1$meanB~baiM1$year, type="l", ylab="",lwd=2,lty=5,col="#006600",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiM2$meanB~baiM2$year, type="l", ylab="",lwd=2,lty=5,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#66CC33")
par(new=T)
plot(baiM1b$meanB~baiM1b$year, type="l", ylab="",lwd=2,lty=5,col="#003300",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiM3$meanB~baiM3$year, type="l", ylab="",lwd=2,lty=5,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)


axis(side = 2, at=seq(0,370,60),lwd = 1,cex.axis=0.85,las=2)
ylab.text = expression('Basal area increment (mm'^"2"*')')
mtext(ylab.text,side=2, line =2.5,cex=0.85)
### arrows
arrows(baiM_all$year, 
       baiM_all$meanB-baiM_all$seB, 
       baiM_all$year, 
       baiM_all$meanB+baiM_all$seB, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
par(new=T)

plot(baiM1$meanB~baiM1$year, type="l", ylab="",lwd=2,lty=5,col="#006600",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiM2$meanB~baiM2$year, type="l", ylab="",lwd=2,lty=5,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F,col="#66CC33")
par(new=T)
plot(baiM1b$meanB~baiM1b$year, type="l", ylab="",lwd=2,lty=5,col="#003300",
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)
par(new=T)
plot(baiM3$meanB~baiM3$year, type="l", ylab="",lwd=2,lty=5,
     xlab="",ylim=c(0,390),xlim=c(1900,2015),axes=F)


max(dat.siteE$N)
par(new=T)
plot(dat.siteE$N~dat.siteE$year, type="l",lty=3, ylab="",lwd=2,
     xlab="",axes=F,xlim=c(1900,2015),ylim=c(0,162))
axis(side = 4, at = seq(0,162,40), lwd = 1,cex.axis=0.85,las=2)
axis(side = 1, at=seq(1900,2015,10),lwd = 1,cex.axis=0.85)
mtext(side=1,"Year",line=2,cex=0.85)
mtext(side=4,"Sample Depth",line=2.2,cex=0.85)
box()

######
######## dimensions 8x4
###

###############------- FIGURE 1B
######################## just gs cmi  OR GS TEMP before outbreak 

########### CLIMATE DATA 

data.clim=read.csv("CAFI_climate_archivalB.csv",header = T)
names(data.clim)
head(data.clim)
str(data.clim) 
dat.clim2=data.clim[,c(1,2,4,10)]
names(dat.clim2)
###
dat.climAVG <-  
  ddply(dat.clim2, c("year"), summarise,
        NC    = length(GS_CMI),
        meanC = mean(GS_CMI),
        sdC   = sd(GS_CMI),
        seC = sdC / sqrt(NC),
        NT    = length(GS_temp),
        meanT = mean(GS_temp),
        sdT   = sd(GS_temp),
        seT = sdT / sqrt(NT))
head(dat.climAVG)
###### compute avg ndvi and gs cmi
dat.climAVG_b4=subset(dat.climAVG,year<1997 & year>1945)
datT=subset(dat.climAVG_b4,meanT==12.4475)
datT2=subset(dat.climAVG_b4,meanT==9.83)

dat.climAVG2=subset(dat.climAVG,year>=1997 & year<=2013)
max(dat.climAVG2$meanT) ###12.36
min(dat.climAVG2$meanT)###9.88

datTb=subset(dat.climAVG2,meanT==12.3625)
datT2b=subset(dat.climAVG2,meanT==9.88)
######
### plot CLIMATE
max(dat.climAVG_b4$meanC) ###-4.77
min(dat.climAVG_b4$meanC)###-23.99
max(dat.climAVG_b4$meanT) ###12.4
min(dat.climAVG_b4$meanT)###9.8

par(mar = c(5,5.9,2.5,6))
datC1=subset(dat.climAVG_b4,year<=1957)
datC2=subset(dat.climAVG_b4,year>=1957)

plot(datC1$meanC~datC1$year,type="l",ylab="",lwd=2,col="#003300",
     xlab="",xlim=c(1945,1996),ylim=c(-25,0),axes=F)
par(new=T)
plot(datC2$meanC~datC2$year,type="l",ylab="",lwd=2,col="#006600",
     xlab="",xlim=c(1945,1996),ylim=c(-25,0),axes=F)
par(new=T)
arrows(dat.climAVG_b4$year, 
       dat.climAVG_b4$meanC-dat.climAVG_b4$seC, 
       dat.climAVG_b4$year, 
       dat.climAVG_b4$meanC+dat.climAVG_b4$seC, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
rect(1958,-100,1996,420, col= rgb(red=0,blue=0,green=1,alpha=0.2),border=NA)
rect(1946,-100,1957,420, col= rgb(red=0.2,blue=0.2,green=0.5,alpha=0.2),border=NA)

par(new=T)
plot(datC1$meanC~datC1$year,type="l",ylab="",lwd=2,col="#003300",
     xlab="",xlim=c(1945,1996),ylim=c(-25,0),axes=F)
par(new=T)
plot(datC2$meanC~datC2$year,type="l",ylab="",lwd=2,col="#006600",
     xlab="",xlim=c(1945,1996),ylim=c(-25,0),axes=F)

axis(side = 2, at=seq(-25,0,5),lwd = 1,cex.axis=0.85,las=2)
######
axis(side = 1, at=seq(1945,1996,5),lwd = 1,cex.axis=0.85)
box()
ylab.text = expression('GS CMI (cm water'^"1"*'season'^-"1"*')')
mtext(ylab.text,side=2, line =2.5,cex=0.85)
mtext(side=1,"Year",line=2,cex=0.85)

##########
############### FIGURE W/NDVI, GS CMI, AND ALM 
##########
#####
######### insect and time period data
dataI=read.csv("leaf_mining.csv")
names(dataI)
dataI2=dataI[,c(1,2)]
names(dataI2)
str(dataI2)
###########
###########

########### CLIMATE DATA 
data.clim=read.csv("CAFI_climate_archivalB.csv",header = T)
names(data.clim)
head(data.clim)
str(data.clim) 
dat.clim2=data.clim[,c(1,2,4,10)]
names(dat.clim2)
###

dat.climAVG <-  
  ddply(dat.clim2, c("year"), summarise,
        N    = length(GS_CMI),
        meanC = mean(GS_CMI),
        sdC   = sd(GS_CMI),
        seC = sdC / sqrt(N))
head(dat.climAVG)
########
###########  ndvi 
dat.ndvi=read.csv("NDVI_7yr.csv",header=T) 

names(dat.ndvi)
head(dat.ndvi)
dat.ndvi2=na.omit(dat.ndvi)
####
dat.ndviAVG <-  
  ddply(dat.ndvi2, c("year"), summarise,
        N    = length(ndvi.max),
        meanN = mean(ndvi.max),
        sdN   = sd(ndvi.max),
        seN = sdN / sqrt(N))
head(dat.ndviAVG) ##5x7

###### compute avg ndvi and gs cmi
str(dataI2)
names(dataI2)[names(dataI2) == "ï..year"] <- "year"

dataI_2=subset(dataI2,year>1996 & year<2014)
dat.climAVG_2=subset(dat.climAVG,year>1996 & year<2014)
dat.ndviAVG_2=subset(dat.ndviAVG,year>1996 & year<2014)
######

### plot alm
max(dataI_2$leafminer.ha.)
min(dataI_2$leafminer.ha.)

par(mar = c(5,5.9,2.5,6))
### plot CLIMATE
max(dat.climAVG_2$meanC)
min(dat.climAVG_2$meanC)
par(mar = c(5,5.9,2.5,6))
plot(dat.climAVG_2$meanC~dat.climAVG_2$year,type="l",ylab="",lwd=2.5,
     xlab="",xlim=c(1997,2013),ylim=c(-50,20),axes=F,col="black")

arrows(dat.climAVG_2$year, 
       dat.climAVG_2$meanC-dat.climAVG_2$seC, 
       dat.climAVG_2$year, 
       dat.climAVG_2$meanC+dat.climAVG_2$seC, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
par(new=T)
plot(dat.climAVG_2$meanC~dat.climAVG_2$year,type="l",ylab="",lwd=2.5,
     xlab="",xlim=c(1997,2013),ylim=c(-50,20),axes=F,col="black")
axis(side = 2, at=seq(-28,0,6),lwd = 1,cex.axis=0.85,las=2)

#### leaf miner
par(new=T)
plot(dataI_2$leafminer.ha.~dataI_2$year,type="o",ylab="",lwd=2,lty=1,
     xlab="",xlim=c(1997,2013),axes=F,ylim=c(0,1000000))
aty <- seq(0,310000,100000)
axis(side = 4, at=aty,lwd = 1,cex.axis=0.85,las=2,labels=format(aty, scientific=FALSE))


### plot ndvi
par(mar = c(5,5.9,2.5,6))
par(new=T)
min(dat.ndviAVG_2$meanN)
max(dat.ndviAVG_2$meanN)


plot(dat.ndviAVG_2$meanN~dat.ndviAVG_2$year,type="l",ylab="",lwd=2,lty=3,
     xlab="",xlim=c(1997,2013),axes=F,ylim=c(0.52,0.8))

arrows(dat.ndviAVG_2$year, 
       dat.ndviAVG_2$meanN-dat.ndviAVG_2$seN, 
       dat.ndviAVG_2$year, 
       dat.ndviAVG_2$meanN+dat.ndviAVG_2$seN, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
axis(side = 4, at=seq(0.7,0.8,0.04),lwd = 1,cex.axis=0.85,las=2)
axis(side = 1, at=seq(1997,2013,1),lwd = 1,cex.axis=0.85)
ylab.text = expression('GS CMI (cm water'^"1"*'season'^-"1"*')')
mtext(ylab.text,side=2, line =2.2,cex=0.85)
mtext(side=1,"Year",line=2,cex=0.85)
mtext(side=4,"Leaf mining (ha)",line=3.5,cex=0.85,adj=0.1)
mtext(side=4,"NDVI",line=3.3,cex=0.85,adj=0.8)
##########
box() ### dims = 600x500



