################# 
####### -----------ANALYSIS: climate-growth response before ALM
#####
rm(list = ls())

library(dplyr)
library(ggplot2)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(nlme)
library(tidyr)
library(data.table)

##################
####
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")

data1=read.csv("CAFI_BAI.csv")
names(data1)

data2=data1%>% 
  rowwise()%>%
  mutate(log.bai=log1p(bai))  
hist(data2$log.bai)
names(data2)
head(data2)
tail(data2)
##########
data2=as.data.frame(data2)
data2$site=as.factor(data2$site)
data2$plot=as.factor(data2$plot)
#names(data2)[names(data2) == "ï..year"] <- "year"

########### CLIMATE DATA 
data.clim=read.csv("CAFI_climate_archivalB.csv",header = T)
names(data.clim)
head(data.clim)
str(data.clim) 
data.clim$site=as.factor(data.clim$site)
##########
data.clim2b=subset(data.clim,year>=1946 & year<=1996)
head(data.clim2b)

#
#####
data.clim3=data.clim2b%>% 
  group_by(site) %>% 
  mutate(scaleGS_CMI=scale(GS_CMI,center=TRUE,scale=TRUE),
         scaleGSP_CMI=scale(PGS_CMI,center = TRUE,scale=TRUE),
         scaleSP_CMI=scale(Spring_CMI,center=TRUE,scale=TRUE),
         scaleGS_temp=scale(GS_temp,center=TRUE,scale=TRUE),
         scaleGSP_temp=scale(PGS_temp,center = TRUE,scale=TRUE),
         scaleSP_temp=scale(Spring_temp,center=TRUE,scale=TRUE),
         scaleGSP_precip=scale(PGS_precip,center = TRUE,scale=TRUE),
         scaleGS_precip=scale(GS_precip,center = TRUE,scale=TRUE),
         scaleSP_precip=scale(Spring_precip,center = TRUE,scale=TRUE),
         scaleFW_temp=scale(FW_temp,center = TRUE,scale=TRUE),
         scaleFW_precip=scale(FW_precip,center = TRUE,scale=TRUE),
         scaleFW_CMI=scale(FW_CMI,center = TRUE,scale=TRUE))
######################
########## MEAN TEMP BY EYAR
dat.meanT<-  
  ddply(data.clim3, c("year"), summarise,
        N    = length(GS_temp),
        meanB = mean(GS_temp),
        sdB   = sd(GS_temp),
        seB = sdB / sqrt(N))
#####mean gs cmi by year
##########
dat.meanC<-  
  ddply(data.clim3, c("year"), summarise,
        N    = length(GS_CMI),
        meanB = mean(GS_CMI),
        sdB   = sd(GS_CMI),
        seB = sdB / sqrt(N))
### then run models 

datALLB=merge(data2,data.clim3,by=c('year','site'),all=TRUE)
names(datALLB)
head(datALLB)
str(datALLB)

########### REMOVE TREES THAT DIED AFTER 1997
datALL2B=subset(datALLB, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
                  tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
                  tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
                  tree!="TM6.61"& tree!="TM8.94")

#####
head(datALL2B)
names(datALL2B)
datALL2B[] <- lapply(datALL2B, function(x) if(is.factor(x)) factor(x) else x)
str(datALL2B)

############
#### ------ 
dat.b4M=subset(datALL2B,status=="dying")
dat.b4M[] <- lapply(dat.b4M, function(x) if(is.factor(x)) factor(x) else x)
######
dat.b4M$plot=as.factor(dat.b4M$plot)
dat.b4M$site=as.factor(dat.b4M$site)
str(dat.b4M)
################ FROM 1946 - 1957
dat.b4M0=subset(dat.b4M,year>=1946 & year<=1957)
names(dat.b4M0)
str(dat.b4M0)

vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_CMI+scaleFW_temp,data=dat.b4M0))  # not collinear

##### TABLE S4 ---- precip and cmi collinear
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=dat.b4M0)) ###

##### TABLE s13 and fig 3 ----DEAD TREE RESPONE TO CLIMATE 
mod0M=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_CMI+scaleFW_temp,random=~1|site/plot/tree,data=dat.b4M0,
          na.action=na.exclude,method="ML")
summary(mod0M)

mod0M1=update(mod0M, .~. -scaleFW_temp)
anova(mod0M1,mod0M)
model.sel(mod0M1,mod0M,rank=AICc) # not sig 

mod0M2=update(mod0M, .~. -scaleFW_CMI)
anova(mod0M2,mod0M)
model.sel(mod0M2,mod0M,rank=AICc) # not sig 

mod0M3=update(mod0M, .~. -scaleGSP_CMI)
anova(mod0M3,mod0M)
model.sel(mod0M3,mod0M,rank=AICc) # sig

mod0M4=update(mod0M, .~. -scaleSP_CMI)
anova(mod0M4,mod0M)
model.sel(mod0M4,mod0M,rank=AICc) # not sig

mod0M5=update(mod0M, .~. -scaleGS_CMI)
anova(mod0M5,mod0M)
model.sel(mod0M5,mod0M,rank=AICc) # sig

mod0M6=update(mod0M, .~. -scaleGSP_temp)
anova(mod0M6,mod0M)
model.sel(mod0M6,mod0M,rank=AICc) # sig

mod0M7=update(mod0M, .~. -scaleSP_temp)
anova(mod0M7,mod0M)
model.sel(mod0M7,mod0M,rank=AICc) # sig

mod0M8=update(mod0M, .~. -scaleGS_temp)
anova(mod0M8,mod0M)
model.sel(mod0M8,mod0M,rank=AICc) #  sig

##### compare null model to best model
modM_null=lme(log.bai~1,random=~1|site/plot/tree,data=dat.b4M0,
          na.action=na.exclude,method="ML")

modMB=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleGSP_CMI,random=~1|site/plot/tree,data=dat.b4M0,
          na.action=na.exclude,method="ML")

model.sel(modM_null,modMB,rank=AICc)#modMB
anova(modM_null,modMB)
##########
##### TABLE s13 and fig 3  ----DEAD TREE RESPONE TO CLIMATE # FINAL MODEL
mod0M=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/plot/tree,data=dat.b4M0,
          na.action=na.exclude,method="REML")

summary(mod0M) ##### 
plot(mod0M)
qqnorm(resid(mod0M))
anova(mod0M)

plot(x=dat.b4M0$year, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$tree, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$site, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$plot, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$scaleGS_CMI, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$scaleGS_temp, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$FW_CMI, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$FW_temp, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$scaleGSP_CMI, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$scaleGSP_temp, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$Spring_CMI, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M0$Spring_temp, y=resid(mod0M), type="pearson")
abline(h = 0, lty = 2) #
#####
####### cliamte analyses from 1971 - 1982
dat.b4M2=subset(dat.b4M,year>=1971 & year<=1982)

vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,data=dat.b4M2)) ## NOT COLLINEAR
##### TABLE S4
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=dat.b4M2))

##### TABLE s13 and fig 3 
modM2=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+scaleFW_temp+scaleFW_CMI+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI,random=~1|site/tree,data=dat.b4M2,
          na.action=na.exclude,method = "ML")
summary(modM2)

modM2b=update(modM2, .~. -scaleFW_temp)
anova(modM2b,modM2)
model.sel(modM2b,modM2,rank=AICc) # sig

modM2c=update(modM2, .~. -scaleFW_CMI)
anova(modM2c,modM2)
model.sel(modM2c,modM2,rank=AICc) # sig 

modM2d=update(modM2, .~. -scaleGSP_CMI)
anova(modM2d,modM2)
model.sel(modM2d,modM2,rank=AICc) # sig

modM2e=update(modM2, .~. -scaleSP_CMI)
anova(modM2e,modM2)
model.sel(modM2e,modM2,rank=AICc) # not sig

modM2f=update(modM2, .~. -scaleGS_CMI)
anova(modM2,modM2f)
model.sel(modM2f,modM2,rank=AICc) # sig

modM2g=update(modM2, .~. -scaleGSP_temp)
anova(modM2g,modM2)
model.sel(modM2g,modM2,rank=AICc) # not sig

modM2h=update(modM2, .~. -scaleSP_temp)
anova(modM2h,modM2)
model.sel(modM2h,modM2,rank=AICc) # sig

modM2i=update(modM2, .~. -scaleGS_temp)
anova(modM2i,modM2)
model.sel(modM2i,modM2,rank=AICc) # not sig
##########
##### compare null model to best model
modM2N=lme(log.bai~1,random=~1|site/tree,data=dat.b4M2,
           na.action=na.exclude,method = "ML")

modM2B=lme(log.bai~scaleSP_temp+scaleFW_temp+scaleFW_CMI+
            scaleGS_CMI+scaleGSP_CMI,random=~1|site/tree,data=dat.b4M2,
          na.action=na.exclude,method = "ML")

model.sel(modM2N,modM2B) #modM2B
anova(modM2N,modM2B)
######
########### final model
modM2=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/tree,data=dat.b4M2,
          na.action=na.exclude,method = "REML")
plot(log.bai~year,data=dat.b4M2) ## just one tree is weird..but it deosn't change results 
summary(modM2) ### GS previous CMI 
plot(modM2)
qqnorm(resid(modM2)) ### get rid of points, see what happens to results 
#identify(qqnorm(resid(modM2))) ###686s 
plot(x=dat.b4M2$tree, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$site, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$plot, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$scaleGS_CMI, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$scaleGS_temp, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$FW_CMI, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$FW_temp, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$scaleGSP_CMI, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$scaleGSP_temp, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$Spring_CMI, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M2$Spring_temp, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #

######
####### cliamte analyses from 1983 - 1994
dat.b4M3=subset(dat.b4M,year>=1983 & year<=1994)

vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,data=dat.b4M3)) ## NOT COLLINEAR
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=dat.b4M3))

##### TABLE s13 and fig 3 
modM3=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=dat.b4M3,
          na.action=na.exclude,method = "ML")
summary(modM3) ##

modM3b=update(modM3, .~. -scaleFW_temp)
anova(modM3b,modM3)
model.sel(modM3b,modM3,rank=AICc) # sig

modM3c=update(modM3, .~. -scaleFW_CMI)
anova(modM3c,modM3)
model.sel(modM3c,modM3,rank=AICc) # sig 

modM3d=update(modM3, .~. -scaleGSP_CMI)
anova(modM3d,modM3)
model.sel(modM3d,modM3,rank=AICc) # sig

modM3e=update(modM3, .~. -scaleSP_CMI)
anova(modM3e,modM3)
model.sel(modM3e,modM3,rank=AICc) # sig

modM3f=update(modM3, .~. -scaleGS_CMI)
anova(modM3,modM3f)
model.sel(modM3f,modM3,rank=AICc) #NOT SIG

modM3g=update(modM3, .~. -scaleGSP_temp)
anova(modM3g,modM3)
model.sel(modM3g,modM3,rank=AICc) # not sig

modM3h=update(modM3, .~. -scaleSP_temp)
anova(modM3h,modM3)
model.sel(modM3h,modM3,rank=AICc) # sig

modM3i=update(modM3, .~. -scaleGS_temp)
anova(modM3i,modM3)
model.sel(modM3i,modM3,rank=AICc) # SIG

########## COMPARE NULL TO BEST MODEL 
modM3N=lme(log.bai~1,random=~1|site/tree,data=dat.b4M3,
          na.action=na.exclude,method = "ML")

modM3B=lme(log.bai~scaleGS_temp+scaleSP_temp+
             scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=dat.b4M3,
          na.action=na.exclude,method = "ML")

model.sel(modM3N,modM3B,rank=AICc)#modM3B
anova(modM3N,modM3B)
######final model
modM3=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/tree,data=dat.b4M3,
          na.action=na.exclude,method = "REML")
summary(modM3) ##
plot(modM3) #### outlier 
qqnorm(resid(modM3)) ### 384 ### keep outlier!!! its a 0 value and desnt change results
#####
#### remove outlier
datM3b=dat.b4M3[-(384),]
###
modM3b=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
             scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=datM3b,
           na.action=na.exclude,method = "REML")
summary(modM3b) ### GS TEMP ### results are the SAME

######
plot(x=dat.b4M3$year, y=resid(modM3), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$tree, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$site, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleGS_temp, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleGS_CMI, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleFW_CMI, y=resid(modM3), type="pearson") ####
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleFW_temp, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleGSP_CMI, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleGSP_temp, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleSP_CMI, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4M3$scaleSP_temp, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #


##########-------------LIVING TREES 
############
dat.b4A=subset(datALL2B,status=="living")

dat.b4A[] <- lapply(dat.b4A, function(x) if(is.factor(x)) factor(x) else x)
dat.b4A$plot=as.factor(dat.b4A$plot)
dat.b4A$site=as.factor(dat.b4A$site)
################ FROM 1946 - 1957
dat.b4A0=subset(dat.b4A,year>=1946 & year<=1957)##pre - divergence
str(dat.b4A0)
head(dat.b4A0)
names(dat.b4A0)


vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+ ##### vif<5
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,data=dat.b4A0)) ##A1CMI AND GSP CMI COLLIEANR

##### TABLE S4
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=dat.b4A0))

#### TABLE s12 and fig 3 
mod0A=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,random=~1|site/plot/tree,data=dat.b4A0,
          na.action=na.exclude,method="ML")
summary(mod0A) ##### 

mod0Ab=update(mod0A, .~. -scaleFW_temp)
anova(mod0Ab,mod0A)
model.sel(mod0Ab,mod0A,rank=AICc) #not sig

mod0Ac=update(mod0A, .~. -scaleFW_CMI)
anova(mod0Ac,mod0A)
model.sel(mod0Ac,mod0A,rank=AICc) # not sig

mod0Ad=update(mod0A, .~. -scaleGSP_CMI)
anova(mod0Ad,mod0A)
model.sel(mod0Ad,mod0A,rank=AICc) # sig

mod0Ae=update(mod0A, .~. -scaleSP_CMI)
anova(mod0Ae,mod0A)
model.sel(mod0Ae,mod0A,rank=AICc) # sig

mod0Af=update(mod0A, .~. -scaleGS_CMI)
anova(mod0Af,mod0A)
model.sel(mod0Af,mod0A,rank=AICc) # sig

mod0Ag=update(mod0A, .~. -scaleGSP_temp)
anova(mod0Ag,mod0A)
model.sel(mod0Ag,mod0A,rank=AICc) # sig

mod0Ah=update(mod0A, .~. -scaleSP_temp)
anova(mod0Ah,mod0A)
model.sel(mod0Ah,mod0A,rank=AICc) # sig

mod0Ai=update(mod0A, .~. -scaleGS_temp)
anova(mod0Ai,mod0A)
model.sel(mod0Ai,mod0A,rank=AICc) # sig

####### compare null to best model
mod0AN=lme(log.bai~1,random=~1|site/plot/tree,data=dat.b4A0,
          na.action=na.exclude,method="ML")
mod0AB=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI,random=~1|site/plot/tree,data=dat.b4A0,
          na.action=na.exclude,method="ML")#mod0AB
model.sel(mod0AN,mod0AB,rank = AICc)
anova(mod0AN,mod0AB)
######## FINAL MODEL
mod0A=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/plot/tree,data=dat.b4A0,
          na.action=na.exclude,method="REML")
summary(mod0A) ##### 
plot(mod0A)
qqnorm(resid(mod0A))

plot(x=dat.b4A0$tree, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$site, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2)
plot(x=dat.b4A0$plot, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$scaleGS_CMI, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$scaleGS_temp, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$FW_CMI, y=resid(mod0A), type="pearson") #### THIS IS WHAT LOOKS BAD....
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$FW_temp, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$scaleGSP_CMI, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$scaleGSP_temp, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$Spring_CMI, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat.b4A0$Spring_temp, y=resid(mod0A), type="pearson")
abline(h = 0, lty = 2) #
####
############
####### cliamte analyses from 1971 - 1982
datA5=subset(dat.b4A,year>=1971 & year<=1982)

vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,data=datA5)) ## NOT COLLINEAR

#### table s4
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=datA5))
#### TABLE s12 and fig 3 
mod5A=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=datA5,
          na.action=na.exclude,method = "ML")
summary(mod5A) ### GS previous CMI 

mod5Ab=update(mod5A, .~. -scaleFW_temp)
anova(mod5Ab,mod5A)
model.sel(mod5Ab,mod5A,rank=AICc) # sig

mod5Ac=update(mod5A, .~. -scaleFW_CMI)
anova(mod5Ac,mod5A)
model.sel(mod5Ac,mod5A,rank=AICc) #not sig

mod5Ad=update(mod5A, .~. -scaleGSP_CMI)
anova(mod5Ad,mod5A)
model.sel(mod5Ad,mod5A,rank=AICc) # sig

mod5Ae=update(mod5A, .~. -scaleSP_CMI)
anova(mod5Ae,mod5A)
model.sel(mod5Ae,mod5A,rank=AICc) ## sig

mod5Af=update(mod5A, .~. -scaleGS_CMI)
anova(mod5Af,mod5A)
model.sel(mod5Af,mod5A,rank=AICc) # sig

mod5Ag=update(mod5A, .~. -scaleGSP_temp)
anova(mod5Ag,mod5A)
model.sel(mod5Ag,mod5A,rank=AICc) # sig

mod5Ah=update(mod5A, .~. -scaleSP_temp)
anova(mod5Ah,mod5A)
model.sel(mod5Ah,mod5A,rank=AICc) ## sig

mod5Ai=update(mod5A, .~. -scaleGS_temp)
anova(mod5Ai,mod5A)
model.sel(mod5Ai,mod5A,rank=AICc) #not sig

##########COMPARE NULL TO BEST MODEL
mod5AN=lme(log.bai~1,random=~1|site/tree,data=datA5,
          na.action=na.exclude,method = "ML")
mod5AB=lme(log.bai~scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+scaleFW_temp,random=~1|site/tree,data=datA5,
          na.action=na.exclude,method = "ML")
model.sel(mod5AN,mod5AB,rank=AICc)#mod5AB
anova(mod5AN,mod5AB)
#### FINAL MODEL
mod5A=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/tree,data=datA5,
          na.action=na.exclude,method = "REML")
summary(mod5A) ### GS previous CMI 
plot(mod5A) ###
qqnorm(resid(mod5A))


plot(x=datA5$tree, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$site, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$plot, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$scaleGS_CMI, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$scaleGS_temp, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$FW_CMI, y=resid(mod5A), type="pearson") 
abline(h = 0, lty = 2) #
plot(x=datA5$FW_temp, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$scaleGSP_CMI, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$scaleGSP_temp, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$Spring_CMI, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA5$Spring_temp, y=resid(mod5A), type="pearson")
abline(h = 0, lty = 2) #
#####
########## --------living 
####### cliamte analyses from 1983 - 1994
####
datA6=subset(dat.b4A,year>=1983 & year<=1994)

vif(lm(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
         scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+
         scaleFW_temp+scaleFW_CMI,data=datA6)) ## NOT COLLINEAR

##### TABLE S4
vif(lm(log.bai~scaleFW_CMI+scaleFW_precip+scaleGS_CMI+scaleGS_precip+
         scaleSP_CMI+scaleSP_precip+scaleFW_CMI+scaleFW_precip+scaleGSP_CMI+
         scaleGSP_precip,data=datA6))

########### TABLE s12 and fig 3 
modA6=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
            scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+
            scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=datA6,
          na.action=na.exclude,method = "ML")
summary(modA6)

modA6b=update(modA6, .~. -scaleFW_temp)
anova(modA6b,modA6)
model.sel(modA6b,modA6,rank=AICc)  # NOT sig

modA6c=update(modA6, .~. -scaleFW_CMI)
anova(modA6c,modA6)
model.sel(modA6,modA6,rank=AICc) # sig

modA6d=update(modA6, .~. -scaleGSP_CMI)
anova(modA6d,modA6)
model.sel(modA6d,modA6,rank=AICc) # sig

modA6e=update(modA6, .~. -scaleSP_CMI)
anova(modA6e,modA6)
model.sel(modA6e,modA6,rank=AICc) ## sig

modA6f=update(modA6, .~. -scaleGS_CMI)
anova(modA6f,modA6)
model.sel(modA6f,modA6,rank=AICc) # sig

modA6g=update(modA6, .~. -scaleGSP_temp)
anova(modA6g,modA6)
model.sel(modA6g,modA6,rank=AICc) #not  sig

modA6h=update(modA6, .~. -scaleSP_temp)
anova(modA6h,modA6)
model.sel(modA6h,modA6,rank=AICc) ## sig

modA6i=update(modA6, .~. -scaleGS_temp)
anova(modA6i,modA6)
model.sel(modA6i,modA6,rank=AICc) #sig
########### compare null to best model
modA6N=lme(log.bai~1,random=~1|site/tree,data=datA6,
          na.action=na.exclude,method = "ML")
modA6B=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_CMI+
             scaleGSP_CMI,random=~1|site/tree,data=datA6,
           na.action=na.exclude,method = "ML")
model.sel(modA6N,modA6B,rank = AICc)#modA6B
anova(modA6N,modA6B)
########## FINAL MODEL
modA6=lme(log.bai~scaleGS_temp+scaleGS_CMI+scaleSP_temp+scaleSP_CMI+scaleFW_temp+scaleFW_CMI+
            scaleGSP_temp+scaleGSP_CMI,random=~1|site/tree,data=datA6,
          na.action=na.exclude,method = "REML") ##KEEP OUTLIERI ITS A TRUE 0
### AND SAME SITE WHERE THERE IS A 0 FOR A DEAD TREE
summary(modA6) ### GS TEMP  and CMI  
plot(modA6)
qqnorm(resid(modA6)) ####426
#####
#### remove outlier
datA6b=datA6[-(426),]
modA6b=lme(log.bai~scaleGS_temp+scaleSP_temp+scaleGSP_temp+
             scaleGS_CMI+scaleSP_CMI+scaleGSP_CMI+
             scaleFW_temp+scaleFW_CMI,random=~1|site/tree,data=datA6b,
           na.action=na.exclude,method = "REML")
summary(modA6b) ### GS TEMP  and CMI 
plot(modA6b)
qqnorm(resid(modA6b)) 
###
plot(x=datA6$year, y=resid(modA6), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA6$tree, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$site, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$plot, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleGS_CMI, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleGS_temp, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleFW_CMI, y=resid(modA6), type="pearson") #### THIS IS WHAT LOOKS BAD....
abline(h = 0, lty = 2) #
plot(x=datA6$scaleFW_temp, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleGSP_CMI, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleGSP_temp, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleSP_CMI, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA6$scaleSP_temp, y=resid(modA6), type="pearson")
abline(h = 0, lty = 2) #
#####
