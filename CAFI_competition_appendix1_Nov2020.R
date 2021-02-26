####  --------- analyses: competition  ##### APPENDIX 2 
###
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

#################
#############
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
########### CLIMATE DATA 
data.clim=read.csv("CAFI_climate_archivalB.csv",header = T)
names(data.clim)
head(data.clim)
######
names(data.clim)
str(data.clim)
##########
#names(data2)[names(data2) == "ï..site"] <- "site"
#names(data2)[names(data2) == "ï..year"] <- "year"

names(data2)

datALLB=merge(data2,data.clim,by=c('year','site'),all=TRUE)
names(datALLB)
head(datALLB)
str(datALLB)

########### REMOVE TREES THAT DIED BEFORE 1997
datALL2B=subset(datALLB, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
                  tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
                  tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
                  tree!="TM6.61"& tree!="TM8.94")

#####
head(datALL2B)
names(datALL2B)
datALL2B[] <- lapply(datALL2B, function(x) if(is.factor(x)) factor(x) else x)
str(datALL2B)


######### insect and time period data
dataI=read.csv("leaf_mining.csv")
names(dataI)
str(dataI)
#names(dataI)[names(dataI) == "ï..year"] <- "year"

dataI2=subset(dataI,year>=1997 & year<=2013)
dataI3=dataI2%>% 
  mutate(scaleALM=scale(leafminer.ha.,center=TRUE,scale=TRUE),
         scaleALMP=scale(alm.prev,center = TRUE,scale=TRUE))

###########
data3=merge(datALL2B,dataI3,by=c("year"),all=TRUE)
names(data3)
str(data3)
data3_alm=subset(data3,year>=1997 & year<=2013)
head(data3_alm)
names(data3_alm)
str(data3_alm)
###### scale variables for model 
data4=data3_alm%>% 
  group_by(site) %>% 
  mutate( scaleGSP_CMI=scale(PGS_CMI,center = TRUE,scale=TRUE),
          scaleGS_temp=scale(GS_temp,center=TRUE,scale=TRUE))
head(data4)
names(data4)
######
data4=as.data.frame(data4)
data4$tree=as.factor(data4$tree)
data4$status=as.factor(data4$status)

data4$status = relevel(data4$status, ref="living")
##############

############
dat_alive=subset(data4,status=="living")
datA_lm=subset(dat_alive,year>=1997 & year<=2013)
datA_lm[] <- lapply(datA_lm, function(x) if(is.factor(x)) factor(x) else x)
str(datA_lm)
datA_lm=as.data.table(datA_lm)
names(datA_lm)
########
datA_lm3=datA_lm[,c(1:7,22:25)]
names(datA_lm3)
##################
######## ############# stem density + biomass
aspDB=read.csv("biomass+stem+IV.csv")
names(aspDB)
str(aspDB)
aspDB2=subset(aspDB,year>=1997 & year<=2013)
#names(aspDB2)[names(aspDB2) == "ï..plot"] <- "plot"
names(aspDB2)

aspDB2$plot=as.factor(aspDB2$plot)
names(aspDB2)
aspD3=aspDB2[,c(1:3,9,12,18,23,24)]
names(aspD3)
aspD3$site=as.factor(aspD3$site)
aspD3$plot=as.factor(aspD3$plot)
str(aspD3)

aspD4=aspD3%>% 
  group_by(plot,site) %>% 
  mutate(scaleDA=scale(aspen_stems.ha,center=TRUE,scale=TRUE),
         scaleDABA=scale(allbutaspen_stems.ha,center=TRUE,scale=TRUE),
         scaleBA=scale(aspen_biomass,center=TRUE,scale=TRUE),
         scaleBABA=scale(allbutaspen_biomass,center=TRUE,scale=TRUE))

str(aspD4)
aspD4$site=as.factor(aspD4$site)
datAD=merge(datA_lm3,aspD4, by=c("plot","year","site"),all = TRUE)
########## 
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleDA,data=datAD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleDABA,data=datAD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleBABA,data=datAD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleBA,data=datAD)) ## not collinear

########## ASPEN DENSITY
names(datAD)
str(datAD)
datDA2=na.omit(datAD)

modDA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleDA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="ML")
summary(modDA)
modDA1=update(modDA,~.-scaleGSP_CMI)
model.sel(modDA1,modDA,rank = AICc) # remove
anova(modDA1,modDA)
modDA2=update(modDA,~.-scaleGS_temp)
model.sel(modDA2,modDA,rank = AICc) # remove
anova(modDA2,modDA)
modDA3=update(modDA,~.-scaleALMP)
model.sel(modDA3,modDA,rank = AICc) # keep
anova(modDA3,modDA)
modDA4=update(modDA,~.-scaleDA)
model.sel(modDA4,modDA,rank = AICc)# keep
anova(modDA4,modDA)

########## COMPARE NULL TO BEST MODEL
modDA_new=lme(log.bai~scaleALMP+ scaleDA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="ML")
modDA0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="ML")
model.sel(modDA_new,modDA0,rank = AICc) ##da new
anova(modDA_new,modDA0) 
#####
######################## full model results 
modDA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleDA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="REML")
summary(modDA) 
plot(modDA)
qqnorm(modDA)

plot(x=datDA2$tree, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$site, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$plot, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2)  
plot(x=datDA2$scaleGSP_CMI, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleGS_temp, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datDA2$scaleALMP, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$scaleDA, y=resid(modDA), type="pearson")
abline(h = 0, lty = 2)


######### OTHER DENSTIY 
modDABA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleDABA,random=~1|site/plot/tree,data=datDA2,
            na.action=na.exclude, method="ML")
modDABA1=update(modDABA,~.-scaleGSP_CMI)
model.sel(modDABA1,modDABA,rank = AICc) # remove
anova(modDABA1,modDABA)
modDABA2=update(modDABA,~.-scaleGS_temp)
model.sel(modDABA2,modDABA,rank = AICc) # remove
anova(modDABA2,modDABA)
modDABA3=update(modDABA,~.-scaleALMP)
model.sel(modDABA3,modDABA,rank = AICc) # keep
anova(modDABA3,modDABA)
modDABA4=update(modDABA,~.-scaleDABA)
model.sel(modDABA4,modDABA,rank = AICc)# remove
anova(modDABA4,modDABA)

######## compare null to best model
modDABA_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datDA2,
              na.action=na.exclude, method="ML")
modDABA0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA2,
           na.action=na.exclude, method="ML")
model.sel(modDABA_new,modDABA0,rank = AICc) ##da new
anova(modDABA_new,modDABA0)

############ full model results 
modDABA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleDABA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="REML")
summary(modDABA) 
plot(modDABA)
qqnorm(modDABA)

plot(x=datDA2$tree, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$site, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$plot, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2)  
plot(x=datDA2$scaleGSP_CMI, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleGS_temp, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datDA2$scaleALMP, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$scaleDABA, y=resid(modDABA), type="pearson")
abline(h = 0, lty = 2)

######### ASPEN BIOMASS 
#################################

modBA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleBA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="ML")

modBA1=update(modBA,~.-scaleGSP_CMI)
model.sel(modBA1,modBA,rank = AICc) # remove
anova(modBA1,modBA)
modBA2=update(modBA,~.-scaleGS_temp)
model.sel(modBA2,modBA,rank = AICc) # remove
anova(modBA2,modBA)
modBA3=update(modBA,~.-scaleALMP)
model.sel(modBA3,modBA,rank = AICc) # keep
anova(modBA3,modBA)
modBA4=update(modBA,~.-scaleBA)
model.sel(modBA4,modBA,rank = AICc)# KEEP
anova(modBA4,modBA)

######## compare null to best model

modBA_new=lme(log.bai~scaleALMP+scaleBA,random=~1|site/plot/tree,data=datDA2,
                na.action=na.exclude, method="ML")
modBA0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA2,
             na.action=na.exclude, method="ML")
model.sel(modBA_new,modBA0,rank = AICc) ##da new
anova(modBA_new,modBA0)
####### FULL MODEL RESULTS 
modBA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleBA,random=~1|site/plot/tree,data=datDA2,
            na.action=na.exclude, method="REML")
summary(modBA) 
plot(modBA)
qqnorm(modBA)

plot(x=datDA2$tree, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$site, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$plot, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2)  
plot(x=datDA2$scaleGSP_CMI, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleGS_temp, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datDA2$scaleALMP, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$scaleBA, y=resid(modBA), type="pearson")
abline(h = 0, lty = 2)

######### OTHER BIOMASS 
##############################
modBABA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleBABA,random=~1|site/plot/tree,data=datDA2,
            na.action=na.exclude, method="ML")
summary(modBABA) 
modBABA1=update(modBABA,~.-scaleGSP_CMI)
model.sel(modBABA1,modBABA,rank = AICc) # remove
anova(modBABA1,modBABA)
modBABA2=update(modBABA,~.-scaleGS_temp)
model.sel(modBABA2,modBABA,rank = AICc) # remove
anova(modBABA2,modBABA)
modBABA3=update(modBABA,~.-scaleALMP)
model.sel(modBABA3,modBABA,rank = AICc) # keep
anova(modBABA3,modBABA)
modBABA4=update(modBABA,~.-scaleBABA)
model.sel(modBABA4,modBABA,rank = AICc)# KEEP
anova(modBABA4,modBABA)

######## compare null to best model
modBABA_new=lme(log.bai~scaleALMP+scaleBABA,random=~1|site/plot/tree,data=datDA2,
              na.action=na.exclude, method="ML")
modBABA0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA2,
           na.action=na.exclude, method="ML")
model.sel(modBABA_new,modBABA0,rank = AICc) ##da new
anova(modBABA_new,modBABA0)

####### FULL MODEL RESULTS 
modBABA=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleBABA,random=~1|site/plot/tree,data=datDA2,
          na.action=na.exclude, method="REML")
summary(modBABA) 
plot(modBABA)
qqnorm(modBABA)

plot(x=datDA2$tree, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA2$plot, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$site, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleALMP, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleGSP_CMI, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleGS_temp, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA2$scaleBABA, y=resid(modBABA), type="pearson")
abline(h = 0, lty = 2) #
#########
######## dying 
###### 
head(data4)
dat_dead=subset(data4,status=="dying")
datM_lm=subset(dat_dead,year>=1997 & year<=2013)
str(datM_lm)
names(datM_lm)
datM_lm[] <- lapply(datM_lm, function(x) if(is.factor(x)) factor(x) else x)
str(datM_lm)
library(data.table)
datM_lm=as.data.table(datM_lm)

##########
datM_lm3=datM_lm[,c(1:7,22:25)]
names(datM_lm3)
datM_lm3=na.omit(datM_lm3)
datMD=merge(datM_lm3,aspD4,by=c("plot","year","site"),all=TRUE)

names(datMD)
str(datMD)

########## 
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleDA,data=datMD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleDABA,data=datMD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleBABA,data=datMD)) ## not collinear
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI+scaleBA,data=datMD)) ## not collinear

########## ASPEN DENSITY
datDA_M2=na.omit(datMD)
########################
modDA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleDA,random=~1|site/plot/tree,data=datDA_M2,
            na.action=na.exclude, method="ML")
summary(modDA_M) 
modDA_M1=update(modDA_M,~.-scaleGSP_CMI)
model.sel(modDA_M1,modDA_M,rank = AICc) # remove
anova(modDA_M1,modDA_M)
modDA_M2=update(modDA_M,~.-scaleGS_temp)
model.sel(modDA_M2,modDA_M,rank = AICc) # remove
anova(modDA_M2,modDA_M)
modDA_M3=update(modDA_M,~.-scaleALMP)
model.sel(modDA_M3,modDA_M,rank = AICc) # keep
anova(modDA_M3,modDA_M)
modDA_M4=update(modDA_M,~.-scaleDA)
model.sel(modDA_M4,modDA_M,rank = AICc)# remove
anova(modDA_M4,modDA_M)

######## compare null to best model
modDA_M_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datDA_M2,
                na.action=na.exclude, method="ML")
modDA_M0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA_M2,
             na.action=na.exclude, method="ML")
model.sel(modDA_M_new,modDA_M0,rank = AICc) ##da new
anova(modDA_M_new,modDA_M0)

########## full model results 
modDA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleDA,random=~1|site/plot/tree,data=datDA_M2,
          na.action=na.exclude, method="REML")
summary(modDA_M) 
plot(modDA_M)
qqnorm(modDA_M)
plot(x=datDA_M2$tree, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA_M2$plot, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$site, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleALMP, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGSP_CMI, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGS_temp, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleDA, y=resid(modDA_M), type="pearson")
abline(h = 0, lty = 2) #
#########
######### OTHER DENSTIY 
modDABA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
                scaleDABA,random=~1|site/plot/tree,data=datDA_M2,
              na.action=na.exclude, method="ML")
summary(modDABA_M) 
modDABA_M1=update(modDABA_M,~.-scaleGSP_CMI)
model.sel(modDABA_M1,modDABA_M,rank = AICc) # remove
anova(modDABA_M1,modDABA_M)
modDABA_M2=update(modDABA_M,~.-scaleGS_temp)
model.sel(modDABA_M2,modDABA_M,rank = AICc) # remove
anova(modDABA_M2,modDABA_M)
modDABA_M3=update(modDABA_M,~.-scaleALMP)
model.sel(modDABA_M3,modDABA_M,rank = AICc) # keep
anova(modDABA_M3,modDABA_M)
modDABA_M4=update(modDABA_M,~.-scaleDABA)
model.sel(modDABA_M4,modDABA_M)# remove
anova(modDABA_M4,modDABA_M)

######## compare null to best model
modDABA_M_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datDA_M2,
                na.action=na.exclude, method="ML")
modDABA_M0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA_M2,
             na.action=na.exclude, method="ML")
model.sel(modDABA_M_new,modDABA_M0,rank = AICc) ##da new
anova(modDABA_M_new,modDABA_M0)
##
############## full model results 
modDABA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleDABA,random=~1|site/plot/tree,data=datDA_M2,
            na.action=na.exclude, method="REML")
summary(modDABA_M) 
plot(modDABA_M)
qqnorm(modDABA_M)

plot(x=datDA_M2$tree, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA_M2$plot, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$site, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleALMP, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGSP_CMI, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDABA_M2$scaleGS_temp, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleDABA, y=resid(modDABA_M), type="pearson")
abline(h = 0, lty = 2) #

######### ASPEN BIOMASS 
modBA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleBA,random=~1|site/plot/tree,data=datDA_M2,
            na.action=na.exclude, method="ML")
summary(modBA_M) 

modBA_M1=update(modBA_M,~.-scaleGSP_CMI)
model.sel(modBA_M1,modBA_M,rank = AICc) # remove
anova(modBA_M1,modBA_M)
modBA_M2=update(modBA_M,~.-scaleGS_temp)
model.sel(modBA_M2,modBA_M,rank = AICc) # remove
anova(modBA_M2,modBA_M)
modBA_M3=update(modBA_M,~.-scaleALMP)
model.sel(modBA_M3,modBA_M,rank = AICc) # keep
anova(modBA_M3,modBA_M)
modBA_M4=update(modBA_M,~.-scaleBA)
model.sel(modBA_M4,modBA_M)# remove
anova(modBA_M,modBA_M4)

######## compare null to best model
modBA_M_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datDA_M2,
                  na.action=na.exclude, method="ML")
modBA_M_M0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA_M2,
               na.action=na.exclude, method="ML")
model.sel(modBA_M_new,modBA_M_M0,rank = AICc) ##da new
anova(modBA_M_new,modBA_M_M0)
##
######### full model results 
modBA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
            scaleBA,random=~1|site/plot/tree,data=datDA_M2,
          na.action=na.exclude, method="REML")
summary(modBA_M) 
plot(modBA_M)
qqnorm(modBA_M)

plot(x=datDA_M2$tree, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA_M2$plot, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$site, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleALMP, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGSP_CMI, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGS_temp, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$ scaleBA, y=resid(modBA_M), type="pearson")
abline(h = 0, lty = 2) #

######### OTHER BIOMASS 
#############################
modBABA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
                scaleBABA,random=~1|site/plot/tree,data=datDA_M2,
              na.action=na.exclude, method="ML")
summary(modBABA_M) 

modBABA_M1=update(modBABA_M,~.-scaleGSP_CMI)
model.sel(modBABA_M1,modBABA_M,rank = AICc) # remove
anova(modBABA_M1,modBABA_M)
modBABA_M2=update(modBABA_M,~.-scaleGS_temp)
model.sel(modBABA_M2,modBABA_M,rank = AICc) # remove
anova(modBABA_M2,modBABA_M)
modBABA_M3=update(modBABA_M,~.-scaleALMP)
model.sel(modBABA_M3,modBABA_M,rank = AICc) # keep
anova(modBABA_M3,modBABA_M)
modBABA_M4=update(modBABA_M,~.-scaleBABA)
model.sel(modBABA_M4,modBABA_M,rank = AICc)# remove
anova(modBABA_M4,modBABA_M)

######## compare null to best model
modBABA_M_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datDA_M2,
                na.action=na.exclude, method="ML")
modBABA_M0=lme(log.bai~1,random=~1|site/plot/tree,data=datDA_M2,
               na.action=na.exclude, method="ML")
model.sel(modBABA_M_new,modBABA_M0,rank = AICc) ##da new
anova(modBABA_M_new,modBABA_M0)
##
######### full model results 
modBABA_M=lme(log.bai~scaleGSP_CMI+scaleGS_temp+scaleALMP+
              scaleBABA,random=~1|site/plot/tree,data=datDA_M2,
            na.action=na.exclude, method="REML")
summary(modBABA_M) 
plot(modBABA_M)
qqnorm(modBABA_M)

plot(x=datDA_M2$tree, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2)
plot(x=datDA_M2$plot, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$site, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleALMP, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGSP_CMI, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$scaleGS_temp, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datDA_M2$ scaleBABA, y=resid(modBABA_M), type="pearson")
abline(h = 0, lty = 2) #
#############
################## figures
head(aspD4)
names(aspD4)

mod_dens=lme(aspen_stems.ha~year,random = ~1|site/plot,data=aspD4,method="REML",
             na.action = na.exclude)
summary(mod_dens)
plot(mod_dens)
plot(allEffects(mod_dens))

##########################
#############################
ggplot()+geom_point(data=aspD4,aes(x=year,y=aspen_stems.ha,color=plot))
library(effects)
effD<- effect(term=c("year"),mod=mod_dens)
effD=as.data.frame(effD)
head(effD)

plotD=ggplot()+
  geom_point(data=aspD4,aes(x=year,y=aspen_stems.ha),shape=19,size=1.5)+
  geom_ribbon(data=effD,aes(x=year,ymin=lower, 
                                ymax = upper),
              alpha=0.1,linetype=0,na.rm = TRUE,fill="black")+
  geom_line(data=effD,aes(y=fit,x=year),lwd=1,color="black")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14),
        legend.position = "top")+
  theme(text=element_text(family="Helvetica"))+labs(fill = "Plot")+
  xlab("Year")+ylab("Aspen density (stems/ha)")+
  geom_text(aes(label = "(a)"), x = 1997, y=3000,fontface=2,size=4.5) 

plotD

#######
####################
mod_bio=lme(aspen_biomass~year,random = ~1|site/plot,data=aspD4,method="REML",
             na.action = na.exclude)
summary(mod_bio)
plot(mod_bio)
plot(allEffects(mod_bio))
######################################


effB<- effect(term=c("year"),mod=mod_bio)
effB=as.data.frame(effB)
head(effB)

plotB=ggplot()+
  geom_point(data=aspD4,aes(x=year,y=aspen_biomass),shape=19,size=1.5)+
  geom_ribbon(data=effB,aes(x=year,ymin=lower, 
                            ymax = upper),
              alpha=0.1,linetype=0,na.rm = TRUE,fill="black")+
  geom_line(data=effB,aes(y=fit,x=year),lwd=1,color="black")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14))+
  theme(text=element_text(family="Helvetica"),
        legend.position = "top")+labs(fill = "Plot")+
  xlab("Year")+ylab("Aspen biomass (Mg/ha)")+
geom_text(aes(label = "(b)"), x = 1997, y=120,fontface=2,size=4.5) 

plotB

################
###################### bai over time 
####################
head(datAD)
str(datAD)
datAD2=subset(datAD,year<=2012)
mod_bai=lme(log.bai~year,random = ~1|site/plot/tree,data=datAD2,method="REML",
            na.action = na.exclude)
summary(mod_bai)
plot(mod_bai)
plot(allEffects(mod_bai))
######################################

effBAI<- effect(term=c("year"),mod=mod_bai)
effBAI=as.data.frame(effBAI)
head(effBAI)

plotBAI=ggplot()+
  geom_point(data=datAD2,aes(x=year,y=log.bai),shape=19,size=1.5)+
  geom_ribbon(data=effBAI,aes(x=year,ymin=lower, 
                            ymax = upper),
              alpha=0.1,linetype=0,na.rm = TRUE,fill="black")+
  geom_line(data=effBAI,aes(y=fit,x=year),lwd=1,color="black")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14))+
  theme(text=element_text(family="Helvetica"),
        legend.position = "top")+labs(fill = "Plot")+
  xlab("Year")+
  labs(y=expression(paste(BAI  (ln* ("mm"^2)))))+
  geom_text(aes(label = "(c)"), x = 1997, y=7.3,fontface=2,size=4.5) 

plotBAI


##############################
library(ggpubr)
ggarrange(plotD, plotB, plotBAI, ncol=1, nrow=3)

plot_aspen=ggarrange(plotD, plotB, plotBAI, ncol=1, nrow=3)
ggsave(plot = plot_aspen, filename = "plot_aspen.jpg",path="C:/Users/mab834/Downloads/CAFI_R_April2020/CAFI_R_April2020",
       dpi = 600, width = 5, height = 8)

##################
########################
##########