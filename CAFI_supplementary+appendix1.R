#####
#######
########## APPENDICES + additional supplementary tables + figures + 
#####
####
rm(list = ls()) # Clear out the workspace
library(dplR) 
library(graphics)
library(utils)
library(stats)
library(dplyr)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(nlme)
library(ggplot2)
library(data.table)

###############
################# APPENDIX 1 ------- CAFI COMPARISONS 
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")

library(ggplot2)
library(data.table)
library(dplyr)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(nlme)
library(tidyr)
library(emmeans)
library(lmerTest)
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Data Cleaning and Organizing
##########Load in data created in previous Rmd file.
dat <- read.csv('CAFI_Mortality_Data_Updated.csv', stringsAsFactors = FALSE)
head(dat)
##########Grab final observation for each tree.

#get final observation for eahc tree
final_obs <- group_by(dat, TREE_ID) %>% 
  dplyr::summarize(final_year = max(Sample_Time))

#merge with original data frame
dat_m <- merge(dat, final_obs, by = 'TREE_ID')

#subset to get only final observation
dat_final <- subset(dat_m, dat_m$Sample_Time == dat_m$final_year)
head(dat_final)

#take a look at the data - what statuses are here?
gg <- ggplot(data = dat_final, aes(x = as.factor(Site)))
gg + geom_histogram(stat = 'count', aes(fill = as.factor(STAT_new))) +
  scale_fill_manual(values = c('forest green', 'gray'),
                    labels = c('Live', 'Dead'),
                    name = 'Status') +
  xlab('Site') +
  theme(axis.text.x = element_text(hjust = 1, angle = 90))


####### what i need to do is change canopy position labels - so dom/codom = upper and intermediate/suppressed = lower
head(dat_final)
names(dat_final)
str(dat_final)
#######
######in CAFI: 0= no estimate, dom=1, codom==2, interm=3, suppressed=4, understory=5
### also understory = 5
### do anlayses w/and without understory included in int/supp category
### stat alive = 1, stat dead = 2

dat_final$CRCL=as.factor(dat_final$CRCL)
str(dat_final)
dat_final$STAT_new=as.factor(dat_final$STAT_new) 
str(dat_final)## STAT_new has 2 levels
dat_final$CRCL_live=as.factor(dat_final$CRCL_live) 
str(dat_final)## CRCL_live has 5 levels 1,2,3,4,5
head(dat_final)
str(dat_final)
####### rename crown levels
dat_final2=dat_final
dat_final2=as.data.table(dat_final2)
invisible(dat_final2[ CRCL_live == "1", CRCL_live := "dom"])
invisible(dat_final2[ CRCL_live == "2", CRCL_live := "dom"])
invisible(dat_final2[ CRCL_live == "3", CRCL_live := "sub"])
invisible(dat_final2[ CRCL_live == "4", CRCL_live := "sub"])
invisible(dat_final2[ CRCL_live == "5", CRCL_live := "sub"])
dat_final2[] <- lapply(dat_final2, function(x) if(is.factor(x)) factor(x) else x) 
str(dat_final2)

invisible(dat_final2[ STAT_new == "0", STAT_new := "live_cafi"])
invisible(dat_final2[ STAT_new == "2", STAT_new := "dead_cafi"])
dat_final2[] <- lapply(dat_final2, function(x) if(is.factor(x)) factor(x) else x) ### no observations for 1079
str(dat_final2)
names(dat_final2)
###

dat_final2$TREE_ID=as.factor(dat_final2$TREE_ID)
dat_final2$Site=as.factor(dat_final2$Site)
dat_final2$PSP=as.factor(dat_final2$PSP)
str(dat_final2)
#########
#################compare canopy and dbh in sampling time 1 
#get first observation for each tree

dat2=subset(dat,Sample_Time=="1")
####
names(dat2)
dat2b=dat2[,c(1,2,3,4,5,8:10)]

########################
names(dat_final2)
dat_final3=dat_final2[,c(1,4,5,7)]
names(dat_final3)
###
################

dat3=merge(dat2b,dat_final3,by=c("TREE_ID","Site","PSP"),all=TRUE)
names(dat3)
####### rename crown levels
dat3=as.data.table(dat3)
dat3$CRCL=as.factor(dat3$CRCL)
invisible(dat3[ CRCL == "1", CRCL := "dom"])
invisible(dat3[ CRCL == "2", CRCL := "dom"])
invisible(dat3[ CRCL == "3", CRCL := "sub"])
invisible(dat3[ CRCL == "4", CRCL := "sub"])
invisible(dat3[ CRCL == "5", CRCL := "sub"])
dat3[] <- lapply(dat3, function(x) if(is.factor(x)) factor(x) else x) 
str(dat3)

datMC=subset(dat3,STAT_new=="dead_cafi")
datMC[] <- lapply(datMC, function(x) if(is.factor(x)) factor(x) else x) 
str(datMC) ##471 dead trees 

datAC=subset(dat3,STAT_new=="live_cafi")
datAC[] <- lapply(datAC, function(x) if(is.factor(x)) factor(x) else x) 
str(datAC) ##711 live trees
### --------------- differences in canopy bewteen live and dead trees based on CAFI database
#######
################ at first year of measurement 
names(dat3)
cafF=dat3[,c(8,9)]
names(cafF)
str(cafF)
head(cafF)
cafF2=na.omit(cafF)
head(cafF2)
str(cafF2)
x.tabF=chisq.test(x=cafF2$STAT_new,y=cafF2$CRCL,correct = TRUE) ## not sure if this is should be true of false...
x.tabF
caf.tabF=table(cafF2)##p-value of less that 0.05 significance level.
caf.tabF
x.tabF$expected#
x.tabF#
caf.tabF=as.data.table(caf.tabF)
head(caf.tabF)

510/(510+201)
201/(510+201)

161/(161+310)
310/(161+310)

#####
names(dat3)
####### logistic regressoin
m1c <- glmer(CRCL~STAT_new+(1|Site/PSP),data=dat3, 
            family = binomial(link = "logit"))
summary(m1c)

m1d <- glmer(CRCL~1+(1|Site/PSP),data=dat3, 
             family = binomial(link = "logit"))
summary(m1d)
model.sel(m1c,m1d,rank = AICc) ## m1c
anova(m1c,m1d)

plot(allEffects(m1c))
#### compare live and dead CAFI dbh
names(dat3)
dat3_dbh=dat3[,c(1,2,3,6,9)]
head(dat3_dbh)
str(dat3_dbh)
dat3_dbh$TREE_ID=as.factor(dat3_dbh$TREE_ID)
dat3_dbh$Site=as.factor(dat3_dbh$Site)
dat3_dbh$PSP=as.factor(dat3_dbh$PSP)
#####
########### 
############ mixed model comparing live and dead tree dbh
hist(dat3_dbh$DBH)
dat3_dbh$STAT_new <- relevel(dat3_dbh$STAT_new, ref="live_cafi")

mod.dbh2=lme(log(DBH_cm)~STAT_new,
             random=~1|Site/PSP,data=dat3_dbh,
             na.action=na.exclude,method = "ML")
mod.dbh2b=lme(log(DBH_cm)~1,
             random=~1|Site/PSP,data=dat3_dbh,
             na.action=na.exclude,method = "ML")
anova(mod.dbh2b,mod.dbh2)
model.sel(mod.dbh2b,mod.dbh2,rank=AICc) #######status sig 

#####final model 
mod.dbh2=lme(log(DBH_cm)~STAT_new,
             random=~1|Site/PSP,data=dat3_dbh,
             na.action=na.exclude,method = "REML")
summary(mod.dbh2)
plot(allEffects(mod.dbh2))
plot(mod.dbh2)
qqnorm(resid(mod.dbh2))
plot(x=dat3_dbh$Site, y=resid(mod.dbh2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat3_dbh$STAT_new, y=resid(mod.dbh2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat3_dbh$PSP, y=resid(mod.dbh2), type="pearson")
abline(h = 0, lty = 2) #

###########
################ -------- SUPPLEMENTARY TABLES -------------

### TABLE 1 - NO STATS

######## ----- TABLE S2 - CHRONOLOGY STATS 
############## REMOVE TREES THAT DIED BEFORE 1997; STATS ARE FROM 1946-2013
############### dead trees
## tm3, tm7,tm1,tm6

setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020/RRW")
dat1=read.csv("RRW_61-63.csv",header=T,row.names = 1)  #####1021
names(dat1)
dat1b=dat1[c(46:113),]
dat1b2<- bai.out(rwl = dat1b)
#### remove trees that died before 1997
dat1b3=dat1b2[,-c(6,7,11,10)]
names(dat1b3)
rwi.stats(dat1b3)
rwl.report(dat1b3)
str(dat1b3)
dim(dat1b3)
##############
##tm 8
dat2=read.csv("RRW_94-96.csv",header=T,row.names = 1) ##1032
names(dat2)
dat2b=dat2[c(46:113),]
dat2b2<- bai.out(rwl = dat2b)
#### remove trees that died before 1997
dat2b3=dat2b2[,-c(9)]
rwi.stats(dat2b3)
rwl.report(dat2b3)
dim(dat2b3)
############### 
###tm8
dat3=read.csv("RRW_233.csv",header=T,row.names = 1)##1079
names(dat3)
dat3b=dat3[c(46:113),]
#View(dat3b)
dat3b2<- bai.out(rwl = dat3b)
#### remove trees that died before 1997
dat3b3=dat3b2[,-c(8)]

rwi.stats(dat3b3)
rwl.report(dat3b3)
dim(dat3b3)
############### 
dat4=read.csv("RRW_97-99.csv",header=T,row.names = 1) ##1033
names(dat4)
dat4b=dat4[c(46:113),]
#View(dat3b)
dat4b2<- bai.out(rwl = dat4b)
rwi.stats(dat4b2)
rwl.report(dat4b2)
dim(dat4b2)
############### 
##tm11
dat5=read.csv("RRW_307-309.csv",header=T,row.names = 1) ##1104
names(dat5)
dat5b=dat5[c(46:113),]
#View(dat5b)
#### remove trees that died before 1997
dat5b2=dat5b[,-c(23)]

dat5b3<- bai.out(rwl = dat5b2)
rwi.stats(dat5b3)
rwl.report(dat5b3)
dim(dat5b3)
############### 
##tm4,tm5,tm10,tm7
dat6=read.csv("RRW_199-201.csv",header=T,row.names = 1) ####1067
names(dat6)
dat6b=dat6[c(46:113),]
#View(dat6b)
dat6b2<- bai.out(rwl = dat6b)
#### remove trees that died before 1997
dat6b3=dat6b2[,-c(13,14,11,16)]
names(dat6b3)

rwi.stats(dat6b3)
rwl.report(dat6b3)
dim(dat6b3)
############### 
dat7=read.csv("RRW_298-300.csv",header=T,row.names = 1) #####1101
##tm2,tm6
names(dat7)
dat7b=dat7[c(46:113),]
#View(dat7b)
dat7b2<- bai.out(rwl = dat7b)
#### remove trees that died before 1997
dat7b3=dat7b2[,-c(2,6)]
names(dat7b3)

rwi.stats(dat7b3)
rwl.report(dat7b3)
dim(dat7b3)

############### 
dat8=read.csv("RRW_91-93.csv",header=T,row.names = 1) ####1031
names(dat8)
dat8b=dat8[c(46:113),]
#View(dat8b)
dat8b2<- bai.out(rwl = dat8b)
rwi.stats(dat8b2)
rwl.report(dat8b2)
dim(dat8b2)
##################
######################
######################## ----- Table S3 ---- year of establishment comparisons 
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")

cafE=read.csv("CAFI_ENVIRO_new.csv")
head(cafE)
str(cafE)
#names(cafE)[names(cafE) == "ï..tree"] <- "tree"

#### remove trees that died after 1997
cafE2=subset(cafE, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&
               tree!="TM6.61"& tree!="TM8.94")

cafE2[] <- lapply(cafE2, function(x) if(is.factor(x)) factor(x) else x)
str(cafE2)
############
data1=read.csv("CAFI_BAI.csv")
names(data1)
#names(data1)[names(data1) == "ï..year"] <- "year"

############remove trees that died before 1997
data2=subset(data1, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&
               tree!="TM6.61"& tree!="TM8.94")
names(data2)

data2[] <- lapply(data2, function(x) if(is.factor(x)) factor(x) else x)
data2=as.data.frame(data2)
data2$site=as.factor(data2$site)
data2$plot=as.factor(data2$plot)
str(data2)
########### merge data2 and cafE2 to get plot name for cafE2
dat2.plot=subset(data2,year=="1986")
str(dat2.plot)
names(dat2.plot)
head(dat2.plot)
dat3.plot=dat2.plot[,c(2,3,4)]
head(dat3.plot)
head(cafE2)
cafE2=as.data.table(cafE2)
invisible(cafE2[site == "199-201", site := "1067"])
invisible(cafE2[site == "233", site := "1079"])
invisible(cafE2[site == "298-300", site := "1101"])
invisible(cafE2[site == "307-309", site := "1104"])
invisible(cafE2[site == "91-93", site := "1031"])
invisible(cafE2[site == "94-96", site := "1032"])
invisible(cafE2[site == "97-99", site := "1033"])
invisible(cafE2[site == "61-63", site := "1021"])

cafE3=merge(cafE2,dat3.plot,by=c("tree","site"))
cafE3[] <- lapply(cafE3, function(x) if(is.factor(x)) factor(x) else x)
head(cafE3)
names(cafE3)
str(cafE3)
######
#write.csv(cafE3,file="cafE3.csv")
############ tree age mean +- SE in 2016 for live trees and year of death
caf_age=cafE3[,c(3,10)]
head(caf_age)
caf_age2=na.omit(caf_age)
##########
caf_age3 <-  
  ddply(caf_age2, c("status"), summarise,
        N    = length(age_yr_death),
        meanB = mean(age_yr_death),
        sdB   = sd(age_yr_death),
        seB = sdB / sqrt(N))
caf_age3
#######
################## YEAR OF ESTABLISHMENT
cafE3$status <- relevel(cafE3$status, ref="A")
str(cafE3)
names(cafE3)
cafE3$tree=as.factor(cafE3$tree)
cafE3$site=as.factor(cafE3$site)
cafE3$status=as.factor(cafE3$status)

plot(yr_estab~plot,data=cafE3)
cafE4=cafE3[,c(1,2,3,8,12)]
names(cafE4)
cafE4=na.omit(cafE4)
######
lm_estab=lme(yr_estab~status,random=~1|site/plot,data=cafE4,
             na.action=na.exclude, method="ML")
lm_estab2=lme(yr_estab~1,random=~1|site/plot,data=cafE4,
              na.action=na.exclude, method="ML")
model.sel(lm_estab2,lm_estab,rank=AICc)
anova(lm_estab2,lm_estab) #yr of estab not different 
####
##### final model
lm_estab=lme(yr_estab~status,random=~1|site/plot,data=cafE4,
             na.action=na.exclude, method="REML")
summary(lm_estab)
plot(lm_estab) 
qqnorm(resid(lm_estab)) #### not normal, remove some points and see if results change 
plot(x=cafE4$status, y=resid(lm_estab), type="pearson")
abline(h = 0, lty = 2) #
plot(x=cafE4$site, y=resid(lm_estab), type="pearson")
abline(h = 0, lty = 2) #
plot(x=cafE4$plot, y=resid(lm_estab), type="pearson")
abline(h = 0, lty = 2) #

##########
###### remove weird plot (10309)) ### just a lot of variability in tree age 
yr2=subset(cafE4, plot!="10309")  
mod_yr2=lme(yr_estab~status,random=~1|site/plot,data=yr2,
            na.action=na.exclude, method="REML")
summary(mod_yr2) ### same results as mod1 ### KEEP POINTS 
plot(mod_yr2) 
qqnorm(resid(mod_yr2))

#### TABLE S4 IS IN R FILE: CAFI_C-G_beforeALM

#### TABLE S5 IS IN R FILE: CAFI_clim*alm

#### TABLE S6 IS IN R FILE: CAFI_NDVI_BAI

#### TABLE S7 IS IN R FILE: CAFI_dbh+canopy

#### TABLE S8 IS IN R FILE: CAFI_dbh+canopy

#### TABLE S9 IS IN R FILE: CAFI_dbh+canopy

#### TABLE S10 IS IN R FILE: CAFI_productivity_beforeALM

#### 
#######################TABLE S11 
################ trends in living and dying tree BAI 
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
data1=read.csv("CAFI_BAI.csv")
names(data1)
#names(data1)[names(data1) == "ï..year"] <- "year"

data2=data1%>% 
  rowwise()%>%
  mutate(log.bai = log1p(bai))  

names(data2)
head(data2)
tail(data2)
###########
############## REMOVE TREES THAT DIED BEFORE 1997
data3=subset(data2, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
               tree!="TM6.61"& tree!="TM8.94")

head(data3)
tail(data3)

dat_p1=subset(data3,year>=1946 & year<=1957)
dat_p2=subset(data3,year>=1958 & year<=1996)
dat_p3=subset(data3,year>=1997 & year<=2013)


######-------LIVING TREES 
datA_p1=subset(dat_p1,status=="living")
datA_p1[] <- lapply(datA_p1, function(x) if(is.factor(x)) factor(x) else x)
datA_p2=subset(dat_p2,status=="living")
datA_p2[] <- lapply(datA_p2, function(x) if(is.factor(x)) factor(x) else x)
datA_p3=subset(dat_p3,status=="living")
datA_p3[] <- lapply(datA_p3, function(x) if(is.factor(x)) factor(x) else x)
######
########## 
modA1=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p1,
          na.action=na.exclude,method = "ML")#
modA1b=lme(log.bai~1,random = ~1|site/plot/tree,data=datA_p1,
          na.action=na.exclude,method = "ML")#
model.sel(modA1,modA1b,rank=AICc) ###sig
anova(modA1,modA1b)

modA1=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p1,
          na.action=na.exclude,method = "REML")#increase
summary(modA1)
plot(modA1)
qqnorm(resid(modA1))
plot(x=datA_p1$site, y=resid(modA1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p1$plot, y=resid(modA1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p1$tree, y=resid(modA1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p1$year, y=resid(modA1), type="pearson")
abline(h = 0, lty = 2) #

modA2=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p2,
          na.action=na.exclude,method = "ML")#
modA2b=lme(log.bai~1,random = ~1|site/plot/tree,data=datA_p2,
          na.action=na.exclude,method = "ML")#
model.sel(modA2,modA2b,rank=AICc) ###sig
anova(modA2,modA2b)

modA2=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p2,
          na.action=na.exclude,method = "REML")#increase
summary(modA2)
plot(modA2)
qqnorm(resid(modA2))
plot(x=datA_p2$site, y=resid(modA2), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datA_p2$plot, y=resid(modA2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p2$tree, y=resid(modA2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p2$year, y=resid(modA2), type="pearson")
abline(h = 0, lty = 2) #

modA3=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p3,
          na.action=na.exclude,method = "ML")#
modA3b=lme(log.bai~1,random = ~1|site/plot/tree,data=datA_p3,
           na.action=na.exclude,method = "ML")#i
model.sel(modA3,modA3b,rank=AICc) ###sig
anova(modA3,modA3b)

modA3=lme(log.bai~year,random = ~1|site/plot/tree,data=datA_p3,
          na.action=na.exclude,method = "REML")##DECREASE 
summary(modA3)
plot(modA3)
qqnorm(resid(modA3))
plot(x=datA_p3$site, y=resid(modA3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p3$plot, y=resid(modA3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p3$tree, y=resid(modA3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_p3$year, y=resid(modA3), type="pearson")
abline(h = 0, lty = 2) #

######-------DYING TREES 
datM_p1=subset(dat_p1,status=="dying")
datM_p1[] <- lapply(datM_p1, function(x) if(is.factor(x)) factor(x) else x)
datM_p2=subset(dat_p2,status=="dying")
datM_p2[] <- lapply(datM_p2, function(x) if(is.factor(x)) factor(x) else x)
datM_p3=subset(dat_p3,status=="dying")
datM_p3[] <- lapply(datM_p3, function(x) if(is.factor(x)) factor(x) else x)
######
########## 
modM1=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p1,
          na.action=na.exclude,method = "ML")#
modM1b=lme(log.bai~1,random = ~1|site/plot/tree,data=datM_p1,
           na.action=na.exclude,method = "ML")#
model.sel(modM1,modM1b,rank=AICc) ###sig
anova(modM1,modM1b)

modM1=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p1,
          na.action=na.exclude,method = "REML")
summary(modM1)#increase
plot(modM1)
qqnorm(resid(modM1))
plot(x=datM_p1$site, y=resid(modM1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p1$plot, y=resid(modM1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p1$tree, y=resid(modM1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p1$year, y=resid(modM1), type="pearson")
abline(h = 0, lty = 2) #

modM2=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p2,
          na.action=na.exclude,method = "ML")#
modM2b=lme(log.bai~1,random = ~1|site/plot/tree,data=datM_p2,
           na.action=na.exclude,method = "ML")#
model.sel(modM2,modM2b,rank=AICc) ###sig
anova(modM2,modM2b)

modM2=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p2,
          na.action=na.exclude,method = "REML")
summary(modM2) #### DECREASE
plot(modM2)
qqnorm(resid(modM2))
plot(x=datM_p2$site, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p2$plot, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p2$tree, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p2$year, y=resid(modM2), type="pearson")
abline(h = 0, lty = 2) #

modM3=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p3,
          na.action=na.exclude,method = "ML")#
modM3b=lme(log.bai~1,random = ~1|site/plot/tree,data=datM_p3,
           na.action=na.exclude,method = "ML")#
model.sel(modM3,modM3b,rank=AICc) ###sig
anova(modM3,modM3b)

modM3=lme(log.bai~year,random = ~1|site/plot/tree,data=datM_p3,
          na.action=na.exclude,method = "REML")
summary(modM3)
plot(modM3)
qqnorm(resid(modM3))
plot(x=datM_p3$site, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p3$plot, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p3$tree, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_p3$year, y=resid(modM3), type="pearson")
abline(h = 0, lty = 2) #

######
####

######### TABLE S12 IS IN R FILE: CAFI_c-g_beforeALM

######### TABLE S13 IS IN R FILE: CAFI_c-g_beforeALM

#########
############# ------Table S14 ---------- TRENDS IN CLIMATE 
#######################
####
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
########### CLIMATE DATA 
data.clim2=read.csv("CAFI_climate_archivalB.csv",header = T)
names(data.clim2)
head(data.clim2)
str(data.clim2) 
data.clim2$site=as.factor(data.clim2$site)
##########
head(data.clim2)
names(data.clim2)
#####
########## CMI 
#####------------TRENDS FROM 1946 - 1957 
data.climA=subset(data.clim2,year>=1946 & year<=1957)
head(data.climA)
tail(data.climA)

cmiGS_A=lme(GS_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="ML")
cmiGS_Ab=lme(GS_CMI~1,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
anova(cmiGS_A,cmiGS_Ab)
model.sel(cmiGS_A,cmiGS_Ab,rank=AICc) # not sig 
####
cmiGS_A=lme(GS_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="REML")
summary(cmiGS_A) ##### 
plot(cmiGS_A)
qqnorm(resid(cmiGS_A))
plot(x=data.climA$year, y=resid(cmiGS_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(cmiGS_A), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
cmiSP_A=lme(Spring_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="ML")
cmiSP_Ab=lme(Spring_CMI~1,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
anova(cmiSP_A,cmiSP_Ab)
model.sel(cmiSP_A,cmiSP_Ab,rank=AICc) # not sig

cmiSP_A=lme(Spring_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="REML")
summary(cmiSP_A) ##### 
plot(cmiSP_A)
qqnorm(resid(cmiGS_A))
plot(x=data.climA$year, y=resid(cmiSP_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(cmiSP_A), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
cmiFW_A=lme(FW_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="ML")
cmiFW_Ab=lme(FW_CMI~1,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
anova(cmiFW_A,cmiFW_Ab)
model.sel(cmiFW_A,cmiFW_Ab,rank=AICc) # not sig

cmiFW_A=lme(FW_CMI~year,random=~1|site,data=data.climA,
            na.action=na.exclude,method="REML")
summary(cmiFW_A) ##### 
plot(cmiFW_A)
qqnorm(resid(cmiFW_A))
plot(x=data.climA$year, y=resid(cmiFW_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(cmiGS_A), type="pearson")
abline(h = 0, lty = 2) #
#####
########## TRENDS FROM 1971-1982 
data.climB=subset(data.clim2,year>=1971 & year<=1982)
head(data.climB)
tail(data.climB)

cmiGS_B=lme(GS_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="ML")
cmiGS_Bb=lme(GS_CMI~1,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
anova(cmiGS_B,cmiGS_Bb)
model.sel(cmiGS_B,cmiGS_Bb,rank=AICc) # not sig

cmiGS_B=lme(GS_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="REML")
summary(cmiGS_B) ##### 
plot(cmiGS_B)
qqnorm(resid(cmiGS_B))
plot(x=data.climB$year, y=resid(cmiGS_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(cmiGS_B), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
cmiSP_B=lme(Spring_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="ML")
cmiSP_Bb=lme(Spring_CMI~1,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
anova(cmiSP_B,cmiSP_Bb)
model.sel(cmiSP_B,cmiSP_Bb,rank=AICc) # not sig

cmiSP_B=lme(Spring_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="REML")
summary(cmiSP_B) ##### 
plot(cmiSP_B)
qqnorm(resid(cmiGS_B))
plot(x=data.climB$year, y=resid(cmiSP_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(cmiSP_B), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
cmiFW_B=lme(FW_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="ML")
cmiFW_Bb=lme(FW_CMI~1,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
anova(cmiFW_B,cmiFW_Bb)
model.sel(cmiFW_B,cmiFW_Bb,rank=AICc) # SIG

cmiFW_B=lme(FW_CMI~year,random=~1|site,data=data.climB,
            na.action=na.exclude,method="REML")
summary(cmiFW_B) #####  
plot(cmiFW_B)####  outliers - precip was high in the CRV in winter in 1976, so cmi is high at 1031,1032,1067
ggplot(data.climB,aes(x=year,y=FW_CMI))+geom_point()+facet_wrap(~site)
#### remove these years/sites to see if results change 
ggplot(data.climB,aes(x=year,y=FW_CMI))+geom_point()+facet_wrap(~site)
qqnorm(resid(cmiFW_B))
plot(x=data.climB$year, y=resid(cmiFW_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(cmiGS_B), type="pearson")
abline(h = 0, lty = 2) #
##########
clim_RM=data.climB %>%
  filter(!(year==1977 & site==1067)) %>%  # remove these rows
  filter(!(year==1977 & site==1031)) %>%
  filter(!(year==1977 & site==1032))

###########
cmiFW_B2=lme(FW_CMI~year,random=~1|site,data=clim_RM,
             na.action=na.exclude,method="REML")
summary(cmiFW_B2) #####  decrease  - so same results keep original data frame 
plot(cmiFW_B2)####  
########
#############
########## TRENDS FROM 1983-1994
data.climC=subset(data.clim2,year>=1983 & year<=1994)
head(data.climC)
tail(data.climC)

cmiGS_C=lme(GS_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="ML")
cmiGS_Cb=lme(GS_CMI~1,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
anova(cmiGS_C,cmiGS_Cb)
model.sel(cmiGS_C,cmiGS_Cb,rank=AICc) # SIG

cmiGS_C=lme(GS_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="REML")
summary(cmiGS_C) ##### 
plot(cmiGS_C)
qqnorm(resid(cmiGS_C))
plot(x=data.climC$year, y=resid(cmiGS_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(cmiGS_C), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
cmiSP_C=lme(Spring_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="ML")
cmiSP_Cb=lme(Spring_CMI~1,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
anova(cmiSP_C,cmiSP_Cb)
model.sel(cmiSP_C,cmiSP_Cb,rank=AICc) # NOT SIG

cmiSP_C=lme(Spring_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="REML")
summary(cmiSP_C) ##### 
plot(cmiSP_C)
qqnorm(resid(cmiSP_C))
plot(x=data.climC$year, y=resid(cmiSP_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(cmiSP_C), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
cmiFW_C=lme(FW_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="ML")
cmiFW_Cb=lme(FW_CMI~1,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
anova(cmiFW_C,cmiFW_Cb)
model.sel(cmiFW_C,cmiFW_Cb,rank=AICc) #  SIG

cmiFW_C=lme(FW_CMI~year,random=~1|site,data=data.climC,
            na.action=na.exclude,method="REML")
summary(cmiFW_C) #####  
plot(cmiFW_C)#
qqnorm(resid(cmiFW_C))
plot(x=data.climC$year, y=resid(cmiFW_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(cmiGS_C), type="pearson")
abline(h = 0, lty = 2) #
#############
########## TRENDS FROM 1997 - 2013
data.climD=subset(data.clim2,year>=1997 & year<=2013)
head(data.climD)
tail(data.climD)

cmiGS_D=lme(GS_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="ML")
cmiGS_Db=lme(GS_CMI~1,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
anova(cmiGS_D,cmiGS_Db)
model.sel(cmiGS_D,cmiGS_Db,rank=AICc) # NOT SIG

cmiGS_D=lme(GS_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="REML")
summary(cmiGS_D) ##### 
plot(cmiGS_D)
qqnorm(resid(cmiGS_D))
plot(x=data.climD$year, y=resid(cmiGS_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(cmiGS_D), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
cmiSP_D=lme(Spring_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="ML")
cmiSP_Db=lme(Spring_CMI~1,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
anova(cmiSP_D,cmiSP_Db)
model.sel(cmiSP_D,cmiSP_Db,rank=AICc) # SIG

cmiSP_D=lme(Spring_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="REML")
summary(cmiSP_D) ##### 
plot(cmiSP_D)
qqnorm(resid(cmiSP_D))
plot(x=data.climD$year, y=resid(cmiSP_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(cmiSP_D), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
cmiFW_D=lme(FW_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="ML")
cmiFW_Db=lme(FW_CMI~1,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
anova(cmiFW_D,cmiFW_Db)
model.sel(cmiFW_D,cmiFW_Db,rank=AICc) # NOT SIG

cmiFW_D=lme(FW_CMI~year,random=~1|site,data=data.climD,
            na.action=na.exclude,method="REML")
summary(cmiFW_D) #####  
plot(cmiFW_D)#
qqnorm(resid(cmiFW_D))
plot(x=data.climD$year, y=resid(cmiFW_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(cmiGS_D), type="pearson")
abline(h = 0, lty = 2) #

########
###############
########## TEMPERATURE
######----------------------TRENDS FROM 1946 - 1957 
data.climA=subset(data.clim2,year>=1946 & year<=1957)
head(data.climA)
tail(data.climA)

tempGS_A=lme(GS_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
tempGS_Ab=lme(GS_temp~1,random=~1|site,data=data.climA,
              na.action=na.exclude,method="ML")
anova(tempGS_A,tempGS_Ab)
model.sel(tempGS_A,tempGS_Ab,rank=AICc) # NOT SIG

tempGS_A=lme(GS_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="REML")
summary(tempGS_A) ##### 
plot(tempGS_A)
qqnorm(resid(tempGS_A))
plot(x=data.climA$year, y=resid(tempGS_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(tempGS_A), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
tempSP_A=lme(Spring_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
tempSP_Ab=lme(Spring_temp~1,random=~1|site,data=data.climA,
              na.action=na.exclude,method="ML")
anova(tempSP_A,tempSP_Ab)
model.sel(tempSP_A,tempSP_Ab,rank=AICc) # NOT SIG

tempSP_A=lme(Spring_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="REML")
summary(tempSP_A) ##### 
plot(tempSP_A)
qqnorm(resid(tempGS_A))
plot(x=data.climA$year, y=resid(tempSP_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(tempSP_A), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
tempFW_A=lme(FW_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="ML")
tempFW_Ab=lme(FW_temp~1,random=~1|site,data=data.climA,
              na.action=na.exclude,method="ML")
anova(tempFW_A,tempFW_Ab)
model.sel(tempFW_A,tempFW_Ab,rank=AICc) # NOT SIG

tempFW_A=lme(FW_temp~year,random=~1|site,data=data.climA,
             na.action=na.exclude,method="REML")
summary(tempFW_A) ##### 
plot(tempFW_A)
qqnorm(resid(tempFW_A))
plot(x=data.climA$year, y=resid(tempFW_A), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climA$site, y=resid(tempGS_A), type="pearson")
abline(h = 0, lty = 2) #
#####
########## TRENDS FROM 1971-1982 
data.climB=subset(data.clim2,year>=1971 & year<=1982)
head(data.climB)
tail(data.climB)

tempGS_B=lme(GS_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
tempGS_Bb=lme(GS_temp~1,random=~1|site,data=data.climB,
              na.action=na.exclude,method="ML")
anova(tempGS_B,tempGS_Bb)
model.sel(tempGS_B,tempGS_Bb,rank=AICc) # NOT SIG

tempGS_B=lme(GS_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="REML")
summary(tempGS_B) ##### 
plot(tempGS_B)
qqnorm(resid(tempGS_B))
plot(x=data.climB$year, y=resid(tempGS_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(tempGS_B), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
tempSP_B=lme(Spring_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
tempSP_Bb=lme(Spring_temp~1,random=~1|site,data=data.climB,
              na.action=na.exclude,method="ML")
anova(tempSP_B,tempSP_Bb)
model.sel(tempSP_B,tempSP_Bb,rank=AICc) #  SIG

tempSP_B=lme(Spring_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="REML")
summary(tempSP_B) ##### 
plot(tempSP_B)
qqnorm(resid(tempGS_B))
plot(x=data.climB$year, y=resid(cmiSP_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(cmiSP_B), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
tempFW_B=lme(FW_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="ML")
tempFW_Bb=lme(FW_temp~1,random=~1|site,data=data.climB,
              na.action=na.exclude,method="ML")
anova(tempFW_B,tempFW_Bb)
model.sel(tempFW_B,tempFW_Bb,rank=AICc) #  SIG


tempFW_B=lme(FW_temp~year,random=~1|site,data=data.climB,
             na.action=na.exclude,method="REML")
summary(tempFW_B) #####  
plot(tempFW_B)####
qqnorm(resid(tempFW_B))

ggplot(data.climB,aes(x=year,y=FW_temp))+geom_point()+facet_wrap(~site)
#### remove 1977 see if it changes results 
qqnorm(resid(tempFW_B))
plot(x=data.climB$year, y=resid(tempFW_B), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climB$site, y=resid(tempGS_B), type="pearson")
abline(h = 0, lty = 2) #
##########
clim_RMT=data.climB %>%
  filter(!(year==1977))

###########
tempFW_B2=lme(FW_temp~year,random=~1|site,data=clim_RMT,
              na.action=na.exclude,method="REML")
summary(tempFW_B2) ##### INCREASE  - so same results keep original data frame 
plot(tempFW_B2)####  
########
#############
########## TRENDS FROM 1983-1994
data.climC=subset(data.clim2,year>=1983 & year<=1994)
head(data.climC)
tail(data.climC)

tempGS_C=lme(GS_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
tempGS_Cb=lme(GS_temp~1,random=~1|site,data=data.climC,
              na.action=na.exclude,method="ML")
anova(tempGS_C,tempGS_Cb)
model.sel(tempGS_C,tempGS_Cb,rank=AICc) #  SIG


tempGS_C=lme(GS_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="REML")
summary(tempGS_C) ##### 
plot(tempGS_C)
qqnorm(resid(tempGS_C))
plot(x=data.climC$year, y=resid(cmiGS_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(cmiGS_C), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
tempSP_C=lme(Spring_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
tempSP_Cb=lme(Spring_temp~1,random=~1|site,data=data.climC,
              na.action=na.exclude,method="ML")
anova(tempSP_C,tempSP_Cb)
model.sel(tempSP_C,tempSP_Cb,rank=AICc) #  SIG


tempSP_C=lme(Spring_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="REML")
summary(tempSP_C) ##### 
plot(tempSP_C)
qqnorm(resid(tempSP_C))
plot(x=data.climC$year, y=resid(tempSP_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(tempSP_C), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
tempFW_C=lme(FW_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="ML")
tempFW_Cb=lme(FW_temp~1,random=~1|site,data=data.climC,
              na.action=na.exclude,method="ML")
anova(tempFW_C,tempFW_Cb)
model.sel(tempFW_C,tempFW_Cb,rank=AICc) # NOT SIG


tempFW_C=lme(FW_temp~year,random=~1|site,data=data.climC,
             na.action=na.exclude,method="REML")
summary(tempFW_C) #####  
plot(tempFW_C)#
qqnorm(resid(tempFW_C))
plot(x=data.climC$year, y=resid(tempFW_C), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climC$site, y=resid(tempGS_C), type="pearson")
abline(h = 0, lty = 2) #
#############
########## TRENDS FROM 1997 - 2013
data.climD=subset(data.clim2,year>=1997 & year<=2013)
head(data.climD)
tail(data.climD)
###########
tempGS_D=lme(GS_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
tempGS_Db=lme(GS_temp~1,random=~1|site,data=data.climD,
              na.action=na.exclude,method="ML")
anova(tempGS_D,tempGS_Db)
model.sel(tempGS_D,tempGS_Db,rank=AICc) # NOT SIG

tempGS_D=lme(GS_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="REML")
summary(tempGS_D) ##### 
plot(tempGS_D)
qqnorm(resid(tempGS_D))
plot(x=data.climD$year, y=resid(tempGS_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(tempGS_D), type="pearson")
abline(h = 0, lty = 2) #
#####
#################
tempSP_D=lme(Spring_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
tempSP_Db=lme(Spring_temp~1,random=~1|site,data=data.climD,
              na.action=na.exclude,method="ML")
anova(tempSP_D,tempSP_Db)
model.sel(tempSP_D,tempSP_Db,rank=AICc) #SIG

tempSP_D=lme(Spring_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="REML")
summary(tempSP_D) ##### 
plot(tempSP_D)
qqnorm(resid(tempSP_D))
plot(x=data.climD$year, y=resid(tempSP_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(tempSP_D), type="pearson")
abline(h = 0, lty = 2) #
##################
###########
tempFW_D=lme(FW_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="ML")
tempFW_Db=lme(FW_temp~1,random=~1|site,data=data.climD,
              na.action=na.exclude,method="ML")
anova(tempFW_D,tempFW_Db)
model.sel(tempFW_D,tempFW_Db,rank=AICc) # NOT SIG

tempFW_D=lme(FW_temp~year,random=~1|site,data=data.climD,
             na.action=na.exclude,method="REML")
summary(tempFW_D) #####  
plot(tempFW_D)#
qqnorm(resid(tempFW_D))
plot(x=data.climD$year, y=resid(tempFW_D), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data.climD$site, y=resid(tempGS_D), type="pearson")
abline(h = 0, lty = 2) #

###########

#############


#### TABLE S15 IS IN R FILE: CAFI_clim*alm

#### TABLE S16 IS IN R FILE: CAFI_clim*alm

#### TABLE S17 IS IN R FILE: CAFI_clim*alm

#### TABLE S18 IS IN R FILE: CAFI_NDVI_BAI

#### TABLE S19 IS IN R FILE: CAFI_NDVI_BAI

#### TABLE S19 IS IN R FILE: CAFI_biomass_NDVI


########## FIGURE S1
######
############## supplementary figure with BIOMASS
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
#######
########## mortaliy is every 5 years (mg/ha/every 5 years)
caf_mortC2=read.csv("caf_mort_biomass.csv")
names(caf_mortC2)
head(caf_mortC2)
caf_mortC2=na.omit(caf_mortC2)
str(caf_mortC2)
#names(caf_mortC2)[names(caf_mortC2) == "ï..plot"] <- "plot"

caf_mortC2$plot=as.factor(caf_mortC2$plot)
plot(mort_corrected~year,data=caf_mortC2)
#########################
############################# merge with this df to get site into the df
dat.sp=read.csv("CAFI_sp.csv",header=T) 
names(dat.sp)
head(dat.sp)
str(dat.sp)
#names(dat.sp)[names(dat.sp) == "ï..year"] <- "year"


dat.sp$plot=as.factor(dat.sp$plot)
dat.sp$site=as.factor(dat.sp$site)
head(dat.sp)
str(dat.sp)
#########
######################## 
biomass_mort=merge(dat.sp,caf_mortC2,by=c("plot","year"),all = TRUE)
names(biomass_mort)
head(biomass_mort)
biomass_mort3=subset(biomass_mort,year>=1997 & year<=2013)
#####

plot_mort=ggplot(data = biomass_mort3)+geom_point(aes(y=mort_corrected,x=year,fill=plot),shape=21,size=3)+
  facet_wrap(~site,scales="free",ncol=2)+
  theme_bw()+
  theme(text = element_text(size=12,family="Helvetica",color="black"),
        axis.text=element_text(size=10,family="Helvetica",color="black"),
        axis.text.x = element_text(size=10,angle = 30),
        axis.text.y = element_text(size=10))+ labs(x="Year", y=expression(Aspen~biomass~mortality~(Mg~ha^-1)))+
  xlim(1997,2013)+labs(fill="Plot")

plot_mort

ggsave(plot = plot_mort, filename = "CAF_DENS.pdf", path = "~/Documents/CAFI_all_final",
       dpi = 600, width = 7, height = 8)
#######
################################### ------- FIGURE S2 ----------- all trees that died and year
#####
cafE_freq=read.csv("CAF_freq2.csv")
head(cafE_freq)
tail(cafE_freq)
#names(cafE_freq)[names(cafE_freq) == "ï..yr_death"] <- "yr_death"

p2=ggplot(data=cafE_freq,aes(x=lst_yr_growth,fill=as.factor(plot)))+geom_histogram(binwidth = 0.5)+
  theme_bw()+facet_wrap(~as.factor(site),ncol = 2)+
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015))+
  scale_y_continuous(breaks = c(0,1,2))+
  theme(text = element_text(size=12,family="Helvetica",color="black"),
        axis.text=element_text(size=10,family="Helvetica",color="black"),
        axis.text.x = element_text(angle = 30, hjust = 1))+
  labs(x="Last year of growth",y="Count",fill = "Plot")
p2

ggsave(plot = p2, filename = "yr_death1.pdf", path = "~/Documents/CAFI_all_final",
       dpi = 600, width = 6, height = 4)

#####
############
####################
######################## ------- FIGURE S3 ----------- SAMPLE DEPTH 
data1=read.csv("CAFI_BAI.csv")
names(data1)
#names(data1)[names(data1) == "ï..year"] <- "year"

data2=data1%>% 
  rowwise()%>%
  mutate(log.bai = log1p(bai))  

names(data2)
head(data2)
tail(data2)
#########
############remove trees that died before 1997
data2B=subset(data2, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
                tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
                tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
                tree!="TM6.61"& tree!="TM8.94")
names(data2B)

data2B[] <- lapply(data2B, function(x) if(is.factor(x)) factor(x) else x)
data2B=as.data.frame(data2B)
data2B$site=as.factor(data2B$site)
data2B$plot=as.factor(data2B$plot)
str(data2B)
#############
head(data2B)
##
#####
########## plot level
dat.site <-  
  ddply(data2B, c("year","status","plot","site"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))

head(dat.site)
ggplot(data=dat.site)+geom_line(aes(x=year,y=meanB,lty=status)) +facet_wrap(~site,scales="free_y")
###########
############## NUMBER OF TREES THAT DIED
dat.freq=read.csv("CAFI_freq_death.csv") 
str(dat.freq) #### no.dead represents the last year of growth for trees. i.e. no_dead = 10 in 2000 means that the last year of growth for 10 trees was in 2000
#### which is what i need for this figure b/c it is sample depth (the no. of trees used for analyses) ## we cant use year of death if not a full growth ring 
dat.freq$site= factor(dat.freq$site, 
                      levels=c("61-63","91-93","94-96","97-99","199-201","233","298-300","307-309"),
                      labels=c("1021","1031","1032","1033","1067","1079","1101","1104"))
#names(dat.freq)[names(dat.freq) == "ï..year"] <- "year"

dat.site2=merge(dat.site,dat.freq,by=c("year","site"),all=TRUE)
head(dat.site2)

head(dat.site2)
dat.site3=subset(dat.site2,year>1990 & year<=2013)
str(dat.site3)
dat.site3$status=as.factor(dat.site3$status)

dat.site3$status <- relevel(dat.site3$status, ref="living")
plot1=ggplot(data=dat.site3)+geom_line(aes(x=year,y=N,lty=status))+
  facet_wrap(~plot,scales="free_y")+
  theme_bw()+
  theme(text=element_text(family="Helvetica"))+
  theme(axis.text.y=element_text(size=7,color="black",family="Helvetica"),axis.title = 
          element_text(size=7),legend.position = "none",
        axis.text.x=element_text(size=7,color="black",family="Helvetica",angle=40))+labs(x="",y="")+
  theme(strip.text.x = element_text(size = 8))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1.0))
plot1

ggsave(plot = plot1, filename = "SD_plot.pdf", path = "~/Documents/CAFI_all_final",
       dpi = 600, width = 6, height = 4)
###
######site level
##########
dat.site2b <-  
  ddply(data2B, c("year","status","site"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))

head(dat.site2b)
dat.site3b=merge(dat.site2b,dat.freq,by=c("year","site"),all=TRUE)
head(dat.site3b)

head(dat.site3b)
dat.site3c=subset(dat.site3b,year>1990 & year<=2013)
dat.site3c$status=as.factor(dat.site3c$status)

dat.site3c$status <- relevel(dat.site3c$status, ref="living")
plot1c=ggplot(data=dat.site3c)+geom_line(aes(x=year,y=N,lty=status))+
  facet_wrap(~site,scales="free_y")+
  theme_bw()+
  theme(text=element_text(family="Helvetica"))+
  theme(axis.text.y=element_text(size=10,color="black",family="Helvetica"),axis.title = 
          element_text(size=10),legend.position = "none",
        axis.text.x=element_text(size=10,color="black",family="Helvetica",angle=30))+labs(x="",y="")+
  theme(strip.text.x = element_text(size = 10))
plot1c

ggsave(plot = plot1c, filename = "SD_site.pdf", path = "~/Documents/CAFI_all_final",
       dpi = 600, width = 6, height = 4)

#########
################## FIGURE S4 ----------GS TEMPERATURE BEFORE ALM OUTBREAK
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
####-----TEMP
par(mar = c(5,5.9,2.5,6))

plot(dat.climAVG_b4$meanT~dat.climAVG_b4$year,type="l",ylab="",lwd=2,
     xlab="",xlim=c(1945,1996),ylim=c(9,13),axes=F)

arrows(dat.climAVG_b4$year, 
       dat.climAVG_b4$meanT-dat.climAVG_b4$seT, 
       dat.climAVG_b4$year, 
       dat.climAVG_b4$meanT+dat.climAVG_b4$seT, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
par(new=T)
plot(dat.climAVG_b4$meanT~dat.climAVG_b4$year,type="l",ylab="",lwd=2,
     xlab="",xlim=c(1945,1996),ylim=c(9,13),axes=F)
axis(side = 2, at=seq(9,14,1),lwd = 1,cex.axis=0.85,las=2)

######
axis(side = 1, at=seq(1945,1996,5),lwd = 1,cex.axis=0.85)
box()
ylab.text = expression("Temperature ("*~degree*C*")")
mtext(ylab.text,side=2, line =2.5,cex=0.85)
mtext(side=1,"Year",line=2,cex=0.85)
##########
############### FIGURE W/NDVI, GS temp, AND ALM 
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
        N    = length(GS_temp),
        meanC = mean(GS_temp),
        sdC   = sd(GS_temp),
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
#names(dataI2)[names(dataI2) == "ï..year"] <- "year"

dataI_2=subset(dataI2,year>1996 & year<2014)
dat.climAVG_2=subset(dat.climAVG,year>1996 & year<2014)
dat.ndviAVG_2=subset(dat.ndviAVG,year>1996 & year<2014)
######

### plot alm
max(dataI_2$leafminer.ha.)
min(dataI_2$leafminer.ha.)

par(mar = c(5,5.9,2.5,6))
### plot CLIMATE
max(dat.climAVG_2$meanC)#12.3
min(dat.climAVG_2$meanC)#9
par(mar = c(5,5.9,2.5,6))
plot(dat.climAVG_2$meanC~dat.climAVG_2$year,type="l",ylab="",lwd=2.5,
     xlab="",xlim=c(1997,2013),ylim=c(5,19),axes=F,col="black")

arrows(dat.climAVG_2$year, 
       dat.climAVG_2$meanC-dat.climAVG_2$seC, 
       dat.climAVG_2$year, 
       dat.climAVG_2$meanC+dat.climAVG_2$seC, 
       length=0.03, 
       angle=90, 
       code=3,col="gray49")
par(new=T)
plot(dat.climAVG_2$meanC~dat.climAVG_2$year,type="l",ylab="",lwd=2.5,
     xlab="",xlim=c(1997,2013),ylim=c(5,19),axes=F,col="black")
axis(side = 2, at=seq(9,13,1),lwd = 1,cex.axis=0.85,las=2)

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
ylab.text = expression("Temperature ("*~degree*C*")")
mtext(ylab.text,side=2, line =2.2,cex=0.85,adj=0.4)
mtext(side=1,"Year",line=2,cex=0.85)
mtext(side=4,"Leaf mining (ha)",line=3.5,cex=0.85,adj=0.1)
mtext(side=4,"NDVI",line=3.3,cex=0.85,adj=0.8)

##########
box() ### dims = 600x500


##########################################