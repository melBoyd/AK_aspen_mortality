######
################### ANALYSIS: differences in annual productivity 1946 - 1996
#####
rm(list = ls())

library(dplyr)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(lme4)
library(nlme)
library(lmerTest)
library(data.table)

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

#####data frame with time periods 
head(data3)
str(data3)
data3[] <- lapply(data3, function(x) if(is.factor(x)) factor(x) else x)
data3=as.data.frame(data3)
data3$site=as.factor(data3$site)
data3$plot=as.factor(data3$plot)
data3$tree=as.factor(data3$tree)
data3$status=as.factor(data3$status)

data3$status = relevel(data3$status, ref="living")
######
######## mean productivigy by year
##########
dat.mean <-  
  ddply(data3, c("year","status"), summarise,
        N    = length(bai),
        meanB = mean(bai),
        sdB   = sd(bai),
        seB = sdB / sqrt(N))

str(data3)
############## subset by year 
dat46=subset(data3,year==1946) 
dat46[] <- lapply(dat46, function(x) if(is.factor(x)) factor(x) else x)
dat47=subset(data3,year==1947) 
dat47[] <- lapply(dat47, function(x) if(is.factor(x)) factor(x) else x)
dat48=subset(data3,year==1948) 
dat48[] <- lapply(dat48, function(x) if(is.factor(x)) factor(x) else x)
dat49=subset(data3,year==1949)
dat49[] <- lapply(dat49, function(x) if(is.factor(x)) factor(x) else x)
dat50=subset(data3,year==1950) 
dat50[] <- lapply(dat50, function(x) if(is.factor(x)) factor(x) else x)
dat51=subset(data3,year==1951) 
dat51[] <- lapply(dat51, function(x) if(is.factor(x)) factor(x) else x)
dat52=subset(data3,year==1952)
dat52[] <- lapply(dat52, function(x) if(is.factor(x)) factor(x) else x)
dat53=subset(data3,year==1953) 
dat53[] <- lapply(dat53, function(x) if(is.factor(x)) factor(x) else x)
dat54=subset(data3,year==1954) 
dat54[] <- lapply(dat54, function(x) if(is.factor(x)) factor(x) else x)
dat55=subset(data3,year==1955) 
dat55[] <- lapply(dat55, function(x) if(is.factor(x)) factor(x) else x)
dat56=subset(data3,year==1956) 
dat56[] <- lapply(dat56, function(x) if(is.factor(x)) factor(x) else x)
dat57=subset(data3,year==1957) #
dat57[] <- lapply(dat57, function(x) if(is.factor(x)) factor(x) else x)
dat58=subset(data3,year==1958) 
dat58[] <- lapply(dat58, function(x) if(is.factor(x)) factor(x) else x)
dat59=subset(data3,year==1959) 
dat59[] <- lapply(dat59, function(x) if(is.factor(x)) factor(x) else x)
dat60=subset(data3,year==1960) #
dat60[] <- lapply(dat60, function(x) if(is.factor(x)) factor(x) else x)
dat61=subset(data3,year==1961) 
dat61[] <- lapply(dat61, function(x) if(is.factor(x)) factor(x) else x)
dat62=subset(data3,year==1962) ###
dat62[] <- lapply(dat62, function(x) if(is.factor(x)) factor(x) else x)
dat63=subset(data3,year==1963) #
dat63[] <- lapply(dat63, function(x) if(is.factor(x)) factor(x) else x)
dat64=subset(data3,year==1964) 
dat64[] <- lapply(dat64, function(x) if(is.factor(x)) factor(x) else x)
dat65=subset(data3,year==1965) 
dat65[] <- lapply(dat65, function(x) if(is.factor(x)) factor(x) else x)
dat66=subset(data3,year==1966) 
dat66[] <- lapply(dat66, function(x) if(is.factor(x)) factor(x) else x)
dat67=subset(data3,year==1967) 
dat67[] <- lapply(dat67, function(x) if(is.factor(x)) factor(x) else x)
dat68=subset(data3,year==1968) 
dat68[] <- lapply(dat68, function(x) if(is.factor(x)) factor(x) else x)
dat69=subset(data3,year==1969) 
dat69[] <- lapply(dat69, function(x) if(is.factor(x)) factor(x) else x)
dat70=subset(data3,year==1970) 
dat70[] <- lapply(dat70, function(x) if(is.factor(x)) factor(x) else x)
dat71=subset(data3,year==1971) 
dat71[] <- lapply(dat71, function(x) if(is.factor(x)) factor(x) else x)
dat72=subset(data3,year==1972) 
dat72[] <- lapply(dat72, function(x) if(is.factor(x)) factor(x) else x)
dat73=subset(data3,year==1973) 
dat73[] <- lapply(dat73, function(x) if(is.factor(x)) factor(x) else x)
dat74=subset(data3,year==1974) 
dat74[] <- lapply(dat74, function(x) if(is.factor(x)) factor(x) else x)
dat75=subset(data3,year==1975) 
dat75[] <- lapply(dat75, function(x) if(is.factor(x)) factor(x) else x)
dat76=subset(data3,year==1976) 
dat76[] <- lapply(dat76, function(x) if(is.factor(x)) factor(x) else x)
dat77=subset(data3,year==1977) 
dat77[] <- lapply(dat77, function(x) if(is.factor(x)) factor(x) else x)
dat78=subset(data3,year==1978) 
dat78[] <- lapply(dat78, function(x) if(is.factor(x)) factor(x) else x)
dat79=subset(data3,year==1979) 
dat79[] <- lapply(dat79, function(x) if(is.factor(x)) factor(x) else x)
dat80=subset(data3,year==1980) #
dat80[] <- lapply(dat80, function(x) if(is.factor(x)) factor(x) else x)
dat81=subset(data3,year==1981) #
dat81[] <- lapply(dat81, function(x) if(is.factor(x)) factor(x) else x)
dat82=subset(data3,year==1982) #
dat82[] <- lapply(dat82, function(x) if(is.factor(x)) factor(x) else x)
dat83=subset(data3,year==1983) ##
dat83[] <- lapply(dat83, function(x) if(is.factor(x)) factor(x) else x)
dat84=subset(data3,year==1984) 
dat84[] <- lapply(dat84, function(x) if(is.factor(x)) factor(x) else x)
dat85=subset(data3,year==1985) 
dat85[] <- lapply(dat85, function(x) if(is.factor(x)) factor(x) else x)
dat86=subset(data3,year==1986) 
dat86[] <- lapply(dat86, function(x) if(is.factor(x)) factor(x) else x)
dat87=subset(data3,year==1987) 
dat87[] <- lapply(dat87, function(x) if(is.factor(x)) factor(x) else x)
dat88=subset(data3,year==1988)
dat88[] <- lapply(dat88, function(x) if(is.factor(x)) factor(x) else x)
dat89=subset(data3,year==1989) 
dat89[] <- lapply(dat89, function(x) if(is.factor(x)) factor(x) else x)
dat90=subset(data3,year==1990) 
dat90[] <- lapply(dat90, function(x) if(is.factor(x)) factor(x) else x)
dat91=subset(data3,year==1991)
dat91[] <- lapply(dat91, function(x) if(is.factor(x)) factor(x) else x)
dat92=subset(data3,year==1992) 
dat92[] <- lapply(dat92, function(x) if(is.factor(x)) factor(x) else x)
dat93=subset(data3,year==1993) 
dat93[] <- lapply(dat93, function(x) if(is.factor(x)) factor(x) else x)
dat94=subset(data3,year==1994) 
dat94[] <- lapply(dat94, function(x) if(is.factor(x)) factor(x) else x)
dat95=subset(data3,year==1995) 
dat95[] <- lapply(dat95, function(x) if(is.factor(x)) factor(x) else x)
dat96=subset(data3,year==1996) 
dat96[] <- lapply(dat96, function(x) if(is.factor(x)) factor(x) else x)

#########ONLY NEED SITE/plot AS A RANDOM EFFECT ### only one msrmt per tree 
####
####### table s10 and figure 1 
mod46=lme(log.bai~status,random = ~1|site/plot,data=dat46,
          na.action=na.exclude,method = "ML")# 
mod46b=lme(log.bai~1,random = ~1|site/plot,data=dat46,
          na.action=na.exclude,method = "ML")#
model.sel(mod46,mod46b,rank=AICc) # 
anova(mod46,mod46b)

mod46=lme(log.bai~status,random = ~1|site/plot,data=dat46,
          na.action=na.exclude,method = "REML")#same######  i need to start here this is when residuals look ok
summary(mod46)
plot(mod46) 
qqnorm(resid(mod46))
plot(x=dat46$site, y=resid(mod46), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat46$status, y=resid(mod46), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat46$plot, y=resid(mod46), type="pearson")
abline(h = 0, lty = 2) #
######
mod47=lme(log.bai~status,random = ~1|site/plot,data=dat47,
          na.action=na.exclude,method = "ML")# 
mod47b=lme(log.bai~1,random = ~1|site/plot,data=dat47,
           na.action=na.exclude,method = "ML")#
model.sel(mod47,mod47b,rank=AICc) #
anova(mod47,mod47b)

mod47=lme(log.bai~status,random = ~1|site/plot,data=dat47,
          na.action=na.exclude,method = "REML") # 
summary(mod47)
plot(mod47)
qqnorm(resid(mod47))
plot(x=dat47$site, y=resid(mod47), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat47$status, y=resid(mod47), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat47$plot, y=resid(mod47), type="pearson")
abline(h = 0, lty = 2) #
#######
mod48=lme(log.bai~status,random = ~1|site/plot,data=dat48,
          na.action=na.exclude,method = "ML")# 
mod48b=lme(log.bai~1,random = ~1|site/plot,data=dat48,
           na.action=na.exclude,method = "ML")#
model.sel(mod48,mod48b,rank=AICc) # 
anova(mod48,mod48b)

mod48=lme(log.bai~status,random = ~1|site/plot,data=dat48,
          na.action=na.exclude,method = "REML") # 
summary(mod48)
plot(mod48)
qqnorm(resid(mod48))
plot(x=dat48$site, y=resid(mod48), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat48$status, y=resid(mod48), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat48$plot, y=resid(mod48), type="pearson")
abline(h = 0, lty = 2) #
#########
##########
mod49=lme(log.bai~status,random = ~1|site/plot,data=dat49,
          na.action=na.exclude,method = "ML")# 
mod49b=lme(log.bai~1,random = ~1|site/plot,data=dat49,
           na.action=na.exclude,method = "ML")#
model.sel(mod49,mod49b,rank=AICc) # 
anova(mod49,mod49b)

mod49=lme(log.bai~status,random = ~1|site/plot,data=dat49,
          na.action=na.exclude,method = "REML") # 
summary(mod49)
qqnorm(resid(mod49))
plot(x=dat49$site, y=resid(mod49), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat49$status, y=resid(mod49), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat49$plot, y=resid(mod49), type="pearson")
abline(h = 0, lty = 2) #
######
mod50=lme(log.bai~status,random = ~1|site/plot,data=dat50,
          na.action=na.exclude,method = "ML")# 
mod50b=lme(log.bai~1,random = ~1|site/plot,data=dat50,
           na.action=na.exclude,method = "ML")#
model.sel(mod50,mod50b,rank=AICc) # 
anova(mod50,mod50b)

mod50=lme(log.bai~status,random = ~1|site/plot,data=dat50,
          na.action=na.exclude,method = "REML") # 
summary(mod50)
qqnorm(resid(mod50))
plot(x=dat50$site, y=resid(mod50), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat50$status, y=resid(mod50), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat50$plot, y=resid(mod50), type="pearson")
abline(h = 0, lty = 2) #
######
mod51=lme(log.bai~status,random = ~1|site/plot,data=dat51,
          na.action=na.exclude,method = "ML")# 
mod51b=lme(log.bai~1,random = ~1|site/plot,data=dat51,
           na.action=na.exclude,method = "ML")#
model.sel(mod51,mod51b,rank=AICc) # 
anova(mod51,mod51b)

mod51=lme(log.bai~status,random = ~1|site/plot,data=dat51,
          na.action=na.exclude,method = "REML") # 
summary(mod51)
qqnorm(resid(mod51))
plot(x=dat51$site, y=resid(mod51), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat51$status, y=resid(mod51), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat51$plot, y=resid(mod51), type="pearson")
abline(h = 0, lty = 2) #

######
######
mod52=lme(log.bai~status,random = ~1|site/plot,data=dat52,
          na.action=na.exclude,method = "ML")# 
mod52b=lme(log.bai~1,random = ~1|site/plot,data=dat52,
           na.action=na.exclude,method = "ML")#
model.sel(mod52,mod52b,rank=AICc) # 
anova(mod52,mod52b)

mod52=lme(log.bai~status,random = ~1|site/plot,data=dat52,
          na.action=na.exclude,method = "REML") # 
summary(mod52)
plot(mod52)
qqnorm(resid(mod52))
plot(x=dat52$site, y=resid(mod52), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat52$status, y=resid(mod52), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat52$plot, y=resid(mod52), type="pearson")
abline(h = 0, lty = 2) #
######
mod53=lme(log.bai~status,random = ~1|site/plot,data=dat53,
          na.action=na.exclude,method = "ML")# 
mod53b=lme(log.bai~1,random = ~1|site/plot,data=dat53,
           na.action=na.exclude,method = "ML")#
model.sel(mod53,mod53b,rank=AICc) #
anova(mod53,mod53b)

mod53=lme(log.bai~status,random = ~1|site/plot,data=dat53,
          na.action=na.exclude,method = "REML") 
summary(mod53)
plot(mod53)
qqnorm(resid(mod53))
plot(x=dat53$site, y=resid(mod53), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat53$status, y=resid(mod53), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat53$plot, y=resid(mod53), type="pearson")
abline(h = 0, lty = 2) #
######
mod54=lme(log.bai~status,random = ~1|site/plot,data=dat54,
          na.action=na.exclude,method = "ML")# 
mod54b=lme(log.bai~1,random = ~1|site/plot,data=dat54,
           na.action=na.exclude,method = "ML")#
model.sel(mod54,mod54b,rank=AICc) 
anova(mod54,mod54b)
#####
mod54=lme(log.bai~status,random = ~1|site/plot,data=dat54,
          na.action=na.exclude,method = "REML") 
summary(mod54) ##
plot(mod54)
qqnorm(resid(mod54))
plot(x=dat54$site, y=resid(mod54), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat54$status, y=resid(mod54), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat54$plot, y=resid(mod54), type="pearson")
abline(h = 0, lty = 2) #
######
mod55=lme(log.bai~status,random = ~1|site/plot,data=dat55,
          na.action=na.exclude,method = "ML")# 
mod55b=lme(log.bai~1,random = ~1|site/plot,data=dat55,
           na.action=na.exclude,method = "ML")#
model.sel(mod55,mod55b,rank=AICc)
anova(mod55,mod55b)

mod55=lme(log.bai~status,random = ~1|site/plot,data=dat55,
          na.action=na.exclude,method = "REML") 
summary(mod55)
plot(mod55)
qqnorm(resid(mod55))
plot(x=dat55$site, y=resid(mod55), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat55$status, y=resid(mod55), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat55$plot, y=resid(mod55), type="pearson")
abline(h = 0, lty = 2) #
######
mod56=lme(log.bai~status,random = ~1|site/plot,data=dat56,
          na.action=na.exclude,method = "ML")# 
mod56b=lme(log.bai~1,random = ~1|site/plot,data=dat56,
           na.action=na.exclude,method = "ML")#
model.sel(mod56,mod56b,rank=AICc)
anova(mod56,mod56b)

mod56=lme(log.bai~status,random = ~1|site/plot,data=dat56,
          na.action=na.exclude,method = "REML") 
summary(mod56)
plot(mod56)
qqnorm(resid(mod56))
plot(x=dat56$site, y=resid(mod56), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat56$status, y=resid(mod56), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat56$plot, y=resid(mod56), type="pearson")
abline(h = 0, lty = 2) #
######
mod57=lme(log.bai~status,random = ~1|site/plot,data=dat57,
          na.action=na.exclude,method = "ML")# 
mod57b=lme(log.bai~1,random = ~1|site/plot,data=dat57,
           na.action=na.exclude,method = "ML")#
model.sel(mod57,mod57b,rank=AICc) 
anova(mod57,mod57b)

mod57=lme(log.bai~status,random = ~1|site/plot,data=dat57,
          na.action=na.exclude,method = "REML") 
summary(mod57)
plot(mod57)
qqnorm(resid(mod57))
plot(x=dat57$site, y=resid(mod57), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat57$status, y=resid(mod57), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat57$plot, y=resid(mod57), type="pearson")
abline(h = 0, lty = 2) #
############
mod58=lme(log.bai~status,random = ~1|site/plot,data=dat58,
          na.action=na.exclude,method = "ML")# 
mod58b=lme(log.bai~1,random = ~1|site/plot,data=dat58,
           na.action=na.exclude,method = "ML")#
model.sel(mod58,mod58b,rank=AICc) #  
anova(mod58,mod58b)

mod58=lme(log.bai~status,random = ~1|site/plot,data=dat58,
          na.action=na.exclude,method = "REML") #
summary(mod58)
plot(mod58)
qqnorm(resid(mod58))
plot(x=dat58$site, y=resid(mod58), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat58$status, y=resid(mod58), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat58$plot, y=resid(mod58), type="pearson")
abline(h = 0, lty = 2) #
#####
mod59=lme(log.bai~status,random = ~1|site/plot,data=dat59,
          na.action=na.exclude,method = "ML")# 
mod59b=lme(log.bai~1,random = ~1|site/plot,data=dat59,
           na.action=na.exclude,method = "ML")#
model.sel(mod59,mod59b,rank=AICc) # 
anova(mod59,mod59b)

mod59=lme(log.bai~status,random = ~1|site/plot,data=dat59,
          na.action=na.exclude,method = "REML") 
summary(mod59)
plot(mod59)
qqnorm(resid(mod59))
plot(x=dat59$site, y=resid(mod59), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat59$status, y=resid(mod59), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat59$plot, y=resid(mod59), type="pearson")
abline(h = 0, lty = 2) #
#####
mod60=lme(log.bai~status,random = ~1|site/plot,data=dat60,
          na.action=na.exclude,method = "ML")# 
mod60b=lme(log.bai~1,random = ~1|site/plot,data=dat60,
           na.action=na.exclude,method = "ML")#
model.sel(mod60,mod60b,rank=AICc) #  
anova(mod60,mod60b)

mod60=lme(log.bai~status,random = ~1|site/plot,data=dat60,
          na.action=na.exclude,method = "REML") #
summary(mod60)
plot(mod60)
qqnorm(resid(mod60))
plot(x=dat60$site, y=resid(mod60), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat60$status, y=resid(mod60), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat60$plot, y=resid(mod60), type="pearson")
abline(h = 0, lty = 2) #
#####
mod61=lme(log.bai~status,random = ~1|site/plot,data=dat61,
          na.action=na.exclude,method = "ML")# 
mod61b=lme(log.bai~1,random = ~1|site/plot,data=dat61,
           na.action=na.exclude,method = "ML")#
model.sel(mod61,mod61b,rank=AICc) #  
anova(mod61,mod61b)

mod61=lme(log.bai~status,random = ~1|site/plot,data=dat61,
          na.action=na.exclude,method = "REML") 
summary(mod61)
plot(mod61)
qqnorm(resid(mod61)) ## outlier = 60
plot(x=dat61$site, y=resid(mod61), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat61$status, y=resid(mod61), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat61$plot, y=resid(mod61), type="pearson")
abline(h = 0, lty = 2) # 
##### should make w/out outlier - check result
dat61b=dat61[-c(60),]
mod61b=lme(log.bai~status,random = ~1|site/plot,data=dat61b,
          na.action=na.exclude,method = "REML") 
summary(mod61b)##### same result
#####
mod62=lme(log.bai~status,random = ~1|site/plot,data=dat62,
          na.action=na.exclude,method = "ML")# 
mod62b=lme(log.bai~1,random = ~1|site/plot,data=dat62,
           na.action=na.exclude,method = "ML")#
model.sel(mod62,mod62b,rank=AICc) #  
anova(mod62,mod62b)

mod62=lme(log.bai~status,random = ~1|site/plot,data=dat62,
          na.action=na.exclude,method = "REML") 
summary(mod62) 
plot(mod62)
qqnorm(resid(mod62)) ## 
plot(x=dat62$site, y=resid(mod62), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat62$status, y=resid(mod62), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat62$plot, y=resid(mod62), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod63=lme(log.bai~status,random = ~1|site/plot,data=dat63,
          na.action=na.exclude,method = "ML")# 
mod63b=lme(log.bai~1,random = ~1|site/plot,data=dat63,
           na.action=na.exclude,method = "ML")#
model.sel(mod63,mod63b,rank=AICc) # 
anova(mod63,mod63b)

mod63=lme(log.bai~status,random = ~1|site/plot,data=dat63,
          na.action=na.exclude,method = "REML") #
summary(mod63)#
plot(mod63)
qqnorm(resid(mod63)) #
plot(x=dat63$site, y=resid(mod63), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat63$status, y=resid(mod63), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat63$plot, y=resid(mod63), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod64=lme(log.bai~status,random = ~1|site/plot,data=dat64,
          na.action=na.exclude,method = "ML")# 
mod64b=lme(log.bai~1,random = ~1|site/plot,data=dat64,
           na.action=na.exclude,method = "ML")#
model.sel(mod64,mod64b,rank=AICc) #  
anova(mod64,mod64b)

mod64=lme(log.bai~status,random = ~1|site/plot,data=dat64,
          na.action=na.exclude,method = "REML") 
summary(mod64)
plot(mod64)
qqnorm(resid(mod64)) 
plot(x=dat64$site, y=resid(mod64), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat64$status, y=resid(mod64), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat64$plot, y=resid(mod64), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod65=lme(log.bai~status,random = ~1|site/plot,data=dat65,
          na.action=na.exclude,method = "ML")# 
mod65b=lme(log.bai~1,random = ~1|site/plot,data=dat65,
           na.action=na.exclude,method = "ML")#
model.sel(mod65,mod65b,rank=AICc) 
anova(mod65,mod65b)

mod65=lme(log.bai~status,random = ~1|site/plot,data=dat65,
          na.action=na.exclude,method = "REML") 
summary(mod65) 
plot(mod65)
qqnorm(resid(mod65)) 
plot(x=dat65$site, y=resid(mod65), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat65$status, y=resid(mod65), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat65$plot, y=resid(mod65), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod66=lme(log.bai~status,random = ~1|site/plot,data=dat66,
          na.action=na.exclude,method = "ML")# 
mod66b=lme(log.bai~1,random = ~1|site/plot,data=dat66,
           na.action=na.exclude,method = "ML")#
model.sel(mod66,mod66b,rank=AICc)
anova(mod66,mod66b)

mod66=lme(log.bai~status,random = ~1|site/plot,data=dat66,
          na.action=na.exclude,method = "REML") 
summary(mod66)
plot(mod66)
qqnorm(resid(mod66)) 
plot(x=dat66$site, y=resid(mod66), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat66$status, y=resid(mod66), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat66$plot, y=resid(mod66), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod67=lme(log.bai~status,random = ~1|site/plot,data=dat67,
          na.action=na.exclude,method = "ML")# 
mod67b=lme(log.bai~1,random = ~1|site/plot,data=dat67,
           na.action=na.exclude,method = "ML")#
model.sel(mod67,mod67b,rank=AICc) 
anova(mod67,mod67b)

mod67=lme(log.bai~status,random = ~1|site/plot,data=dat67,
          na.action=na.exclude,method = "REML") 
summary(mod67)
plot(mod67)
qqnorm(resid(mod67)) ## outlier = 2,15
plot(x=dat67$site, y=resid(mod67), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat67$status, y=resid(mod67), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat67$plot, y=resid(mod67), type="pearson")
abline(h = 0, lty = 2) # 

mod67c=lme(log.bai~status,random = ~1|site/plot,data=dat67[-c(2,15),],
          na.action=na.exclude,method = "REML") 
summary(mod67c) ### same result 
#####
mod68=lme(log.bai~status,random = ~1|site/plot,data=dat68,
          na.action=na.exclude,method = "ML")# 
mod68b=lme(log.bai~1,random = ~1|site/plot,data=dat68,
           na.action=na.exclude,method = "ML")#
model.sel(mod68,mod68b,rank=AICc) # 
anova(mod68,mod68b)

mod68=lme(log.bai~status,random = ~1|site/plot,data=dat68,
          na.action=na.exclude,method = "REML")
summary(mod68) 
plot(mod68)
qqnorm(resid(mod68)) ## 
plot(x=dat68$site, y=resid(mod68), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat68$status, y=resid(mod68), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat68$plot, y=resid(mod68), type="pearson")
abline(h = 0, lty = 2) # 

dat68c=subset(dat68,log.bai>0)
mod68c=lme(log.bai~status,random = ~1|site/plot,data=dat68c,
           na.action=na.exclude,method = "REML") #
summary(mod68c)  ###diff
plot(mod68c) ### looks a bit better but results the same regardless, so keep 0s 
#####
mod69=lme(log.bai~status,random = ~1|site/plot,data=dat69,
          na.action=na.exclude,method = "ML")# 
mod69b=lme(log.bai~1,random = ~1|site/plot,data=dat69,
           na.action=na.exclude,method = "ML")#
model.sel(mod69,mod69b,rank=AICc) # 
anova(mod69,mod69b)

mod69=lme(log.bai~status,random = ~1|site/plot,data=dat69,
          na.action=na.exclude,method = "REML") #
summary(mod69) 
plot(mod69) 
qqnorm(resid(mod69)) 
plot(x=dat69$site, y=resid(mod69), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat69$status, y=resid(mod69), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat69$plot, y=resid(mod69), type="pearson")
abline(h = 0, lty = 2) # 

dat69c=subset(dat69,log.bai>0)
mod69c=lme(log.bai~status,random = ~1|site/plot,data=dat69c,
          na.action=na.exclude,method = "REML") #
summary(mod69c)  ###diff
plot(mod69c) ### looks a bit better but results the same regardless, so keep 0s 
#####
mod70=lme(log.bai~status,random = ~1|site/plot,data=dat70,
          na.action=na.exclude,method = "ML")# 
mod70b=lme(log.bai~1,random = ~1|site/plot,data=dat70,
           na.action=na.exclude,method = "ML")#
model.sel(mod70,mod70b,rank=AICc) # 
anova(mod70,mod70b)

mod70=lme(log.bai~status,random = ~1|site/plot,data=dat70,
          na.action=na.exclude,method = "REML") 
summary(mod70)
plot(mod70)
qqnorm(resid(mod70)) 
plot(x=dat70$site, y=resid(mod70), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat70$status, y=resid(mod70), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat70$plot, y=resid(mod70), type="pearson")
abline(h = 0, lty = 2) # 

#####
mod71=lme(log.bai~status,random = ~1|site/plot,data=dat71,
          na.action=na.exclude,method = "ML")# 
mod71b=lme(log.bai~1,random = ~1|site/plot,data=dat71,
           na.action=na.exclude,method = "ML")#
model.sel(mod71,mod71b,rank=AICc) 
anova(mod71,mod71b)

mod71=lme(log.bai~status,random = ~1|site/plot,data=dat71,
          na.action=na.exclude,method = "REML")
summary(mod71)
plot(mod71)
plot(x=dat71$site, y=resid(mod71), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat71$status, y=resid(mod71), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat71$plot, y=resid(mod71), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod72=lme(log.bai~status,random = ~1|site/plot,data=dat72,
          na.action=na.exclude,method = "ML")# 
mod72b=lme(log.bai~1,random = ~1|site/plot,data=dat72,
           na.action=na.exclude,method = "ML")#
model.sel(mod72,mod72b,rank=AICc) # 
anova(mod72,mod72b)

mod72=lme(log.bai~status,random = ~1|site/plot,data=dat72,
          na.action=na.exclude,method = "REML") 
summary(mod72)
plot(mod72)
qqnorm(resid(mod72))
plot(x=dat72$site, y=resid(mod72), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat72$status, y=resid(mod72), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat72$plot, y=resid(mod72), type="pearson")
abline(h = 0, lty = 2) # 
####
#####
mod73=lme(log.bai~status,random = ~1|site/plot,data=dat73,
          na.action=na.exclude,method = "ML")# 
mod73b=lme(log.bai~1,random = ~1|site/plot,data=dat73,
           na.action=na.exclude,method = "ML")#
model.sel(mod73,mod73b,rank=AICc) # 
anova(mod73,mod73b)

mod73=lme(log.bai~status,random = ~1|site/plot,data=dat73,
          na.action=na.exclude,method = "REML") 
summary(mod73)
plot(mod73)
qqnorm(resid(mod73))
plot(x=dat73$site, y=resid(mod73), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat73$status, y=resid(mod73), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat73$plot, y=resid(mod73), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod74=lme(log.bai~status,random = ~1|site/plot,data=dat74,
          na.action=na.exclude,method = "ML")# 
mod74b=lme(log.bai~1,random = ~1|site/plot,data=dat74,
           na.action=na.exclude,method = "ML")#
model.sel(mod74,mod74b,rank=AICc) #  
anova(mod74,mod74b)
####
mod74=lme(log.bai~status,random = ~1|site/plot,data=dat74,
          na.action=na.exclude,method = "REML")
summary(mod74)
plot(mod74)
qqnorm(resid(mod74))
plot(x=dat74$site, y=resid(mod74), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat74$status, y=resid(mod74), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat74$plot, y=resid(mod74), type="pearson")
abline(h = 0, lty = 2) # 

#####
mod75=lme(log.bai~status,random = ~1|site/plot,data=dat75,
          na.action=na.exclude,method = "ML")# 
mod75b=lme(log.bai~1,random = ~1|site/plot,data=dat75,
           na.action=na.exclude,method = "ML")#
model.sel(mod75,mod75b,rank=AICc) 
anova(mod75,mod75b)
####
mod75=lme(log.bai~status,random = ~1|site/plot,data=dat75,
          na.action=na.exclude,method = "REML") 
summary(mod75)
plot(mod75)
qqnorm(resid(mod75))
plot(x=dat75$site, y=resid(mod75), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat75$status, y=resid(mod75), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat75$plot, y=resid(mod75), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod76=lme(log.bai~status,random = ~1|site/plot,data=dat76,
          na.action=na.exclude,method = "ML")# 
mod76b=lme(log.bai~1,random = ~1|site/plot,data=dat76,
           na.action=na.exclude,method = "ML")#
model.sel(mod76,mod76b,rank=AICc) #  sig ####diff
anova(mod76,mod76b)
####
mod76=lme(log.bai~status,random = ~1|site/plot,data=dat76,
          na.action=na.exclude,method = "REML") 
summary(mod76)
plot(mod76)
qqnorm(resid(mod76))
plot(x=dat76$site, y=resid(mod76), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat76$status, y=resid(mod76), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat76$plot, y=resid(mod76), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod77=lme(log.bai~status,random = ~1|site/plot,data=dat77,
          na.action=na.exclude,method = "ML")# 
mod77b=lme(log.bai~1,random = ~1|site/plot,data=dat77,
           na.action=na.exclude,method = "ML")#
model.sel(mod77,mod77b,rank=AICc) # 
anova(mod77,mod77b)
####
mod77=lme(log.bai~status,random = ~1|site/plot,data=dat77,
          na.action=na.exclude,method = "REML") 
summary(mod77) #
plot(mod77)
qqnorm(resid(mod77))
plot(x=dat77$site, y=resid(mod77), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat77$status, y=resid(mod77), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat77$plot, y=resid(mod77), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod78=lme(log.bai~status,random = ~1|site/plot,data=dat78,
          na.action=na.exclude,method = "ML")# 
mod78b=lme(log.bai~1,random = ~1|site/plot,data=dat78,
           na.action=na.exclude,method = "ML")#
model.sel(mod78,mod78b,rank=AICc) # 
anova(mod78,mod78b)
####
mod78=lme(log.bai~status,random = ~1|site/plot,data=dat78,
          na.action=na.exclude,method = "REML") 
summary(mod78) ##
plot(mod78)
qqnorm(resid(mod78))
plot(x=dat78$site, y=resid(mod78), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat78$status, y=resid(mod78), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat78$plot, y=resid(mod78), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod79=lme(log.bai~status,random = ~1|site/plot,data=dat79,
          na.action=na.exclude,method = "ML")# 
mod79b=lme(log.bai~1,random = ~1|site/plot,data=dat79,
           na.action=na.exclude,method = "ML")#
model.sel(mod79,mod79b,rank=AICc) # 
anova(mod79,mod79b)
####
mod79=lme(log.bai~status,random = ~1|site/plot,data=dat79,
          na.action=na.exclude,method = "REML") 
summary(mod79)
plot(mod79)
qqnorm(resid(mod79))
plot(x=dat79$site, y=resid(mod79), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat79$status, y=resid(mod79), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat79$plot, y=resid(mod79), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod80=lme(log.bai~status,random = ~1|site/plot,data=dat80,
          na.action=na.exclude,method = "ML")# 
mod80b=lme(log.bai~1,random = ~1|site/plot,data=dat80,
           na.action=na.exclude,method = "ML")#
model.sel(mod80,mod80b,rank=AICc) #  
anova(mod80,mod80b)
####
mod80=lme(log.bai~status,random = ~1|site/plot,data=dat80,
          na.action=na.exclude,method = "REML")
summary(mod80)
plot(mod80)
qqnorm(resid(mod80))
plot(x=dat80$site, y=resid(mod80), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat80$status, y=resid(mod80), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat80$plot, y=resid(mod80), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod81=lme(log.bai~status,random = ~1|site/plot,data=dat81,
          na.action=na.exclude,method = "ML")# 
mod81b=lme(log.bai~1,random = ~1|site/plot,data=dat81,
           na.action=na.exclude,method = "ML")#
model.sel(mod81,mod81b,rank=AICc) 
anova(mod81,mod81b)
####
mod81=lme(log.bai~status,random = ~1|site/plot,data=dat81,
          na.action=na.exclude,method = "REML") 
summary(mod81)
plot(mod81)
qqnorm(resid(mod81))
plot(x=dat81$site, y=resid(mod81), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat81$status, y=resid(mod81), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat81$plot, y=resid(mod81), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod82=lme(log.bai~status,random = ~1|site/plot,data=dat82,
          na.action=na.exclude,method = "ML")# 
mod82b=lme(log.bai~1,random = ~1|site/plot,data=dat82,
           na.action=na.exclude,method = "ML")#
model.sel(mod82,mod82b,rank=AICc) 
anova(mod82,mod82b)
####
mod82=lme(log.bai~status,random = ~1|site/plot,data=dat82,
          na.action=na.exclude,method = "REML") 
summary(mod82)
plot(mod82)
qqnorm(resid(mod82))
plot(x=dat82$site, y=resid(mod82), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat82$status, y=resid(mod82), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat82$plot, y=resid(mod82), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod83=lme(log.bai~status,random = ~1|site/plot,data=dat83,
          na.action=na.exclude,method = "ML")# 
mod83b=lme(log.bai~1,random = ~1|site/plot,data=dat83,
           na.action=na.exclude,method = "ML")#
model.sel(mod83,mod83b,rank=AICc) #
anova(mod83,mod83b)
####
mod83=lme(log.bai~status,random = ~1|site/plot,data=dat83,
          na.action=na.exclude,method = "REML")
summary(mod83)
plot(mod83)
qqnorm(resid(mod83))
plot(x=dat83$site, y=resid(mod83), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat83$status, y=resid(mod83), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat83$plot, y=resid(mod83), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod84=lme(log.bai~status,random = ~1|site/plot,data=dat84,
          na.action=na.exclude,method = "ML")# 
mod84b=lme(log.bai~1,random = ~1|site/plot,data=dat84,
           na.action=na.exclude,method = "ML")#
model.sel(mod84,mod84b,rank=AICc) 
anova(mod84,mod84b)
####
mod84=lme(log.bai~status,random = ~1|site/plot,data=dat84,
          na.action=na.exclude,method = "REML") 
summary(mod84)
plot(mod84)
qqnorm(resid(mod84))
plot(x=dat84$site, y=resid(mod84), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat84$status, y=resid(mod84), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat84$plot, y=resid(mod84), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod85=lme(log.bai~status,random = ~1|site/plot,data=dat85,
          na.action=na.exclude,method = "ML")# 
mod85b=lme(log.bai~1,random = ~1|site/plot,data=dat85,
           na.action=na.exclude,method = "ML")#
model.sel(mod85,mod85b,rank=AICc) 
anova(mod85,mod85b)
####
mod85=lme(log.bai~status,random = ~1|site/plot,data=dat85,
          na.action=na.exclude,method = "REML") 
summary(mod85)
plot(mod85)
qqnorm(resid(mod85))
plot(x=dat85$site, y=resid(mod85), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat85$status, y=resid(mod85), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat85$plot, y=resid(mod85), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod86=lme(log.bai~status,random = ~1|site/plot,data=dat86,
          na.action=na.exclude,method = "ML")# 
mod86b=lme(log.bai~1,random = ~1|site/plot,data=dat86,
           na.action=na.exclude,method = "ML")#
model.sel(mod86,mod86b,rank=AICc) 
anova(mod86,mod86b)
####
mod86=lme(log.bai~status,random = ~1|site/plot,data=dat86,
          na.action=na.exclude,method = "REML")
summary(mod86) 
plot(mod86)
qqnorm(resid(mod86))
plot(x=dat86$site, y=resid(mod86), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat86$status, y=resid(mod86), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat86$plot, y=resid(mod86), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod87=lme(log.bai~status,random = ~1|site/plot,data=dat87,
          na.action=na.exclude,method = "ML")# 
mod87b=lme(log.bai~1,random = ~1|site/plot,data=dat87,
           na.action=na.exclude,method = "ML")#
model.sel(mod87,mod87b,rank=AICc) 
anova(mod87,mod87b)
####
mod87=lme(log.bai~status,random = ~1|site/plot,data=dat87,
          na.action=na.exclude,method = "REML") 
summary(mod87) 
plot(mod87)
qqnorm(resid(mod87))
plot(x=dat87$site, y=resid(mod87), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat87$status, y=resid(mod87), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat87$plot, y=resid(mod87), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod88=lme(log.bai~status,random = ~1|site/plot,data=dat88,
          na.action=na.exclude,method = "ML")# 
mod88b=lme(log.bai~1,random = ~1|site/plot,data=dat88,
           na.action=na.exclude,method = "ML")#
model.sel(mod88,mod88b,rank=AICc) 
anova(mod88,mod88b)
####
mod88=lme(log.bai~status,random = ~1|site/plot,data=dat88,
          na.action=na.exclude,method = "REML")
summary(mod88)
plot(mod88)
qqnorm(resid(mod88))
plot(x=dat88$site, y=resid(mod88), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat88$status, y=resid(mod88), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat88$plot, y=resid(mod88), type="pearson")
abline(h = 0, lty = 2) # 

dat88c=subset(dat88,log.bai>0)
mod88c=lme(log.bai~status,random = ~1|site/plot,data=dat88c,
           na.action=na.exclude,method = "REML") #
summary(mod88c)  ###diff
plot(mod88c) ### same resuls so keep 0s 
#####
mod89=lme(log.bai~status,random = ~1|site/plot,data=dat89,
          na.action=na.exclude,method = "ML")# 
mod89b=lme(log.bai~1,random = ~1|site/plot,data=dat89,
           na.action=na.exclude,method = "ML")#
model.sel(mod89,mod89b,rank=AICc) #
anova(mod89,mod89b)
####
mod89=lme(log.bai~status,random = ~1|site/plot,data=dat89,
          na.action=na.exclude,method = "REML") 
summary(mod89)
plot(mod89)
qqnorm(resid(mod80))
plot(x=dat89$site, y=resid(mod89), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat89$status, y=resid(mod89), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat89$plot, y=resid(mod89), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod90=lme(log.bai~status,random = ~1|site/plot,data=dat90,
          na.action=na.exclude,method = "ML")# 
mod90b=lme(log.bai~1,random = ~1|site/plot,data=dat90,
           na.action=na.exclude,method = "ML")#
model.sel(mod90,mod90b,rank=AICc) # 
anova(mod90,mod90b)
####
mod90=lme(log.bai~status,random = ~1|site/plot,data=dat90,
          na.action=na.exclude,method = "REML") #
summary(mod90)
plot(mod90)
qqnorm(resid(mod90))
plot(x=dat90$site, y=resid(mod90), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat90$status, y=resid(mod90), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat90$plot, y=resid(mod90), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod91=lme(log.bai~status,random = ~1|site/plot,data=dat91,
          na.action=na.exclude,method = "ML")# 
mod91b=lme(log.bai~1,random = ~1|site/plot,data=dat91,
           na.action=na.exclude,method = "ML")#
model.sel(mod91,mod91b,rank=AICc) #  
anova(mod91,mod91b)
####
mod91=lme(log.bai~status,random = ~1|site/plot,data=dat91,
          na.action=na.exclude,method = "REML") 
summary(mod91) #
plot(mod90)
qqnorm(resid(mod91))
plot(x=dat91$site, y=resid(mod91), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat91$status, y=resid(mod91), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat91$plot, y=resid(mod91), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod92=lme(log.bai~status,random = ~1|site/plot,data=dat92,
          na.action=na.exclude,method = "ML")# 
mod92b=lme(log.bai~1,random = ~1|site/plot,data=dat92,
           na.action=na.exclude,method = "ML")#
model.sel(mod92,mod92b,rank=AICc) 
anova(mod92,mod92b)
####
mod92=lme(log.bai~status,random = ~1|site/plot,data=dat92,
          na.action=na.exclude,method = "REML") 
summary(mod92)
plot(mod92)
qqnorm(resid(mod92))
plot(x=dat92$site, y=resid(mod92), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat92$status, y=resid(mod92), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat92$plot, y=resid(mod92), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod93=lme(log.bai~status,random = ~1|site/plot,data=dat93,
          na.action=na.exclude,method = "ML")# 
mod93b=lme(log.bai~1,random = ~1|site/plot,data=dat93,
           na.action=na.exclude,method = "ML")#
model.sel(mod93,mod93b,rank=AICc) 
anova(mod93,mod93b)
####
mod93=lme(log.bai~status,random = ~1|site/plot,data=dat93,
          na.action=na.exclude,method = "REML") 
summary(mod93)#
plot(mod93)
qqnorm(resid(mod93))
plot(x=dat93$site, y=resid(mod93), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat93$status, y=resid(mod93), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat93$plot, y=resid(mod93), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod94=lme(log.bai~status,random = ~1|site/plot,data=dat94,
          na.action=na.exclude,method = "ML")# 
mod94b=lme(log.bai~1,random = ~1|site/plot,data=dat94,
           na.action=na.exclude,method = "ML")#
model.sel(mod94,mod94b,rank=AICc) 
anova(mod94,mod94b)
####
mod94=lme(log.bai~status,random = ~1|site/plot,data=dat94,
          na.action=na.exclude,method = "REML") 
summary(mod94)
plot(mod94)
qqnorm(resid(mod94))
plot(x=dat94$site, y=resid(mod94), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat94$status, y=resid(mod94), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat94$plot, y=resid(mod94), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod95=lme(log.bai~status,random = ~1|site/plot,data=dat95,
          na.action=na.exclude,method = "ML")# 
mod95b=lme(log.bai~1,random = ~1|site/plot,data=dat95,
           na.action=na.exclude,method = "ML")#
model.sel(mod95,mod95b,rank=AICc) 
anova(mod95,mod95b)
####
mod95=lme(log.bai~status,random = ~1|site/plot,data=dat95,
          na.action=na.exclude,method = "REML") 
summary(mod95)##
plot(mod95)
qqnorm(resid(mod95))
plot(x=dat95$site, y=resid(mod95), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat95$status, y=resid(mod95), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat95$plot, y=resid(mod95), type="pearson")
abline(h = 0, lty = 2) # 
#####
mod96=lme(log.bai~status,random = ~1|site/plot,data=dat96,
          na.action=na.exclude,method = "ML")# 
mod96b=lme(log.bai~1,random = ~1|site/plot,data=dat96,
           na.action=na.exclude,method = "ML")#
model.sel(mod96,mod96b,rank=AICc) # 
anova(mod96,mod96b)
####
mod96=lme(log.bai~status,random = ~1|site/plot,data=dat96,
          na.action=na.exclude,method = "REML") 
summary(mod96) 
plot(mod96)
qqnorm(resid(mod96))
plot(x=dat96$site, y=resid(mod96), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat96$status, y=resid(mod96), type="pearson")
abline(h = 0, lty = 2) #
plot(x=dat96$plot, y=resid(mod96), type="pearson")
abline(h = 0, lty = 2) # 

###### 
####### adjust pvalues

p=c(0.2814,0.245, 0.3259, 0.4194,0.5201, 0.4467,0.2291,0.1735,0.1458,0.0313,0.023,0.0784,0.0055,
    0.0051,0.0014,0.0068,0.0004,0.0002,0.0002, 0.0001,0.0001, 0.0125,0.0003, 0.0212,0.0002, 0.0001,
    0.0001,0.0001,0,0.0001,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
padj=p.adjust(p,"BH")
padj2=as.data.frame(padj)
#write.csv(padj2,file="padj2.csv")
