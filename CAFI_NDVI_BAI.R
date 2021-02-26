############
###############
################## NDVI - BAI analysis w/new NDVI data
#####
####### 
rm(list = ls())

library(dplyr)
library(reshape)
library(plyr)
library(MuMIn)
library(car)
library(effects)
library(nlme)
library(data.table)
library(tidyr)
library(ggplot2)

setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
data1=read.csv("CAFI_BAI.csv")
data2=data1%>% 
  rowwise()%>%
  mutate(log.bai = log(bai))  

names(data2)
head(data2)
tail(data2)
##################### REMOVE TREES THAT DIED BEFORE 1997
datB=subset(data2, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
              tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
              tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&
              tree!="TM6.61"& tree!="TM8.94")
#############
datB[] <- lapply(datB, function(x) if(is.factor(x)) factor(x) else x)
str(datB)
####
head(datB)
datB$site=as.factor(datB$site)
#names(datB)[names(datB) == "ï..year"] <- "year"

####################
########NEW ndvi data frame 

######7 year moving window
dat.ndvi7=read.csv("NDVI_7yr.csv",header=T) 
names(dat.ndvi7)
names(dat.ndvi7)[names(dat.ndvi7) == "psp"] <- "plot"
names(dat.ndvi7)[names(dat.ndvi7) == "ndvi.max"] <- "ndvi.max7"
str(dat.ndvi7)
dat.ndvi7$plot=as.factor(dat.ndvi7$plot)
dat.ndvi7$site=as.factor(dat.ndvi7$site)
str(dat.ndvi7)

#################
str(datB)
datB2=subset(datB,year>=1997 & year <=2015)
dat.ndvi2=subset(dat.ndvi7,year>=1997 & year <=2015)
datIND=merge(datB2,dat.ndvi2,by=c("year","plot","site"),all=TRUE)
names(datIND)
head(datIND)
str(datIND)
datIND$plot=as.factor(datIND$plot)
datIND$tree=as.factor(datIND$tree)
datIND$status=as.factor(datIND$status)

ggplot()+
  geom_point(data=datIND,aes(y=ndvi.max7,x=year),
             size=2)+facet_wrap(~plot)
###############
######################### calcualte BAI mean, SD, sample size for each year and status 
#### to see when the greatest number of dead trees available
datSUM=datIND %>% 
  group_by(site,plot,status,year) %>% 
  dplyr::summarize(avgB=mean(bai),n=n(),sd=sd(bai))

datSUM_M=subset(datSUM,status=="dying")
datSUM_A=subset(datSUM,status=="living")
head(datSUM_M)

library(ggplot2)
ggplot(datSUM_M,aes(x=year,y=n,color=plot))+geom_point()+facet_wrap(~site)
ggplot(datSUM_M,aes(x=year,y=n,color=plot))+geom_point()+facet_wrap(~plot)
ggplot(datSUM_M,aes(x=year,y=sd,color=plot))+geom_point()+facet_wrap(~site)
####
######
###############by site
datSUM2=datIND %>% 
  group_by(site,status,year) %>% 
  dplyr::summarize(avgB=mean(bai),n=n(),sd=sd(bai))

datSUM_M2=subset(datSUM2,status=="dying")
datSUM_A2=subset(datSUM2,status=="living")
head(datSUM_M2)

library(ggplot2)
ggplot(datSUM_M2,aes(x=year,y=n,color=site))+geom_point()+facet_wrap(~site)

#####
##########
########## separate living and dying 
datIND_live=subset(datIND,status=="living")
######REMOVE FACTOR LEVELS THAT I AM NOT USING 
datIND_live[] <- lapply(datIND_live, function(x) if(is.factor(x)) factor(x) else x)
str(datIND_live)
################
###########
datIND_dead=subset(datIND,status=="dying")
datIND_dead[] <- lapply(datIND_dead, function(x) if(is.factor(x)) factor(x) else x)
str(datIND_dead)
#########################
###### mean bai by plot 
##### living
head(datIND_dead)
datL5=datIND_live %>% 
  group_by(plot,year,site) %>% 
  dplyr::summarise(meanBAI_A=mean(log.bai))
str(datL5)
datL5=as.data.frame(datL5)
#####
#### dying 
datM5=datIND_dead %>% 
  group_by(plot,year,site) %>% 
  dplyr::summarise(meanBAI_M=mean(log.bai))
str(datM5)
datM5=as.data.frame(datM5)

datLM5=merge(datL5,datM5,by=c("plot","year","site"),all=TRUE)
str(datLM5)
datLM25=merge(datLM5,dat.ndvi2,by=c("year","plot","site"),all = TRUE)
head(datLM25)
str(datLM25)
tail(datLM25)
datLM_new=subset(datLM25,year>=1997 & year<=2013)
str(datLM_new)
head(datLM_new)
######## EFFECT OF LIVING AND DYING TREE BAI ON NDVI
##### TABLE S18, FIGURE 5
vif(lm(ndvi.max7~meanBAI_A+meanBAI_M,data=datLM_new)) ## not collinear
datLM_new=na.omit(datLM_new)
mod5_treesLOG=lme(ndvi.max7~meanBAI_A+meanBAI_M,random = ~1|site/plot,data=datLM_new,
                  na.action=na.exclude,method="ML")
summary(mod5_treesLOG)
##########################
mod5_treesLOG1=lme(ndvi.max7~meanBAI_A,random = ~1|site/plot,data=datLM_new,
                   na.action=na.exclude,method="ML")
anova(mod5_treesLOG,mod5_treesLOG1)
model.sel(mod5_treesLOG,mod5_treesLOG1,rank=AICc) #### sig

##########################
mod5_treesLOG2=lme(ndvi.max7~meanBAI_M,random = ~1|site/plot,data=datLM_new,
                   na.action=na.exclude,method="ML")
anova(mod5_treesLOG,mod5_treesLOG2)
model.sel(mod5_treesLOG,mod5_treesLOG2,rank=AICc) #### 
#############
############# -----compare null model to best model
mod5_treesN=lme(ndvi.max7~1,random = ~1|site/plot,data=datLM_new,
                  na.action=na.exclude,method="ML")
model.sel(mod5_treesLOG,mod5_treesN,rank=AICc)
anova(mod5_treesLOG,mod5_treesN)

### FINAL MODEL
mod5_treesLOG_7=lme(ndvi.max7~meanBAI_A+meanBAI_M,random = ~1|site/plot,data=datLM_new,
                  na.action=na.exclude,method="REML")
summary(mod5_treesLOG) ### 
plot(mod5_treesLOG)
qqnorm(resid(mod5_treesLOG))

########
plot(x=datLM_new$meanBAI_M, y=resid(mod5_treesLOG_7), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datLM_new$meanBAI_A, y=resid(mod5_treesLOG_7), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datLM_new$plot, y=resid(mod5_treesLOG_7), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datLM_new$site, y=resid(mod5_treesLOG_7), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datLM_new$year, y=resid(mod5_treesLOG_7), type="pearson")
abline(h = 0, lty = 2) #
#### 
######### until 2005 
### greatest # of dead trees  #### TABLE S6
datLM_new2=subset(datLM25,year<=2005)
str(datLM_new2)
head(datLM_new2)
datLM_new2=na.omit(datLM_new2)

mod5_treesLOGb=lme(ndvi.max7~meanBAI_A+meanBAI_M,random = ~1|site/plot,data=datLM_new2,
                   na.action=na.exclude,method="ML")
summary(mod5_treesLOGb) ###

mod5_treesLOGb1=lme(ndvi.max7~meanBAI_A,random = ~1|site/plot,data=datLM_new2,
                    na.action=na.exclude,method="ML")
anova(mod5_treesLOGb,mod5_treesLOGb1)
model.sel(mod5_treesLOGb,mod5_treesLOGb1, rank=AICc) #### 
####
mod5_treesLOGb2=lme(ndvi.max7~meanBAI_M,random = ~1|site/plot,data=datLM_new2,
                    na.action=na.exclude,method="ML")
anova(mod5_treesLOGb,mod5_treesLOGb2)
model.sel(mod5_treesLOGb,mod5_treesLOGb2, rank=AICc) #### 
#####
######## compare null model to best model
mod5_treesLOGb1N=lme(ndvi.max7~1,random = ~1|site/plot,data=datLM_new2,
                    na.action=na.exclude,method="ML")
model.sel(mod5_treesLOGb1N,mod5_treesLOGb1,rank=AICc)
anova(mod5_treesLOGb1N,mod5_treesLOGb1)

#################
mod5_treesLOGb=lme(ndvi.max7~meanBAI_A+meanBAI_M,random = ~1|site/plot,data=datLM_new2,
                   na.action=na.exclude,method="REML")
summary(mod5_treesLOGb) ###
#########################
########
plot(x=datLM_new2$meanBAI_M, y=resid(mod5_treesLOGb), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datLM_new2$meanBAI_A, y=resid(mod5_treesLOGb), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datLM_new2$plot, y=resid(mod5_treesLOGb), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datLM_new2$site, y=resid(mod5_treesLOGb), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datLM_new2$year, y=resid(mod5_treesLOGb), type="pearson")
abline(h = 0, lty = 2) #
###########################
########## FIGURES 
########## PLOT RESULTS 
eff.tdM<- Effect(c("meanBAI_M"),
                 xlevels=list(meanBAI_M=c(1,2,3,4,5,6,6.5)),
                 mod5_treesLOG_7)

eff.tdM=as.data.frame(eff.tdM)
head(eff.tdM)
str(eff.tdM)
dfM <- data.frame("meanBAI_M" = c(1:6,6.5), "Status" = c("Dying","Dying","Dying",
                                                         "Dying","Dying","Dying","Dying"))
str(dfM)
dfM$meanBAI_M=as.numeric(dfM$meanBAI_M)
dfM$Status=as.factor(dfM$Status)

dfM2=merge(dfM,eff.tdM,by="meanBAI_M",all=TRUE)

#####rename 1st column
dfM3=dfM2 %>% 
  dplyr::rename(meanBAI = meanBAI_M)

eff.tdA<- Effect(c("meanBAI_A"),
                 xlevels=list(meanBAI_A=c(1,2,3,4,5,6,6.5)),
                 mod5_treesLOG_7)

eff.tdA=as.data.frame(eff.tdA)
head(eff.tdA)

dfA <- data.frame("meanBAI_A" = c(1:6,6.5), "Status" = c("Living","Living","Living",
                                                         "Living","Living","Living","Living"))
str(dfA)
dfA$meanBAI_A=as.numeric(dfA$meanBAI_A)
dfA$Status=as.factor(dfA$Status)

dfA2=merge(dfA,eff.tdA,by="meanBAI_A",all=TRUE)

#####rename 1st column
dfA3=dfA2 %>% 
  dplyr::rename(meanBAI = meanBAI_A)

#####
###### bind and put into long format, new column w/group name
eff_BAI <- rbind(dfA3,dfM3)
str(eff_BAI)
######### raw data frame into long format
str(datLM_new)
datM1=datLM_new[,c(1:3,5,8)]
str(datM1)
vec=data.frame(matrix(ncol=1,nrow=214, dimnames=list(NULL, c("name"))))
vec2=vec$name%>%replace_na("Dying")
vec2=as.data.frame(vec2)
str(vec2)
vec2$vec2=as.factor(vec2$vec2)
vec3=vec2 %>% 
  dplyr::rename(Status = vec2)

head(vec3)
head(datM1)
datM1b=cbind(vec3,datM1)
str(datM1b)
datM1b=datM1b %>% 
  dplyr::rename(meanBAI = meanBAI_M)
# live trees
names(datLM_new)
datA1=datLM_new[,c(1:4,8)]
str(datA1)
vecA=data.frame(matrix(ncol=1,nrow=214, dimnames=list(NULL, c("name"))))
vecA2=vecA$name%>%replace_na("Living")
vecA2=as.data.frame(vecA2)
str(vecA2)
vecA2$vecA2=as.factor(vecA2$vecA2)
vecA3=vecA2 %>% 
  dplyr::rename(Status = vecA2)
datA1b=cbind(vecA3,datA1)
datA1b=datA1b %>% 
  dplyr::rename(meanBAI = meanBAI_A)

dat_all=rbind(datA1b,datM1b)
str(dat_all)
head(dat_all)
######## lets make plot with points to see what things look like 
library(ggplot2)
head(eff_BAI)
fig_ndvi=ggplot()+
  geom_point(data=dat_all,aes(y=ndvi.max7,x=meanBAI,fill=Status,shape=Status),
             size=2)+
  geom_line(data=eff_BAI,aes(y=fit,x=meanBAI,group=Status,color=Status),lwd=1.25)+
  geom_ribbon(data=eff_BAI,aes(x=meanBAI,y=fit,ymin=lower, 
                               ymax=upper,group=Status,fill=Status),
              alpha=0.15,linetype=0)+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=14))+ylab(expression(NDVI["max"])) +
  scale_x_continuous(limits = c(1,6.5),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=5))+
  labs(x=expression(Plot-level*phantom(x)*BAI~(ln(mm^2))))+
  scale_color_manual(values = c( "dodgerblue2",  "firebrick3"))+
  scale_fill_manual(values = c( "dodgerblue2",  "firebrick3"))+
  scale_shape_manual(values=c(21,24))


fig_ndvi

ggsave(plot = fig_ndvi, filename = "fig_NDVI_TR.jpeg",
       dpi = 300, width = 6.5, height = 4) ###

######
############
################### +1 LAG IN BAI
####
str(datB)
datB2B=subset(datB,year>=1996 & year <=2015)
dat.ndvi2B=subset(dat.ndvi7,year>=1996 & year <=2015)
datINDB=merge(datB2B,dat.ndvi2B,by=c("year","plot","site"),all=TRUE)
names(datINDB)
head(datINDB)
str(datINDB)
###############
########## separate living and dying 
datIND_liveB=subset(datINDB,status=="living")
######REMOVE FACTOR LEVELS THAT I AM NOT USING 
datIND_liveB[] <- lapply(datIND_liveB, function(x) if(is.factor(x)) factor(x) else x)
str(datIND_liveB)
################
###########
datIND_deadB=subset(datINDB,status=="dying")
datIND_deadB[] <- lapply(datIND_deadB, function(x) if(is.factor(x)) factor(x) else x)
str(datIND_deadB)
#########################
###### mean bai by plot 
##### living
datL5B=datIND_liveB %>% 
  group_by(plot,year,site) %>% 
  dplyr::summarise(meanBAI_A=mean(log.bai))
str(datL5B)
datL5B=as.data.frame(datL5B)
#####
#### dying 
datM5B=datIND_deadB %>% 
  group_by(plot,year,site) %>% 
  dplyr::summarise(meanBAI_M=mean(log.bai))
str(datM5B)
datM5B=as.data.frame(datM5B)

datLM5B=merge(datL5B,datM5B,by=c("plot","year","site"),all=TRUE)
str(datLM5B)
datLM5B=merge(datLM5B,dat.ndvi2B,by=c("year","plot","site"),all = TRUE)
head(datLM5B)
str(datLM5B)

library(data.table)
df_n3=as.data.table(datLM5B)
str(df_n3)
tail(df_n3)

df_n3=df_n3[,c(1:5)]
head(df_n3)
tail(df_n3)
str(df_n3)

invisible(df_n3[year == "1996", year := "X"])
invisible(df_n3[year == "1997", year := "1996"])
invisible(df_n3[year == "1998", year := "1997"])
invisible(df_n3[year == "1999", year := "1998"])
invisible(df_n3[year == "2000", year := "1999"])
invisible(df_n3[year == "2001", year := "2000"])
invisible(df_n3[year == "2002", year := "2001"])
invisible(df_n3[year == "2003", year := "2002"])
invisible(df_n3[year == "2004", year := "2003"])
invisible(df_n3[year == "2005", year := "2004"])
invisible(df_n3[year == "2006", year := "2005"])
invisible(df_n3[year == "2007", year := "2006"])
invisible(df_n3[year == "2008", year := "2007"])
invisible(df_n3[year == "2009", year := "2008"])
invisible(df_n3[year == "2010", year := "2009"])
invisible(df_n3[year == "2011", year := "2010"])
invisible(df_n3[year == "2012", year := "2011"])
invisible(df_n3[year == "2013", year := "2012"])
invisible(df_n3[year == "2014", year := "2013"])
invisible(df_n3[year == "2015", year := "2014"])

names(df_n3)[names(df_n3) == "meanBAI_A"] <- "meanBAI_A_lag1"
names(df_n3)[names(df_n3) == "meanBAI_M"] <- "meanBAI_M_lag1"
names(df_n3)
head(df_n3)

##############
df_ALL=merge(df_n3,datLM5B,by=c("year","site","plot"),all=TRUE)
head(df_ALL)
# 

df_ALL2=subset(df_ALL,year>=1997 & year<=2013)
str(df_ALL2)
df_ALL2=na.omit(df_ALL2)
vif(lm(ndvi.max7~meanBAI_A_lag1+meanBAI_M_lag1+meanBAI_A+meanBAI_M,data=df_ALL2)) ## not collinear
mod_lag_all=lme(ndvi.max7~meanBAI_A_lag1+meanBAI_M_lag1+meanBAI_A+meanBAI_M,random = ~1|site/plot,data=df_ALL2,
                na.action=na.exclude,method="ML")
summary(mod_lag_all) ### 

mod_lag_all2=update(mod_lag_all, .~. -meanBAI_A_lag1)
model.sel(mod_lag_all2,mod_lag_all,rank = AICc) ###not sig 
anova(mod_lag_all2,mod_lag_all) ###

mod_lag_all3=update(mod_lag_all, .~. -meanBAI_M_lag1)
model.sel(mod_lag_all3,mod_lag_all,rank = AICc) ## not sig 
anova(mod_lag_all3,mod_lag_all) ###

mod_lag_all4=update(mod_lag_all, .~. -meanBAI_M)
model.sel(mod_lag_all4,mod_lag_all,rank = AICc) ## sig 
anova(mod_lag_all4,mod_lag_all) ###

mod_lag_all5=update(mod_lag_all, .~. -meanBAI_A)
model.sel(mod_lag_all5,mod_lag_all,rank = AICc) ## sig 
anova(mod_lag_all5,mod_lag_all) ###

########### compare null to best model
mod_lag0=lme(ndvi.max7~1,random = ~1|site/plot,data=df_ALL2,
                na.action=na.exclude,method="ML")
mod_lagB=lme(ndvi.max7~meanBAI_A+meanBAI_M,random = ~1|site/plot,data=df_ALL2,
             na.action=na.exclude,method="ML")
model.sel(mod_lag0,mod_lagB,rank = AICc)
anova(mod_lag0,mod_lagB)

########
mod_lag_all=lme(ndvi.max7~meanBAI_A_lag1+meanBAI_M_lag1+meanBAI_A+meanBAI_M,random = ~1|site/plot,data=df_ALL2,
                na.action=na.exclude,method="REML")
summary(mod_lag_all) ### 

plot(mod_lag_all)
qqnorm(resid(mod_lag_all))
########
plot(x=df_ALL2$meanBAI_M, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) #
plot(x=df_ALL2$meanBAI_A, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) 
plot(x=df_ALL2$plot, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) #
plot(x=df_ALL2$site, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) 
plot(x=df_ALL2$meanBAI_A_lag1, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) #
plot(x=df_ALL2$meanBAI_M_lag1, y=resid(mod_lag_all), type="pearson")
abline(h = 0, lty = 2) #


