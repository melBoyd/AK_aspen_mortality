################# 
#####  --------- analyses: impacts of alm and climate on BAI
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
##############------climate x alm analyses 
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
######
names(data.clim)
str(data.clim)
##########

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
######### MEAN TEMP BY EYAR
dat.meanT<-  
  ddply(data4, c("year"), summarise,
        N    = length(GS_temp),
        meanB = mean(GS_temp),
        sdB   = sd(GS_temp),
        seB = sdB / sqrt(N))
#####mean gs cmi by year
##########
dat.meanC<-  
  ddply(data4, c("year"), summarise,
        N    = length(GS_CMI),
        meanB = mean(GS_CMI),
        sdB   = sd(GS_CMI),
        seB = sdB / sqrt(N))
######## living trees
names(data4)
head(data4)
tail(data4)
str(data4)
data4=as.data.frame(data4)
#####
######## compare living and dying tree productivity
##### Figure 1 and Table S13
data4$tree=as.factor(data4$tree)
data4$status=as.factor(data4$status)

data4$status = relevel(data4$status, ref="living")
##############
modALM=lme(log.bai~status,random = ~1|site/plot/tree,data=data4,
           na.action=na.exclude,method = "ML") 
modALM2=lme(log.bai~1,random = ~1|site/plot/tree,data=data4,
           na.action=na.exclude,method = "ML") 
anova(modALM,modALM2)
model.sel(modALM,modALM2,rank = AICc) # sig

###### FINAL MODEL
modALM=lme(log.bai~status,random = ~1|site/plot/tree,data=data4,
            na.action=na.exclude,method = "REML") 
summary(modALM)
plot(modALM)
qqnorm(resid(modALM)) 
#######
plot(x=data4$status, y=resid(modALM), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data4$site, y=resid(modALM), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data4$plot, y=resid(modALM), type="pearson")
abline(h = 0, lty = 2) #
plot(x=data4$tree, y=resid(modALM), type="pearson")
abline(h = 0, lty = 2) #

############
dat_alive=subset(data4,status=="living")
datA_lm=subset(dat_alive,year>=1997 & year<=2013)
datA_lm[] <- lapply(datA_lm, function(x) if(is.factor(x)) factor(x) else x)
str(datA_lm)
datA_lm=as.data.table(datA_lm)
names(datA_lm)
#
### just leaf mining 
########## determine if alm or almp has greater effect on BAI
###  Table S5
vif(lm(log.bai~scaleALMP+scaleALM,data=datA_lm)) ## not collinear
names(datA_lm)
datA_lm2=datA_lm[,c(1:7,22,23)]
names(datA_lm2)
datA_lm2=na.omit(datA_lm2)
modA_alm=lme(log.bai~scaleALMP+scaleALM,random=~1|site/plot/tree,data=datA_lm2,
             na.action=na.exclude, method="ML")
######
modA_alm1=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datA_lm2,
              na.action=na.exclude, method="ML")
anova(modA_alm1,modA_alm) ## SIG
model.sel(modA_alm1,modA_alm,rank=AICc)

modA_alm2=lme(log.bai~scaleALM,random=~1|site/plot/tree,data=datA_lm2,
              na.action=na.exclude, method="ML")
anova(modA_alm2,modA_alm) ## SIG
model.sel(modA_alm2,modA_alm,rank=AICc)

### compare null to best model
modA_alm0=lme(log.bai~1,random=~1|site/plot/tree,data=datA_lm2,
              na.action=na.exclude, method="ML")
model.sel(modA_alm,modA_alm0,rank=AICc) #ALM
anova(modA_alm,modA_alm0)
#######
########### FINAL MODEL
modA_alm=lme(log.bai~scaleALMP+scaleALM,random=~1|site/plot/tree,data=datA_lm2,
             na.action=na.exclude, method="REML")
summary(modA_alm)
anova(modA_alm)
plot(modA_alm)
qqnorm(resid(modA_alm))
###
plot(x=datA_lm$year, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA_lm$tree, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA_lm$plot, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA_lm$site, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA_lm$scaleALM, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #
plot(x=datA_lm$scaleALMP, y=resid(modA_alm), type="pearson") ##good
abline(h = 0, lty = 2) #



########## build interaction model and reduce
#### Table S14 and Table S15, Figure 4
vif(lm(log.bai~scaleGS_temp+scaleALMP+scaleGSP_CMI,data=datA_lm)) ## not collinear
datA_lm3=datA_lm[,c(1:7,22:25)]
names(datA_lm3)
datA_lm3=na.omit(datA_lm3)
modA=lme(log.bai~scaleALMP*scaleGS_temp + scaleALMP*scaleGSP_CMI,random=~1|site/plot/tree,data=datA_lm3,
         na.action=na.exclude, method="ML")
summary(modA)
modA2=update(modA,~.-scaleALMP:scaleGSP_CMI)
anova(modA,modA2) ## remove
model.sel(modA,modA2,rank=AICc)
modA3=update(modA,~.-scaleALMP:scaleGS_temp)
anova(modA,modA3) ## remove
model.sel(modA,modA3,rank=AICc) ## remove 
##
####### reduced model
modA_new=lme(log.bai~scaleALMP+scaleGS_temp + scaleGSP_CMI,random=~1|site/plot/tree,data=datA_lm3,
               na.action=na.exclude, method="ML") ### 
summary(modA_new)
##########################
modA2B=update(modA_new,~.-scaleALMP)
anova(modA_new,modA2B) ## sig
model.sel(modA_new,modA2B,rank=AICc)

modA3B=update(modA_new,~.-scaleGS_temp)
anova(modA_new,modA3B) #sig
model.sel(modA_new,modA3B,rank=AICc) ## sig 

modA4B=update(modA_new,~.-scaleGSP_CMI)
anova(modA_new,modA4B) #sig
model.sel(modA_new,modA4B,rank=AICc) ## sig 

### compare null to best model
modA0_int=lme(log.bai~1,random=~1|site/plot/tree,data=datA_lm3,
             na.action=na.exclude, method="ML") ### 
model.sel(modA0_int,modA_new,rank=AICc) # modA_new
anova(modA0_int,modA_new)
##########################
modA_newRE=lme(log.bai~scaleALMP+scaleGS_temp + scaleGSP_CMI,random=~1|site/plot/tree,data=datA_lm,
               na.action=na.exclude, method="REML") ### everything sig 
summary(modA_newRE)
plot(allEffects(modA_newRE))

plot(modA_newRE)
qqnorm(resid(modA_newRE))
plot(x=datA_lm$plot, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_lm$site, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_lm$tree, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_lm$scaleALMP, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_lm$scaleGSP_CMI, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datA_lm$scaleGS_temp, y=resid(modA_newRE), type="pearson")
abline(h = 0, lty = 2) #

#########################
######## dying 
###### almp vs alm Table S5
head(data4)
dat_dead=subset(data4,status=="dying")
datM_lm=subset(dat_dead,year>=1997 & year<=2013)
str(datM_lm)
names(datM_lm)
datM_lm[] <- lapply(datM_lm, function(x) if(is.factor(x)) factor(x) else x)
str(datM_lm)
datM_lm=as.data.table(datM_lm)
### just leaf mining 
vif(lm(log.bai~scaleALMP+scaleALM,data = datM_lm)) # not collinear
datM_lm2=datM_lm[,c(1:7,22,23)]
names(datM_lm2)
datM_lm2=na.omit(datM_lm2)
modM_alm=lme(log.bai~scaleALMP+scaleALM,random=~1|site/plot/tree,data=datM_lm2,
             na.action=na.exclude, method="ML")
######
modM_alm1=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datM_lm2,
              na.action=na.exclude, method="ML")
anova(modM_alm1,modM_alm) ## SIG
model.sel(modM_alm1,modM_alm,rank=AICc)

modM_alm2=lme(log.bai~scaleALM,random=~1|site/plot/tree,data=datM_lm2,
              na.action=na.exclude, method="ML")
anova(modM_alm2,modM_alm) ## SIG
model.sel(modM_alm2,modM_alm,rank=AICc)

### compare null to best model
modM0=lme(log.bai~1,random=~1|site/plot/tree,data=datM_lm2,
              na.action=na.exclude, method="ML")
model.sel(modM_alm,modM0,rank=AICc) # modM_alm
anova(modM_alm,modM0)
# final model
modM_alm=lme(log.bai~scaleALMP+scaleALM,random=~1|site/plot/tree,data=datM_lm2,
             na.action=na.exclude, method="REML")
summary(modM_alm)
plot(modM_alm)
qqnorm(resid(modM_alm))
r.squaredGLMM(modM_alm)

plot(x=datM_lm$tree, y=resid(modM_alm), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$site, y=resid(modM_alm), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$plot, y=resid(modM_alm), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$scaleALM, y=resid(modM_alm), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$scaleALMP, y=resid(modM_alm), type="pearson")
abline(h = 0, lty = 2) #
######
########
#######
###########-----interaction Table S14 and S15, Figure 4
vif(lm(log.bai~scaleALMP+scaleGS_temp+scaleGSP_CMI,data = datM_lm)) # not collinear
datM_lm3=datM_lm[,c(1:7,22:25)]
names(datM_lm3)
datM_lm3=na.omit(datM_lm3)
modM=lme(log.bai~scaleALMP*scaleGS_temp+scaleALMP*scaleGSP_CMI,random=~1|site/plot/tree,data=datM_lm3,
         na.action=na.exclude,method = "ML")### if i dont include tree as 
##### random effect then reisudals look good WITH CORRELATION=CORAR1
summary(modM)

modM2=update(modM,~.-scaleALMP:scaleGSP_CMI)
anova(modM,modM2) ## remove
model.sel(modM,modM2,rank = AICc)
modM3=update(modM,~.-scaleALMP:scaleGS_temp)
anova(modM,modM3) ## remove
model.sel(modM,modM3,rank = AICc) 
########################## reduced model
modM_new=lme(log.bai~scaleALMP+scaleGS_temp + scaleGSP_CMI,random=~1|site/plot/tree,data=datM_lm3,
             na.action=na.exclude, method="ML") ### 
summary(modM_new)
##########################
modM2B=update(modM_new,~.-scaleALMP)
anova(modM_new,modM2B) ## sig
model.sel(modM_new,modM2B,rank=AICc)
modM3B=update(modM_new,~.-scaleGS_temp)
anova(modM_new,modM3B) # not sig
model.sel(modM_new,modM3B,rank=AICc) # not sig
modM4B=update(modM_new,~.-scaleGSP_CMI)
anova(modM_new,modM4B) #NOT SIG
model.sel(modM,modM4B,rank=AICc) ##NOT SIG

modM_alm_new=lme(log.bai~scaleALMP,random=~1|site/plot/tree,data=datM_lm3,
                 na.action=na.exclude, method="ML") ### 
### compare null to best model
modM0_int=lme(log.bai~1,random=~1|site/plot/tree,data=datM_lm3,
             na.action=na.exclude, method="ML") ### 
model.sel(modM_alm_new,modM0_int,rank=AICc) ##modM_alm_new
anova(modM_alm_new,modM0_int)
##########################
##### final model 
modM_REML=lme(log.bai~scaleALMP+scaleGS_temp+scaleGSP_CMI,random=~1|site/plot/tree,data=datM_lm3,
              na.action=na.exclude,method = "REML")### i
summary(modM_REML)

plot(x=datM_lm$plot, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$site, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$scaleGSP_CMI, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$scaleGS_temp, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$scaleALMP, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datM_lm$tree, y=resid(modM_REML), type="pearson")
abline(h = 0, lty = 2) #

###
######
########## figures 
#############plot effects
library(effects)
############### almp
effA_alm<- effect(term=c("scaleALMP"),mod=modA_newRE,
                  xlevels=list(scaleALMP=c(-1.2,0,1,2,2.2)))
########
effA_alm=as.data.frame(effA_alm)
######## ---gsp cmi
effA_cmi<- effect(term=c("scaleGSP_CMI"),mod=modA_newRE,
                  xlevels=list(scaleGSP_CMI=c(-2,-1,0,1,2,3,3.1)))
effA_cmi=as.data.frame(effA_cmi)
####----temp
effA_temp<- effect(term=c("scaleGS_temp"),mod=modA_newRE,
                   xlevels=list(scaleGS_temp=c(-2.3,-2,-1,0,1,2,2.4)))
########
effA_temp=as.data.frame(effA_temp)
head(effA_temp)
#######
head(datA_lm)
head(effA_alm)
p1ALM=ggplot()+
  geom_jitter(data=datA_lm, aes(x=scaleALMP, y=log.bai), 
             size=1.5,shape=21,fill="dodgerblue2",width = 0.1)+
  geom_ribbon(data=effA_alm,aes(x=scaleALMP,ymin=lower, 
                             ymax = upper),
              alpha=0.3,linetype=0,na.rm = TRUE,fill="dodgerblue2")+
  geom_line(data=effA_alm,aes(y=fit,x=scaleALMP),lwd=1,color="dodgerblue2")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14),legend.position = "none")+
  theme(text=element_text(family="Helvetica"))+
  scale_y_continuous(limits = c(0,7),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=5))+xlab("Previous leaf mining (z-score)")+
  labs(y=expression(paste(BAI  (ln* ("mm"^2)))))+
  geom_text(aes(label = "(c)"), x = -1.2, y=7,fontface=2,size=4) 


p1ALM
#ggsave(plot = p1ALM, filename = "alive_ALM.pdf", path = "~/Documents/CAFI_all_final",
      # dpi = 600, width = 6, height = 4)

###
############ ------CMI
p1CMI=ggplot()+
  geom_jitter(data=datA_lm, aes(x=scaleGSP_CMI, y=log.bai), 
              size=1.5,shape=21,fill="dodgerblue2",width = 0, height = 0)+
  geom_ribbon(data=effA_cmi,aes(x=scaleGSP_CMI,ymin=lower, 
                                ymax = upper),
              alpha=0.3,linetype=0,na.rm = TRUE,fill="dodgerblue2")+
  geom_line(data=effA_cmi,aes(y=fit,x=scaleGSP_CMI),lwd=1,color="dodgerblue2")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14),legend.position = "none")+
  theme(text=element_text(family="Helvetica"))+
  scale_y_continuous(limits = c(0,7),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=5))+xlab("PGS CMI (z-score)")+
  labs(y=expression(paste(BAI  (ln* ("mm"^2)))))+
  geom_text(aes(label = "(a)"), x = -2.06, y=7,fontface=2,size=4) 

p1CMI

#ggsave(plot = p1CMI, filename = "alive_CMI.pdf", path = "~/Documents/CAFI_all_final",
 #      dpi = 600, width = 6, height = 4)

############ ------ temp 
p1temp=ggplot()+
  geom_jitter(data=datA_lm, aes(x=scaleGS_temp, y=log.bai), 
              size=1.5,shape=21,fill="dodgerblue2",width = 0, height = 0)+
  geom_ribbon(data=effA_temp,aes(x=scaleGS_temp,ymin=lower, 
                                ymax = upper),
              alpha=0.3,linetype=0,na.rm = TRUE,fill="dodgerblue2")+
  geom_line(data=effA_temp,aes(y=fit,x=scaleGS_temp),lwd=1,color="dodgerblue2")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14),legend.position = "none")+
  theme(text=element_text(family="Helvetica"))+
  scale_y_continuous(limits = c(0,7),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=5))+xlab("GS temperature (z-score)")+
labs(y=expression(paste(BAI  (ln* ("mm"^2)))))+
  geom_text(aes(label = "(b)"), x = -2.3, y=7,fontface=2,size=4) 

p1temp

#ggsave(plot = p1temp, filename = "alive_temp.pdf", path = "~/Documents/CAFI_all_final",
 #      dpi = 600, width = 6, height = 4)

########## figures 
#############plot effects
modM_REML=lme(log.bai~scaleALMP+scaleGS_temp+scaleGSP_CMI,random=~1|site/plot/tree,data=datM_lm,
              na.action=na.exclude,method = "REML")### if i dont include tree as 
###
###### DYING 

library(effects)
############### almp
effM_alm<- effect(term=c("scaleALMP"),mod=modM_REML,
                  xlevels=list(scaleALMP=c(-1.3,0,1,2,2.4)))
########
effM_alm=as.data.frame(effM_alm)
head(effM_alm)

p1ALM_M=ggplot()+
  geom_jitter(data=datM_lm, aes(x=scaleALMP, y=log.bai), 
              size=1.5,shape=21,fill="firebrick3",width = 0.1)+
  geom_ribbon(data=effM_alm,aes(x=scaleALMP,ymin=lower, 
                                ymax = upper),
              alpha=0.3,linetype=0,na.rm = TRUE,fill="firebrick3")+
  geom_line(data=effM_alm,aes(y=fit,x=scaleALMP),lwd=1,color="firebrick3")+
  theme_bw()+
  theme(axis.text=element_text(size=12,color="black"),axis.title = 
          element_text(size=14),legend.position = "none")+
  theme(text=element_text(family="Helvetica"))+
  scale_y_continuous(limits = c(0,6.4),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=4))+xlab("Previous leaf mining (z-score)")+
  labs(y=expression(paste(BAI  (ln* ("mm"^2)))))+
  geom_text(aes(label = "(d)"), x = -1.3, y=6.4,fontface=2,size=4) 


p1ALM_M

#ggsave(plot = p1ALM_M, filename = "DEAD_ALM.pdf", path = "~/Documents/CAFI_all_final",
    #  dpi = 600, width = 6, height = 4)

library(ggpubr)
fig=ggarrange(p1CMI+rremove("ylab"),p1temp+rremove("ylab"),p1ALM+rremove("ylab"),p1ALM_M+rremove("ylab"),
          ncol = 2, nrow = 2)

fig2=annotate_figure(fig,
                     left = text_grob(expression(BAI~(ln(mm^2))),rot=90,size=14))
fig2

ggsave(plot = fig2, filename = "fig2_clim_cafi.jpeg",
         dpi = 600, width = 8, height = 6)




