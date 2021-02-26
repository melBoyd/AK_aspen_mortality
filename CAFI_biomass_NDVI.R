##
############
################# ANLYSIS: NDVI - cafi database measurements (biomass) - new NDVI data
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

setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
#######
##########mortality, recruitment, and growth are every 5 years (mg/ha/every 5 years)
caf_mortC2=read.csv("caf_mort_biomass.csv")
names(caf_mortC2)
head(caf_mortC2)
str(caf_mortC2)
#names(caf_mortC2)[names(caf_mortC2) == "ï..plot"] <- "plot"

caf_mortC2$plot=as.factor(caf_mortC2$plot)
plot(mort_corrected~year,data=caf_mortC2)
######
#############
#########################
#####NEW ndvi data frame 

######  7 year moving window
dat.ndvi7=read.csv("NDVI_7yr.csv",header=T) 
names(dat.ndvi7)
names(dat.ndvi7)[names(dat.ndvi7) == "psp"] <- "plot"
names(dat.ndvi7)[names(dat.ndvi7) == "ndvi.max"] <- "ndvi.max7"
str(dat.ndvi7)
dat.ndvi7$plot=as.factor(dat.ndvi7$plot)
dat.ndvi7$site=as.factor(dat.ndvi7$site)
str(dat.ndvi7)
############ merge NDVI data frames
#########
######################## 
dat.ndvi2=subset(dat.ndvi7,year>=1997 & year<=2015)

head(caf_mortC2)
caf_mortC3=subset(caf_mortC2,year>=1997 & year<=2015)
caf_mortC4=merge(dat.ndvi2,caf_mortC3,by=c("plot","year"),all = TRUE)
head(caf_mortC4)
###### 
#####
caf_recruitment=read.csv("caf_recruit_aspen.csv")
caf_growth=read.csv("CAFI_biomass_growth.csv")
head(caf_recruitment)
head(caf_growth)
#names(caf_recruitment)[names(caf_recruitment) == "ï..plot"] <- "plot"
#names(caf_growth)[names(caf_growth) == "ï..plot"] <- "plot"

caf_recruitment$plot=as.factor(caf_recruitment$plot)
caf_growth$plot=as.factor(caf_growth$plot)
caf_mortN=merge(caf_recruitment,caf_mortC4,by=c("plot","year"),all = TRUE)
head(caf_mortN)

caf_mortN2=merge(caf_mortN,caf_growth,by=c("plot","year"),all = TRUE)
head(caf_mortN2)
names(caf_mortN2)
str(caf_mortN2)

caf_mortN3=subset(caf_mortN2,year>=1997 & year<=2015)
caf_mortN4=na.omit(caf_mortN3)
head(caf_mortN4)
tail(caf_mortN4)
str(caf_mortN4)

caf_mortN4$site=as.factor(caf_mortN4$site)
str(caf_mortN4)

########
########## until 2013 
caf_mortN13=subset(caf_mortN4,year<=2013 & year>=1997)
head(caf_mortN13)
str(caf_mortN13)

mod_biomass=lme(ndvi.max7~mort_corrected+biomass_growth+recruitment_aspen_correct,
                random=~1|site/plot,data=caf_mortN13,
                na.action=na.exclude,method = "ML")
summary(mod_biomass)
qqnorm(resid(mod_biomass))
plot(mod_biomass)
##
##### 
mod_biomass2=lme(ndvi.max7~biomass_growth+recruitment_aspen_correct,
                 random=~1|site/plot,data=caf_mortN13,
                 na.action=na.exclude,method = "ML")
anova(mod_biomass,mod_biomass2) ### MORTALITY SIG 
model.sel(mod_biomass,mod_biomass2,rank = AICc)

mod_biomass3=lme(ndvi.max7~biomass_growth+mort_corrected,
                 random=~1|site/plot,data=caf_mortN13,
                 na.action=na.exclude,method = "ML")
anova(mod_biomass,mod_biomass3) ### recruitment NOT sig
model.sel(mod_biomass,mod_biomass3,rank = AICc)

mod_biomass4=lme(ndvi.max7~mort_corrected+recruitment_aspen_correct,
                 random=~1|site/plot,data=caf_mortN13,
                 na.action=na.exclude,method = "ML")
anova(mod_biomass,mod_biomass4) ### rgrowth sig
model.sel(mod_biomass,mod_biomass4,rank = AICc)


###################compare null model to best model
mod_biomassN=lme(ndvi.max7~1,
                 random=~1|site/plot,data=caf_mortN13,
                 na.action=na.exclude,method = "ML")
mod_biomassB=lme(ndvi.max7~biomass_growth+mort_corrected,
                 random=~1|site/plot,data=caf_mortN13,
                 na.action=na.exclude,method = "ML")
################################
model.sel(mod_biomassN,mod_biomassB,
          rank = AICc) #
anova(mod_biomassN,mod_biomassB)
####FINAL MODEL 
######### table S18, figure 6
mod_biomass=lme(ndvi.max7~mort_corrected+biomass_growth+recruitment_aspen_correct,
                random=~1|site/plot,data=caf_mortN13,
                na.action=na.exclude,method = "REML")
summary(mod_biomass)
anova(mod_biomass)
str(caf_mortN13)


plot(x=caf_mortN13$site, y=resid(mod_biomass), type="pearson")
abline(h = 0, lty = 2)
plot(x=caf_mortN13$plot, y=resid(mod_biomass), type="pearson")
abline(h = 0, lty = 2)  
plot(x=caf_mortN13$mort_corrected, y=resid(mod_biomass), type="pearson")
abline(h = 0, lty = 2) #
plot(x=caf_mortN13$biomass_growth, y=resid(mod_biomass), type="pearson")
abline(h = 0, lty = 2) 
plot(x=caf_mortN13$recruitment_aspen_correct, y=resid(mod_biomass), type="pearson")
abline(h = 0, lty = 2)

#####################what about when biomass mortality is <=40 (the two highest points)
mod_biomass40=lme(ndvi.max7~mort_corrected+biomass_growth+recruitment_aspen_correct,
                  random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                  na.action=na.exclude,method = "ML")
summary(mod_biomass40)

#####################
mod_biomass40B=lme(ndvi.max7~biomass_growth+recruitment_aspen_correct,
                   random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                   na.action=na.exclude,method = "ML")
anova(mod_biomass40,mod_biomass40B) ### MORT NOT IMPORTANT 
model.sel(mod_biomass40,mod_biomass40B,rank = AICc)
#####################
mod_biomass40C=lme(ndvi.max7~mort_corrected+recruitment_aspen_correct,
                   random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                   na.action=na.exclude,method = "ML")
anova(mod_biomass40,mod_biomass40C) ### growth IMPORTANT 
model.sel(mod_biomass40,mod_biomass40C,rank = AICc)
###################3
mod_biomass40D=lme(ndvi.max7~mort_corrected+biomass_growth,
                   random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                   na.action=na.exclude,method = "ML")
anova(mod_biomass40,mod_biomass40D) ### RECRUITMENT NOT IMORTANT  
model.sel(mod_biomass40,mod_biomass40D,rank = AICc)

######### compare null model to best model
mod_biomass40N=lme(ndvi.max7~1,
                 random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                 na.action=na.exclude,method = "ML")

mod_biomass40G=lme(ndvi.max7~biomass_growth,
                   random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                   na.action=na.exclude,method = "ML")

model.sel(mod_biomass40N,mod_biomass40G,
          rank = AICc) # mod w/growth

anova(mod_biomass40N,mod_biomass40G)
####################final model

mod_biomass40_final=lme(ndvi.max7~mort_corrected+biomass_growth+recruitment_aspen_correct,
                        random=~1|site/plot,data=subset(caf_mortN13,mort_corrected<=40),
                        na.action=na.exclude,method = "REML")
summary(mod_biomass40_final) ### only growth important 
##################
datx=subset(caf_mortN13,mort_corrected<=40)
plot(x=datx$site, y=resid(mod_biomass40_final), type="pearson")
abline(h = 0, lty = 2)
plot(x=datx$plot, y=resid(mod_biomass40_final), type="pearson")
abline(h = 0, lty = 2)  
plot(x=datx$mort_corrected, y=resid(mod_biomass40_final), type="pearson")
abline(h = 0, lty = 2) #
plot(x=datx$biomass_growth, y=resid(mod_biomass40_final), type="pearson")
abline(h = 0, lty = 2) 
plot(x=datx$recruitment_aspen_correct, y=resid(mod_biomass40_final), type="pearson")
abline(h = 0, lty = 2)

################## plot results of mort_mod
############# figure 6 
eff_mortM<-Effect(c("mort_corrected"),
                  mod=mod_biomass,
                  xlevels=list(mort_corrected=c(0,5,10,15,20,25,30,35,40,45,50)))
########
eff_mortM=as.data.frame(eff_mortM)
head(eff_mortM)
####
eff_mortG<-Effect(c("biomass_growth"),
                  mod=mod_biomass,
                  xlevels=list(biomass_growth=c(0,1,2,3,4,5,6,7,8,9,10)))
eff_mortG=as.data.frame(eff_mortG)
head(eff_mortG)
####
eff_mortR<-Effect(c("recruitment_aspen_correct"),
                  mod=mod_biomass,
                  xlevels=list(recruitment_aspen_correct=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
                                                           1.0,1.1,1.2,1.3,1.4,1.5)))
eff_mortR=as.data.frame(eff_mortR)
head(eff_mortR)
########
##########
library(ggplot2)
plot_mortM=ggplot()+  
  geom_point(data=caf_mortN13,aes(x=mort_corrected,y=ndvi.max7),shape=21,fill="firebrick3",size=2.5)+
  geom_ribbon(data=eff_mortM,aes(x=mort_corrected,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortM,aes(x=mort_corrected,y=fit),lwd=1,color="firebrick3")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.9)+
  labs(x="Aspen biomass mortality (Mg/ha)",y=expression(NDVI["max"]))+
  geom_text(aes(label = "(c)"), x = 0.2, y=0.905,lineheight = 2,fontface=2,size=5) 


plot_mortM
#ggsave(plot = plot_mortM, filename = "plot_mortM.pdf", path = "~/Documents/CAFI_all_final",
#    dpi = 600, width = 5, height = 4)


plot_mortG=ggplot()+
  geom_point(data=caf_mortN13,aes(x=biomass_growth,y=ndvi.max7),shape=21,fill="dodgerblue3",size=2.5)+
  geom_ribbon(data=eff_mortG,aes(x=biomass_growth,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortG,aes(x=biomass_growth,y=fit),lwd=1,color="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.9)+
  labs(x="Aspen biomass growth (Mg/ha)",y=expression(NDVI["max"]))+
  geom_text(aes(label = "(b)"),  x = 0.1, y=0.905,lineheight = 2,fontface=2,size=5) 

plot_mortG
#ggsave(plot = plot_mortG, filename = "plot_mortG.pdf", path = "~/Documents/CAFI_all_final",
#  dpi = 600, width = 5, height = 4)


plot_mortR=ggplot()+
  geom_point(data=caf_mortN13,aes(x=recruitment_aspen_correct,y=ndvi.max7),shape=21,
             fill="cadetblue3",size=2.5)+
  geom_ribbon(data=eff_mortR,aes(x=recruitment_aspen_correct,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortR,aes(x=recruitment_aspen_correct,y=fit),lwd=1,color="cadetblue3",linetype="dashed")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.9)+xlim(0,1.5)+
  labs(x="Aspen biomass recruitment (Mg/ha)",y=expression(NDVI["max"]))+
  geom_text(aes(label = "(a)"), x = 0.0, y=0.905,lineheight = 2,fontface=2,size=5) 

plot(plot_mortR)
#ggsave(plot = plot_mortR, filename = "plot_mortR.pdf", path = "~/Documents/CAFI_all_final",
# dpi = 600, width = 5, height = 4)

####
library(gridExtra)
library(gtable)
library(grid)
#grid.arrange(plot_mortR,plot_mortG,plot_mortM,ncol=3)
biomassALL=grid.arrange(arrangeGrob(plot_mortR,plot_mortG,plot_mortM,ncol=3,
                                    left = textGrob(expression(NDVI["max"]), rot = 90,hjust=0,vjust=2,
                                                    gp = gpar(fontsize = 14))))
ggsave(plot = biomassALL, filename = "biomassALL.jpeg",
       dpi = 600, width = 13, height = 5)
#########
###########
###############
###############  with one column
library(ggplot2)
plot_mortM=ggplot()+  
  geom_point(data=caf_mortN13,aes(x=mort_corrected,y=ndvi.max7),shape=21,fill="firebrick3",size=2.5)+
  geom_ribbon(data=eff_mortM,aes(x=mort_corrected,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortM,aes(x=mort_corrected,y=fit),lwd=1,color="firebrick3")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.91)+
  #labs(x=expression(Aspen~biomass~mortality~(Mg~ha^-1)))+
  labs(x="Aspen biomass mortality (Mg/ha)")+
  geom_text(aes(label = "(c)"), x = 0.19, y=0.91,lineheight = 2,fontface=2,size=4.5) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.1))

plot_mortM


plot_mortG=ggplot()+
  geom_point(data=caf_mortN13,aes(x=biomass_growth,y=ndvi.max7),shape=21,fill="dodgerblue3",size=2.5)+
  geom_ribbon(data=eff_mortG,aes(x=biomass_growth,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortG,aes(x=biomass_growth,y=fit),lwd=1,color="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.91)+
  labs(x="Aspen biomass growth (Mg/ha)")+
  #  labs(x=expression(Aspen~biomass~growth~(Mg~ha^-1)))+
  geom_text(aes(label = "(b)"),  x = 0.09, y=0.91,lineheight = 2,fontface=2,size=4.5) 

plot_mortG

plot_mortR=ggplot()+
  geom_point(data=caf_mortN13,aes(x=recruitment_aspen_correct,y=ndvi.max7),
             shape=21,fill="cadetblue3",size=2.5)+
  geom_ribbon(data=eff_mortR,aes(x=recruitment_aspen_correct,ymin=lower, 
                                 ymax = upper),
              alpha=0.25,linetype=0,na.rm = TRUE)+
  geom_line(data=eff_mortR,aes(x=recruitment_aspen_correct,y=fit),lwd=1,color="cadetblue3",linetype="dashed")+
  theme_bw()+
  theme(text = element_text(size=14,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+ylab("")+ylim(0.6,0.91)+xlim(0,1.5)+
  labs(x="Aspen biomass recruitment (Mg/ha)")+
  # labs(x=expression(Aspen~biomass~recruitment~(Mg~ha^-1)))+
  geom_text(aes(label = "(a)"), x = 0.0, y=0.91,lineheight = 2,fontface=2,size=4.5) 

plot(plot_mortR)

biomassALL2=grid.arrange(arrangeGrob(plot_mortR,plot_mortG,plot_mortM,ncol=1,
                                     left = textGrob(expression(NDVI["max"]), rot = 90,hjust=0,vjust=1.5,
                                                     gp = gpar(fontsize = 14))))
ggsave(plot = biomassALL2, filename = "biomassALL2.jpeg",
       dpi = 600, width = 5, height = 8)
