####
############ analysis: comparison of living and dying tree DBH and canopy position 
rm(list = ls()) # Clear out the workspace
library(dplR) # Dendrochronology Program Library in R
library(graphics)
library(utils)
library(stats)
library(nlme)
library(lme4)
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)
library(MuMIn)
library(effects)
## 
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
######### live trees
##########
###############
dataAB=read.csv("RRW_alive_NEW.csv",header=T) #####all trees ALIVE
#names(dataAB)[names(dataAB) == "ï..year"] <- "year"

longAB <- melt(setDT(dataAB), id.vars = c("year"), variable.name = "tree")
longAB2=subset(longAB,year>=1998)
names(longAB2)
names(longAB2)[names(longAB2) == "variable"] <- "tree"
names(longAB2)[names(longAB2) == "value"] <- "rrw"

#######
longAB2[] <- lapply(longAB2, function(x) if(is.factor(x)) factor(x) else x)

## convert mm to cm
longAB3=longAB2 %>%
  dplyr::mutate(rrw_cm=(rrw/10))
longAB3[is.na(longAB3)] <- 0
####sum ring width across all years from 1998-2015
longAB4=longAB3 %>%
  group_by(tree)%>%
  dplyr::summarise(sum_cm=sum(rrw_cm))

############### dead trees
dataD=read.csv("RRW_dead_ALL.csv",header=T) #####all trees dead
names(dataD)

#names(dataD)[names(dataD) == "ï..year"] <- "year"

longD <- melt(setDT(dataD), id.vars = c("year"), variable.name = "tree")
head(longD)
longD2=subset(longD,year>=1998)
names(longD2)
names(longD2)[names(longD2) == "variable"] <- "tree"
names(longD2)[names(longD2) == "value"] <- "rrw"

#######
longD2[] <- lapply(longD2, function(x) if(is.factor(x)) factor(x) else x)
str(longD2)

## convert mm to cm
longD3=longD2 %>%
  dplyr::mutate(rrw_cm=(rrw/10))

longD3[is.na(longD3)] <- 0
####sum ring width across all years from 1998-2015
longD4=longD3 %>%
  group_by(tree)%>%
  dplyr::summarise(sum_cm=sum(rrw_cm))

#trees that died before 1997
longD5=subset(longD4, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&
               tree!="TM6.61"& tree!="TM8.94")

#####
############ stack live and dead tree data frames
long_all=rbind(longAB4,longD5)

###############dbh dataframe
cafE=read.csv("CAFI_ENVIRO_new.csv")
head(cafE)
#names(cafE)[names(cafE) == "ï..tree"] <- "tree"

#########remove trees not used in analysis
cafE2=subset(cafE, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
               tree!="TM6.61"& tree!="TM8.94")###tree!="TM10.19"
names(cafE2)
head(cafE2)
#####
cafE2[] <- lapply(cafE2, function(x) if(is.factor(x)) factor(x) else x)
str(cafE2)
cafE2$tree=as.factor(cafE2$tree)
cafE2$site=as.factor(cafE2$site)
cafE2$status=as.factor(cafE2$status)
names(cafE2)

cafE3=cafE2[,c(1:4,5,7,11)]
names(cafE3)

df_all=merge(long_all,cafE3,by=c("tree"))
names(df_all)

###########
################# DBH of all trees in 1997
df_all2=df_all %>%
  dplyr::mutate(dbh2=(dbh-sum_cm))
########
############
data1=read.csv("CAFI_BAI.csv")
names(data1)

data2=data1%>% 
  rowwise()%>%
  mutate(log.bai = log1p(bai))  ###log1p keeps 0 RRW values at 0 

names(data2)
head(data2)
tail(data2)
#names(data2)[names(data2) == "ï..year"] <- "year"

##########
#trees that died before 1997
data2=subset(data2, tree!="TM3.61"& tree!="TM4.19"&tree!="TM7.61"&tree!="TM8.23"&
               tree!="TM5.19"& tree!="TM10.19"&tree!="TM7.19"&tree!="TM2.29"&
               tree!="TM6.29"& tree!="TM11.30"&tree!="TM1.61"&tree!="TM3.61"&
               tree!="TM6.61"& tree!="TM8.94")
names(data2)
str(data2)
data2[] <- lapply(data2, function(x) if(is.factor(x)) factor(x) else x)
#####
########### merge datF and cafE to get plot name for cafE2
datF.plot=subset(data2,year=="1996")
names(datF.plot)
head(datF.plot)
datF.plot2=datF.plot[,c(2,3,4)]
head(datF.plot2)
head(df_all2)
df_all2=as.data.table(df_all2)
invisible(df_all2[site == "199-201", site := "1067"])
invisible(df_all2[site == "233", site := "1079"])
invisible(df_all2[site == "298-300", site := "1101"])
invisible(df_all2[site == "307-309", site := "1104"])
invisible(df_all2[site == "91-93", site := "1031"])
invisible(df_all2[site == "94-96", site := "1032"])
invisible(df_all2[site == "97-99", site := "1033"])
invisible(df_all2[site == "61-63", site := "1021"])
str(df_all2)
str(datF.plot2)
datF.plot2$site=as.factor(datF.plot2$site)
datF.plot2$plot=as.factor(datF.plot2$plot)
datF.plot2$tree=as.factor(datF.plot2$tree)

df_all3=merge(datF.plot2,df_all2,by=c("tree","site"))
df_all3[] <- lapply(df_all3, function(x) if(is.factor(x)) factor(x) else x)

head(df_all3)
str(df_all3)

############################
############ mixed model comparing living and dying tree dbh -----TABLE S7
hist(df_all3$dbh2)
mod.dbh1=lme(log(dbh2)~status,
             random=~1|site/plot,data=df_all3,
             na.action=na.exclude,method = "ML")
summary(mod.dbh1)
plot(allEffects(mod.dbh1))
plot(mod.dbh1)

mod.dbh1B=lme(log(dbh2)~1,
              random=~1|site/plot,data=df_all3,
              na.action=na.exclude,method = "ML")

anova(mod.dbh1B,mod.dbh1)
model.sel(mod.dbh1B,mod.dbh1,rank = AICc)

###### FINAL MODEL
mod.dbh1=lme(log(dbh2)~status,
             random=~1|site/plot,data=df_all3,
             na.action=na.exclude,method = "REML")
summary(mod.dbh1)
plot(mod.dbh1)
qqnorm(resid(mod.dbh1))
plot(x=df_all3$site, y=resid(mod.dbh1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=df_all3$status, y=resid(mod.dbh1), type="pearson")
abline(h = 0, lty = 2) #
plot(x=df_all3$plot, y=resid(mod.dbh1), type="pearson")
abline(h = 0, lty = 2) #
####################
#################--------remove 54 outlier see if results change 
df_all3b=df_all3[-(54),]
###
mod.dbh2=lme(log(dbh2)~status,
             random=~1|site/plot,data=df_all3b,
             na.action=na.exclude,method = "REML")
summary(mod.dbh2) ##### SAME RESULT 
plot(mod.dbh2)
qqnorm(resid(mod.dbh2))
###############
##################
######### plot dbh
eff.dbh<- Effect(c("status"),
                 mod.dbh1)
eff.dbh=as.data.frame(eff.dbh)
head(eff.dbh)

##### function for drawing violing plot 
"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )

#######


pDBH2=ggplot()+geom_flat_violin(data=df_all3,aes(x=status,y=log(dbh),fill=status),
                                scale="count",alpha=0.4,trim=FALSE)+
  #position = position_nudge(x = .2, y = 0)+ ####raw data
  #geom_violin(data=cafE3b,aes(x=status,y=log(dbh),fill=status),trim = FALSE)+
  geom_errorbar(data=eff.dbh,aes(x=status,ymin  =  lower, ymax  =  upper),width =  0.09)+
  geom_point(data=eff.dbh,aes(status,fit),size=3,shape=21,
             fill="black")+
  #geom_boxplot(data=cafE3,aes(x=status,y=log10(dbh)),
  #   width = .1,  outlier.shape = NA, alpha = 0.5)+
  scale_fill_manual(values = c("dodgerblue1", "firebrick3"))+
  scale_x_discrete(labels=c("Living","Dying"))+
  theme_bw()+
  theme(text = element_text(size=15,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"),
        legend.position = "none")+
  xlab("Tree status")+ylab("DBH (ln(cm))")+  
  geom_dotplot(data=df_all3,aes(x=status,y=log(dbh),fill=status),binaxis = "y", dotsize = 0.8, stackdir = "down",
               binwidth = 0.08, position = position_nudge(-0.045))+
  scale_y_continuous(limits = c(1,4),
                     labels = scales::number_format(accuracy = 0.1),
                     breaks= scales::pretty_breaks(n=4))+
  geom_text(aes(label = "(a)"), x = 0.2, y=4,fontface=2,size=4.5) 


pDBH2

#########
################
### differences in canopy bewteen live and dead trees based on tree ring data 
####### ------------ TABLE S8 and Table S9
head(df_all3)
df_all3$canopy=as.factor(df_all3$canopy)
df_all3$canopy <- relevel(df_all3$canopy, ref="dom")
ggplot()+geom_point(data=df_all3, aes(x=canopy,y=status))
#########
####### logistc regressoin
m1 <- glmer(canopy~status + (1 | site/plot), 
            data = df_all3, 
            family = binomial(link = "logit"))
m10 <- glmer(canopy~1 + (1 | site/plot), 
            data = df_all3, 
            family = binomial(link = "logit"))

model.sel(m1,m10,rank = AICc) # m1
anova(m1,m10)

plot(allEffects(m1))
library(arm)
binnedplot(fitted(m1), 
           residuals(m1, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


df_cp=df_all3[,c(5,7)]
names(df_cp)

str(df_cp)
df_cp=na.omit(df_cp)
head(df_cp)
str(df_cp)

x.tab=chisq.test(x=df_cp$status,y=df_cp$canopy,correct = FALSE)
x.tab
caf.tab=table(df_cp)##p-value of less that 0.05 significance level.
caf.tab
names(caf.tab)
str(caf.tab)
#write.csv(caf.tab,file="caf.tab.csv")
########### 
caf.tab2=read.csv("caf.tab2.csv") ## just table w/counts 
head(caf.tab2)
str(caf.tab2)
caf.tab2$status=as.factor(caf.tab2$status)
caf.tab2$canopy=as.factor(caf.tab2$canopy)
caf.tab2$canopy<- factor(caf.tab2$canopy, levels = c("dom", "sub", "domD","subD"))

pCANO=ggplot(data=caf.tab2,aes(fill=canopy,y=N,x=factor(status,levels=c("Living","Dying")))) + 
  geom_bar(position = "stack",stat="identity",width=0.5)+
  
  scale_fill_manual(values = c('dodgerblue1','cadetblue1', 'firebrick3','#FF9999'), ##snow4
                    labels = c('Upper (living)', 'Lower (living)','Upper (dying)', 'Lower (dying)'))+
  # name = 'Canopy Position') +
  xlab('Tree Status') +
  theme_bw()+
  theme(text = element_text(size=15,family="Helvetica",color="black"),
        axis.text=element_text(size=13,family="Helvetica",color="black"))+
  theme(legend.position = c(0.82,0.9),
        legend.direction="vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_blank())+
  xlab("Tree status")+ylab("Count")+ylim(0,100)+
  geom_text(aes(label = "(b)"), x = 0.53, y=100,size=4.5,fontface=2) 

pCANO
#ggsave(plot = pCANO, filename = "canopy_treeRings.pdf", path = "~/Documents/CAFI_all_final",
#  dpi = 600, width = 6, height = 4)

library(ggpubr)

figDC=ggarrange(pDBH2+rremove("xlab"),pCANO+rremove("xlab"), # remove xaxis include in annoate
                ncol = 2, nrow =1)
figDC
# maybe change the light blue color???
### make side by side instead so i can have legend for bottom figure on top 
#labels = c("(a)", "(b)", "(c)","(d)"),vjust = -0.02)

############ ----------- figure 2
figDC2=annotate_figure(figDC,
                       bottom = text_grob("Tree Status",size=15))
figDC2

ggsave(plot = figDC2, filename = "fig_DC2.jpeg", 
       dpi = 600, width = 9, height = 4)





