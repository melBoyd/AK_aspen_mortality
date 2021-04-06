rm(list = ls())
##########
################### Figure 3 code

setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")

##########
my_palette <- colorRampPalette(c("red", "lightgrey", "purple2"))(n = 299)
col_breaks = c(seq(-1,0,length=100), # for red
               seq(0,0.1,length=100),  # for yellow
               seq(0.1,0.2,length=100)) # for green

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}


data2 <- read.csv("TEST_climALIVE.csv")
str(data2)

breaks <- c(-0.2,0,0.2)
data2$climate.variable <- factor(data2$climate.variable, levels=c("PGS CMI","PGS temp","FW CMI","FW temp",
                                                                  "Spring CMI","Spring temp","GS CMI",
                                                                  "GS temp"))
library(ggplot2)
head(data2)
names(data2)[names(data2) == "ï..time.period"] <- "time.period"
hm_alive=ggplot(data2, aes(time.period, climate.variable)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 3)),size=2.5) +
  scale_fill_gradientn(colours = c("red","white","purple2"),
                         breaks = breaks)+
  theme_bw()+
  labs(y="",x="")+
  theme(text = element_text(size=4,family="Helvetica",color="black"),
        axis.text=element_text(size=8,family="Helvetica",color="black"),
        legend.title = element_blank(),
        legend.position = "none")+
  ggtitle("(a)")+
  theme(plot.title = element_text(hjust = -0.1,vjust = -0.1,size = 10,face = "bold",family="Helvetica"))
hm_alive

##### dead trees
data3 <- read.csv("TEST_climDEAD.csv")
str(data3)
  
breaks <- c(-0.2,0,0.2)
data3$climate.variable <- factor(data3$climate.variable, levels=c("PGS CMI","PGS temp","FW CMI","FW temp",
                                                                    "Spring CMI","Spring temp","GS CMI",
                                                                    "GS temp"))

names(data3)[names(data3) == "ï..time.period"] <- "time.period"
hm_dead=ggplot(data3, aes(time.period, climate.variable)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 3)),size=2.5) +
    scale_fill_gradientn(colours = c("red","white","purple2"),
                         breaks = breaks)+
    theme_bw()+
    labs(y="",x="")+
    theme(text = element_text(size=4,family="Helvetica",color="black"),
          axis.text=element_text(size=8,family="Helvetica",color="black"),
          legend.title = element_blank(),
          legend.position = "none")+
  ggtitle("(b)")+
  theme(plot.title = element_text(hjust = -0.1,vjust = -0.1,size = 10,face = "bold",family="Helvetica"))
  
hm_dead

####
library(gridExtra)
library(gtable)
library(grid)
library(ggpubr)
hm_all=grid.arrange(arrangeGrob(hm_alive,hm_dead,ncol=2),
                                left = textGrob("Climate variable", rot = 90,hjust=0.25,vjust=2,
                                                gp = gpar(fontsize = 10,
                                                          fontfamily = "Helvetica")),
                    bottom = textGrob("Time period",hjust=0,vjust=-1,
                                       gp = gpar(fontsize = 10,
                                         fontfamily = "Helvetica")))

hm_all


ggsave(plot = hm_all, filename = "Figure_3.jpeg", 
       dpi = 300, width = 6, height = 4,units="in")





