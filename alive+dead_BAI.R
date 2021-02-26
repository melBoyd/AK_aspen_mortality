###
######
############ TREE BAI
rm(list = ls()) # Clear out the workspace
library(dplR) # Dendrochronology Program Library in R
library(graphics)
library(utils)
library(stats)
## 
setwd("~/CAFI_R_April2020/CAFI_R_April2020/CAFI_R_Nov2020")
######### live trees
###############
dataAB=read.csv("RRW_alive_NEW.csv",header=T,row.names = 1) #####all trees ALIVE
bar.aliveB <- bai.out(rwl = dataAB) ### BAI of live trees
#####
########### 
bar.crnB <- chron(bar.aliveB)
bar.crn.newB=bar.crnB[c(28:124),]
yr <- as.numeric(rownames(bar.crn.newB))

plot(yr, bar.crn.newB[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yr, bar.crn.newB[, 1], col = "black", lty = "dashed")
lines(yr, ffcsaps(bar.crn.newB[, 1], nyrs = 32), col = "red", lwd = 2)

#######
############### dead trees
dataD=read.csv("RRW_dead_ALL.csv",header=T,row.names = 1) #####all trees dead
names(dataD)
barDB <- bai.out(rwl = dataD)
####
bar.crnDB <- chron(barDB)
bar.crnDB.new=bar.crnDB[c(20:113),]
yr <- as.numeric(rownames(bar.crnDB.new))
plot(yr, bar.crnDB.new[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yr, bar.crnDB.new[, 1], col = "black", lty = "dashed")
lines(yr, ffcsaps(bar.crnDB.new[, 1], nyrs = 32), col = "red", lwd = 2)

write.csv(barDB,file="baiDEAD.csv")
write.csv(bar.aliveB,file="baiALIVE.csv")