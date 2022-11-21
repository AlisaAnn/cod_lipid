##Hillary diet ordinations from 2019 script
##In R, loaded the packages vegan, mass, labdsv,indicspecies,tidyverse,ggrepel

#The downloaded binary packages are in
#C:\Users\alisa\AppData\Local\Temp\RtmpkPaGOw\downloaded_packages


 # Ordination Prey Plots for Pacific Cod (2008,  2018, 2015)
  ######################
#Purpose: To create ordination graphs for prey groupings for 2008, 2015, 2018, for August and 
#run an MRPP and ISA statistical analysis
#Created: 1.13.2012
#Update 4.21.2020

############### Set Working Directory ################

setwd("~/Documents/Pacific Cod MS/Data/Stomach Contents Analysis")

################ Import Files ######################
OrdinationFull <- read.csv("PcodLogTransform.csv") 
#Data file has already been log transformed and had rare species (< 5% of total abundance), 
  #removed (using PC-ORD because R struggles with verifying NMS assumptions).  
  #This helps us satisfy the assumptions for the NMS ordination

head(OrdinationFull) 


############### Load Libraries ###################
library('vegan')
library("MASS")
library("labdsv")
library("indicspecies")
library("tidyverse")
library("ggrepel")


############## NMDS for August, 2008, 2015, 2018 ####################
#Subset only columns with prey information
Preydata<-OrdinationFull[,c(10:26)]
head(Preydata)
#Subset only columns with environmental information
Environmentdata<-OrdinationFull[,c(3:9)]
head(Environmentdata)
unique(Environmentdata$Year)

#Run a Non-Metric Multidimensional Scaling ordination with a Bray-Curtis distance measure. 

ord<-metaMDS(Preydata, k=2, distance = "bray", trymax=200, autotransform=F)


############## NMDS plotting for all years (In base R) ###########

palette(c("blue", "red3", "grey40"))
#Blue 2008, Red 2015, Grey 2018
quartz() ### windows() for PC
plot(ord, type='n', xlim=c(), ylim=c(),las=1)
points(ord, display = 'sites', cex = 1, pch= 19 , col= Environmentdata$YearSymbol)
text(ord, display = 'spec', cex= .8, lwd=1, col="black")


#Add ellipses of the standard deviation.  
#We are superimposing year onto our plot to see how similar different years are in terms of prey community 
#composition
ordiellipse(ord, Environmentdata$Year, show.groups='2008',col="blue", lwd=1.5, cex=1.5,  label=T)
ordiellipse(ord, Environmentdata$Year, show.groups='2015',col="red3", lwd=1.5, cex=1.5,  label=T, pos=4)
ordiellipse(ord, Environmentdata$Year, show.groups='2018',col="grey40", lwd=1.5, cex=1.5,  label=T, pos=4)
0

#####################################################################
######## Redo the NMS Ordination with prey as vectors in ggplot (prettier graphs!) ##########   

#Run the NMS ordination again          
pcodMDS<-metaMDS(Preydata, distance="bray", k=2, trymax=200, autotransform=FALSE)

## Set up your two NMS Principal Axes 
NMDS1 <- pcodMDS$points[,1]
NMDS2 <- pcodMDS$points[,2]
#Add them to the ordination data plot
pcod.plot<-cbind(Ordination, NMDS1, NMDS2)
head(pcod.plot)

#Add arrows of species with p vals <= 0.01
fit<-envfit(pcodMDS, Preydata)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$FG <- rownames(arrow)
arrow.p<-filter(arrow, P<= 0.01)
head(arrow.p)
unique(arrow$FG)

#Plot ordination with vector species values using ggplot and elipses of the 95% confidence intervals
quartz() ### windows() for PC
ggplot(data=pcod.plot, aes(NMDS1, NMDS2))+
  geom_point(data=pcod.plot, aes(NMDS1, NMDS2, color=YearSymbol), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=YearSymbol), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_fill_manual(values=c("blue", "red", "grey1")) +
  scale_color_manual(values=c("lightblue", "salmon", "grey80", "white")) +
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2,  lty = FG, color = "white"), size = 1.25, 
               lty = 1, arrow=arrow(length=unit(.75, "cm")*arrow.p$R)) +
  geom_text_repel(data=arrow.p, aes(x=NMDS1, y=NMDS2, label= FG), size=4, position=position_jitter(.25)) +
  annotate("text", x = -1.3, y=1.5, label = "2018", size = 6, colour = "grey30") +
  annotate("text", x = -1.7, y=-0.95, label = "2008", size = 6, colour = "blue") +
  annotate("text", x = 1.55, y=1.2, label = "2015", size = 6, colour = "red") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) +
  theme(legend.position="none")

#### Statistics ####

#### MRPP by year (Testing the null hypothesis: all years have the same prey community composition)

mrpp(Preydata, Environmentdata$Year, distance = 'bray', weight = 3)

### ISA by year
class(Environmentdata$Year)
Year.vector <-as.vector(Environmentdata$Year)
indval = multipatt(Preydata, Year.vector, control = how(nperm=999))
summary(indval)


