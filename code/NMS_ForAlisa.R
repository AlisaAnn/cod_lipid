#### NMS Ordinations by IRI (August Pcod)
### For Alisa from Hillary, sent March 2022. most updated version
### Assuming you already have your species and environmental matrices set up. Here, I read in a data file with both the species and environmental data, and then I separated them out in R


#### Import Files ####
AugOrd <- read.csv("data/AugustPreyOrd_ByIRI.csv")
head(AugOrd)

#### Load Libraries ####
library('vegan')
library("MASS")
library("labdsv")
library("indicspecies")
library("tidyverse")
library("ggrepel")
library("goeveg")
library("corrplot")
library("rcartocolor")
library("dplyr")
library("tidyr")
library("ggeasy")


#### Create Environmental and Species Matrices for August ####
AugPreyData <- AugOrd[,c(35:49)]
head(AugPreyData)

AugEnvData <- AugOrd[,c(4:34)]
head(AugEnvData)


#Check Scree Plot (determine dimensions of NMS)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(AugPreyData,distance = "bray",k = 6,trymax = 200,autotransform = FALSE)
#looks like 3 dimensions is best

#### Run the August NMS Ordination ####
AugMDS<-metaMDS(AugPreyData, distance="bray", k=3, trymax=200, autotransform=FALSE)


## Set up your three NMS Principal Axes 
NMDS1 <- AugMDS$points[,1]
NMDS2 <- AugMDS$points[,2]
NMDS3 <- AugMDS$points[,3]

#Add them to the ordination data plot
Augpcod.plot<-cbind(AugOrd, NMDS1, NMDS2, NMDS3)
head(Augpcod.plot)


#### Plot the NMS in Base R ###
#quartz()
plot(AugMDS, type = 't', display = c('species'))

#### Plot the NMS in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#quartz()
Aug <- ggplot(data=Augpcod.plot, aes(NMDS1, NMDS2))+
  geom_point(data=Augpcod.plot, aes(NMDS1, NMDS2, color=Regime), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Regime, color = Regime), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 



## Look at the effects of environmental variables on axes
#Continuous Environmental Variables + Factors
AugEnvDataConFact <- AugOrd[,c(4, 7,8,10:14,18,22,26:31,33,34)]
head(AugEnvDataConFact)
en <- envfit(AugMDS, AugEnvDataConFact, permutations = 999, na.rm = T)

#Only continuous environmental variables
AugEnvDataCont <- AugOrd[,c(8, 10, 14, 18, 22, 28:31, 34)]
head(AugEnvDataCont)
en1 <- envfit(AugMDS, AugEnvDataCont, permutations = 999, na.rm = T)

#Subset data with envfit R^2 > 0.2
#Restructure dataframe
AugEnvDataCut <- AugOrd[,c(8, 14, 28:31)]
head(AugEnvDataCut)
names(AugEnvDataCut)[1]<-paste("TridentTemp")
names(AugEnvDataCut)[2]<-paste("Temp_(YSI)")
names(AugEnvDataCut)[3]<-paste("SL_(mm)")
names(AugEnvDataCut)[4]<-paste("WetWeight_(g)")
names(AugEnvDataCut)[5]<-paste("Length-Weight_Condition")
names(AugEnvDataCut)[6]<-paste("HSI_Condition")
head(AugEnvDataCut)

en2 <- envfit(AugMDS, AugEnvDataCut, permutations = 999, na.rm = T)
#### Get the vectors the correct length
en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)


#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()
plot(AugMDS, type = 't', display = c('species'))
plot(en2)

#### Plot the NMS with environmental variable overlay in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
quartz()
Aug_Env <- ggplot(data=Augpcod.plot, aes(NMDS1, NMDS2))+
  geom_point(data=Augpcod.plot, aes(NMDS1, NMDS2, color=Regime), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Regime, color = Regime), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = en_coord_cont, size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(en_coord_cont), position=position_jitter(.01))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 

### Set up Prey joint-plot 
en_prey <- envfit(AugMDS, AugPreyData, permutations = 999, na.rm = T)
head(AugPreyData)
# Make a subset of Prey Variables with R^2 > 0.1
AugPrey_cut <- AugPreyData[,c(1, 4, 6,7, 10, 13 )]
head(AugPrey_cut)
en_prey_cut <- envfit(AugMDS, AugPrey_cut, permutations = 999, na.rm = T)
prey_coord = as.data.frame(scores(en_prey_cut, "vectors")) * ordiArrowMul(en_prey_cut)

#### Plot the NMS in ggplot with prey vector overlay ####
#quartz()
Aug_prey <- ggplot(data=Augpcod.plot, aes(NMDS1, NMDS2))+
  geom_point(data=Augpcod.plot, aes(NMDS1, NMDS2, color=Regime), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Regime, color = Regime), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.25))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
print(Aug_prey)


#### MRPP By Regime ####
mrpp(AugPreyData, AugEnvData$Regime, distance = 'bray', weight = 3)

### ISA By Regime ####
indval = multipatt(AugPreyData, AugEnvData$Regime, control = how(nperm=999))
summary(indval)

