#### NMS Ordinations by % weight of prey item in stomach
###each stomach adds to 100%. all empty somachs removed
### based on the code sent from Hillary, March 2022. most updated version

#### Import Files ####
prey_wgt <- read_csv("data/prey_wgt_data_num.csv")
head(prey_wgt)
tail(prey_wgt) #to make sure no empty rows at bottom of file


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


#### Create Environmental and Species Matrices ####
prey_wgts <- prey_wgt[,c(5:14)]
head(prey_wgts)

preyEnvData <- prey_wgt[,c(1:4)]
head(preyEnvData)


#Check Scree Plot (determine dimensions of NMS)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(prey_wgts,distance = "bray",k = 6,trymax = 200,autotransform = FALSE)
#looks like 3 dimensions is best based on plot bc stress <0.1 is great

#### Run the NMS Ordination ####
prey_wgtMDS<-metaMDS(prey_wgts, distance="bray", k=3, trymax=200, autotransform=FALSE)


## Set up your three NMS Principal Axes 
NMDS1 <- prey_wgtMDS$points[,1]
NMDS2 <- prey_wgtMDS$points[,2]
NMDS3 <- prey_wgtMDS$points[,3]

#Add them to the ordination data plot
codprey.plot<-cbind(prey_wgt, NMDS1, NMDS2, NMDS3)
head(codprey.plot)
##AA tries to save file w NMDS 3 dimensions. no success
##saveRDS(Wgt_prey,file ="output/nmds_wgt_results.csv")

#### Plot the NMS in Base R ###

plot(prey_wgtMDS, type = 't', display = c('species'))

#### Plot the NMS in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

Wgt_plot <- ggplot(data=codprey.plot, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Season, color = Season), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
plot(Wgt_plot)

## Look at the effects of environmental variables on axes
#Continuous Environmental Variables + Factors
#AugEnvDataConFact <- AugOrd[,c(4, 7,8,10:14,18,22,26:31,33,34)]
#head(AugEnvDataConFact)
#en <- envfit(AugMDS, AugEnvDataConFact, permutations = 999, na.rm = T)

#Only continuous environmental variables
#AugEnvDataCont <- AugOrd[,c(8, 10, 14, 18, 22, 28:31, 34)]
#head(AugEnvDataCont)
#en1 <- envfit(AugMDS, AugEnvDataCont, permutations = 999, na.rm = T)

#Subset data with envfit R^2 > 0.2
#Restructure dataframe
#AugEnvDataCut <- AugOrd[,c(8, 14, 28:31)]
#head(AugEnvDataCut)
#names(AugEnvDataCut)[1]<-paste("TridentTemp")
#names(AugEnvDataCut)[2]<-paste("Temp_(YSI)")
#names(AugEnvDataCut)[3]<-paste("SL_(mm)")
#names(AugEnvDataCut)[4]<-paste("WetWeight_(g)")
#names(AugEnvDataCut)[5]<-paste("Length-Weight_Condition")
#names(AugEnvDataCut)[6]<-paste("HSI_Condition")
#head(AugEnvDataCut)

#en2 <- envfit(AugMDS, AugEnvDataCut, permutations = 999, na.rm = T)
#### Get the vectors the correct length
#en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)


#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()
#plot(AugMDS, type = 't', display = c('species'))
#plot(en2)

#### Plot the NMS with environmental variable overlay in ggplot #####

#safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
#                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#quartz()
#Aug_Env <- ggplot(data=Augpcod.plot, aes(NMDS1, NMDS2))+
 # geom_point(data=Augpcod.plot, aes(NMDS1, NMDS2, color=Regime), show.legend=F, position=position_jitter(.1))+
#  stat_ellipse(aes(fill=Regime, color = Regime), alpha=.25,type='t',size =1, geom="polygon")+ 
 # theme_classic()+
  #scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  #geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = en_coord_cont, size = 1, alpha = 0.5, colour = "black") +
  #geom_text(data = en_coord_cont, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(en_coord_cont), position=position_jitter(.01))+
  #theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 

### Set up Prey joint-plot to overlay prey names on Season elipses
en_prey <- envfit(prey_wgtMDS, prey_wgts, permutations = 999, na.rm = T)
head(prey_wgts)
# Make a subset of Prey Variables with R^2 > 0.1 that are important to final NMDS by season
#for example, column 1 is polycheate and column 6 is shrimp
preywgt_cut <- prey_wgts[,c(1, 3, 6,7, 9, 10)]
head(preywgt_cut)
en_prey_cut <- envfit(prey_wgtMDS, preywgt_cut, permutations = 999, na.rm = T)
prey_coord = as.data.frame(scores(en_prey_cut, "vectors")) * ordiArrowMul(en_prey_cut)

#### Plot the NMS in ggplot with prey vector overlay for Month ####
head(codprey.plot)
Wgt_prey <- ggplot(data=codprey.plot, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot, aes(NMDS1, NMDS2, color=Month), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Month, color = Month), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
#  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
#  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.25))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
print(Wgt_prey)

#### Plot the NMS in ggplot with prey vector overlay for Season ####
Wgt_prey <- ggplot(data=codprey.plot, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Season, color = Season), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.25))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
print(Wgt_prey)

#### MRPP By Season ####
mrpp(prey_wgts, preyEnvData$Season, distance = 'bray', weight = 3)

### ISA By Season ####
indval = multipatt(prey_wgts, preyEnvData$Season, control = how(nperm=999))
summary(indval)

