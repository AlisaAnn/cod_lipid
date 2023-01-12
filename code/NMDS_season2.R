#### NMS Ordinations by % weight of prey item in stomach
###each stomach adds to 100%. all empty somachs removed
### based on the code sent from Hillary, March 2022. 
#this is updated w more wgt categories than done in march 22
#today is jan 11 2023

#### Import Files ####
prey_wgt <- read_csv("data/diet_wgt_data_new.csv")
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
prey_wgts <- prey_wgt[,c(35:47)]
head(prey_wgts)

preyEnvData <- prey_wgt[,c(1:20)]
head(preyEnvData)


#Check Scree Plot (determine dimensions of NMS)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(prey_wgts,distance = "bray",k = 6,trymax = 300,autotransform = FALSE)
##if not able to get convergence w k=6 and trymax = 200 could will reduce prey items
#omit euphasids and omit mysids. also omit 3 rows where they were only prey item so no zeros

prey_w2 <- read_csv("data/diet_wgt_data_new2.csv")
head(prey_w2)
tail(prey_w2) #to make sure no empty rows at bottom of file
prey_wgts2 <- prey_w2[,c(36:46)]
head(prey_wgts2)
preyEnvData <- prey_w2[,c(1:21)]
head(preyEnvData)

dimcheckMDS(prey_wgts2,distance = "bray",k = 6,trymax = 200,autotransform = FALSE)


#### Run the NMS Ordination dimensions ####
prey_wgtMDS<-metaMDS(prey_wgts2, distance="bray", k=3, trymax=300, autotransform=FALSE)


## Set up your three NMS Principal Axes 
NMDS1 <- prey_wgtMDS$points[,1]
NMDS2 <- prey_wgtMDS$points[,2]
NMDS3 <- prey_wgtMDS$points[,3]

#Add them to the ordination data plot
codprey.plot<-cbind(prey_w2, NMDS1, NMDS2, NMDS3)
head(codprey.plot)

##AA tries to save file w NMDS 3 dimensions. 
write.table(codprey.plot, file = "output/nmds_results_new.csv")

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

Wgt_plot <- ggplot(data=codprey.plot, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot, aes(NMDS1, NMDS2, color=Month), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Month, color = Month), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") 
plot(Wgt_plot)

## Look at the effects of environmental variables on axes
##See if can figure out how to test if months explain var in axes instead of season
#Continuous Environmental Variables + Factors
#AugEnvDataConFact <- AugOrd[,c(4, 7,8,10:14,18,22,26:31,33,34)]
#head(AugEnvDataConFact)
#en <- envfit(AugMDS, AugEnvDataConFact, permutations = 999, na.rm = T)

#Only continuous environmental variables
head(preyEnvData)
preyEnvDataCont <- preyEnvData[,c(7, 8, 9, 10, 18, 20, 21)]
head(preyEnvDataCont)
en1 <- envfit(prey_wgtMDS, preyEnvDataCont, permutations = 999, na.rm = T)
#so now look at en1 and based on en1
head(en1)
#Subset data with envfit R^2 > 0.2 based on list generated in head(en1)
#Restructure dataframe to have only Day of Year and TL
preyEnvDataCut <- preyEnvDataCont[,c(1,4)]

names(preyEnvDataCut)[1]<-paste("Day of Year")
names(preyEnvDataCut)[2]<-paste("TL (mm)")
head(preyEnvDataCut)

en2 <- envfit(prey_wgtMDS, preyEnvDataCut, permutations = 999, na.rm = T)
head(en2)
#### Get the vectors the correct length
en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)

#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()  this is used w macOS system
plot(prey_wgtMDS, type = 't', display = c('species'))
plot(en2)

#### Plot the NMS with environmental variable overlay in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

Cod_Env <- ggplot(data=codprey.plot, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Season, color = Season), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = en_coord_cont, size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(en_coord_cont), position=position_jitter(.01))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
plot(Cod_Env)


### Set up Prey joint-plot to overlay prey names on Season elipses
en_prey <- envfit(prey_wgtMDS, prey_wgts2, permutations = 999, na.rm = T)
head(prey_wgts2)

# Make a subset of Prey Variables with R^2 > 0.1 that are important to final NMDS by season
#first make the plot of env var and species

plot(prey_wgtMDS, type = 't', display = c('species'))
plot(en2)
#see what fits pattern. for example, column 3 is polycheate 
#cut gastropod and calanoid
preywgt_cut <- prey_wgts2[,c(1, 2, 3, 5, 6, 7, 9, 10, 11)]
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
mrpp(prey_wgts2, preyEnvData$Season, distance = 'bray', weight = 3)

### ISA By Season ####
indval = multipatt(prey_wgts2, preyEnvData$Season, control = how(nperm=999))
summary(indval)

