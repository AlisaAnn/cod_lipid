#### NMS Ordinations by % weight of prey item in stomach
###each stomach adds to 100%. all empty somachs removed
### based on the code sent from Hillary, March 2022. 
#this is updated w more wgt categories than done in march 22
#today is Feb 1 2023  The 4 after the prey name, means the data are cube root.
# what is differnet about this script, is I focus only on NMDS 1 and 2 (not 3).


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

#### Import Files ####
prey_wgt <- read_csv("data/diet_wgt_data_new22.csv")
head(prey_wgt)
#notice 11 prey groups. mysids and euphasids removed
tail(prey_wgt) #to make sure no empty rows at bottom of file

#### Create Environmental and Species Matrices ####
##removed mysids (only at 3 stations in 2020) and euphasids (only december 2022)
prey_wgts <- prey_wgt[,c(37:47)]
head(prey_wgts)

preyEnvData <- prey_wgt[,c(1:21)]
head(preyEnvData)


#Check Scree Plot (determine dimensions of NMS)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(prey_wgts,distance = "bray",k = 6,trymax = 300,autotransform = FALSE)
##using trymax = 300 instead of trymax = 200, I was able to use all 11 prey groups! wow!


#### Run the NMS Ordination dimensions k = # dimensions####
prey_wgtMDS<-metaMDS(prey_wgts, distance="bray", k=3, trymax=300, autotransform=FALSE)

## Set up your three NMS Principal Axes 
NMDS1 <- prey_wgtMDS$points[,1]
NMDS2 <- prey_wgtMDS$points[,2]
NMDS3 <- prey_wgtMDS$points[,3]

#Add them to the ordination data plot
codprey.plot3<-cbind(prey_wgt, NMDS1, NMDS2, NMDS3)
head(codprey.plot3)
str(codprey.plot3)
codprey.plot3 <- codprey.plot3 %>%
  mutate(Year_fac = as.factor(Year))

##save file w NMDS 3 dimensions. 
write.csv(codprey.plot3, file = "output/nmds_results_new22.csv")

#### Plot the NMS in Base R ###

plot(prey_wgtMDS, type = 't', display = c('species'))
##this shows clearly that (-) NMDS 1 and (-) NMDS 2 is calanoid and polychaete


## Alisa diverge from HT code to see what variables relate to NMDS axes
## want more detailed scatters to see how the axes are related to species
## go thru each of next 3 plots and change yaxis to diff species and diff environ variables

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Harp4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Gammarid4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Poly4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = calanoid4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = crab4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Caprellidae4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Isopod4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = gastropod4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Corphiidae4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = Shrimp4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot2 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS2, y = cumacea4, color = Month)) +
  geom_point()+
  xlab("NMDS 2")+
  theme_minimal()
plot2 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Harp4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Gammarid4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Poly4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = calanoid4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = crab4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Caprellidae4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Isopod4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = gastropod4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Corphiidae4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Shrimp4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

plot3 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = cumacea4, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3 

#### Plot the NMS in ggplot ##
###  back to Hillary script####
##above scatters might be premature because not sure what's significant yet.

### Look at the effects of environmental variables on axes
##Continuous varables + Factors
head(preyEnvData)

#isolate factors
preyEnvDataFact <- preyEnvData[,c(4, 5, 6, 7)]
head(preyEnvDataFact)
en <- envfit(prey_wgtMDS, preyEnvDataFact, permutations = 999, na.rm = T)
head(en)

#Only continuous environmental variables
head(preyEnvData)
preyEnvDataCont <- preyEnvData[,c(8, 9, 10, 11)]
head(preyEnvDataCont)
names(preyEnvDataCont)[1]<-paste("Day of Year")
names(preyEnvDataCont)[2]<-paste("Temp")
names(preyEnvDataCont)[3]<-paste("Salinity")
names(preyEnvDataCont)[4]<-paste("TL (mm)")
head(preyEnvDataCont)

en1 <- envfit(prey_wgtMDS, preyEnvDataCont, permutations = 999, na.rm = T)
head(en1)

#### Get the vectors the correct length
en_coord_cont = as.data.frame(scores(en1, "vectors")) * ordiArrowMul(en1)

#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()  this is used w macOS system
plot(prey_wgtMDS, type = 't', display = c('species'))
plot(en1)
ggsave("./Figs/NMDS12_enfactors.png", width = 6, height = 6, units = 'in')

#so now look at en1 and based on en1
#Subset data with envfit R^2 > 0.2 based on list generated in head(en1)
#Restructure dataframe to have only Day of Year and TL
#preyEnvDataCut <- preyEnvDataCont[,c(2, 3, 4)]

#names(preyEnvDataCut)[1]<-paste("Temperature")
#names(preyEnvDataCut)[2]<-paste("Salinity")
#names(preyEnvDataCut)[3]<-paste("TL (mm)")
#head(preyEnvDataCut)

#en2 <- envfit(prey_wgtMDS3, preyEnvDataCut, permutations = 999, na.rm = T)
#head(en2)

#### Get the vectors the correct length
#en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)


#### Plot the NMS with environmental variable overlay in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

Cod_Env <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=SeasonA), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=SeasonA, color = SeasonA), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = en_coord_cont, size = 1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(en_coord_cont), position=position_jitter(.01))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
plot(Cod_Env)


### Set up Prey joint-plot to overlay prey names on Season elipses
en_prey <- envfit(prey_wgtMDS, prey_wgts, permutations = 999, na.rm = T)
head(prey_wgts)
head(en_prey)

# Make a subset of Prey Variables with R^2 > 0.1 that are important to final NMDS by season
#select the species that have R^2 with significance
en_prey1 <- envfit(NMDS1, prey_wgts, permutations = 999, na.rm = T)
head(prey_wgts)
head(en_prey1)

en_prey2 <- envfit(NMDS2, prey_wgts, permutations = 999, na.rm = T)
head(en_prey2)

en_prey3 <- envfit(NMDS3, prey_wgts, permutations = 999, na.rm = T)
head(en_prey3)
#first make the plot of env var and species
head(prey_wgts)
preywgt_cut <- prey_wgts[,c(1, 2, 3, 6, 10)]
head(preywgt_cut)
preywgt_cut <- rename(preywgt_cut,"Harp" = "Harp4")
preywgt_cut <- rename(preywgt_cut,"Gamm" = "Gammarid4")
preywgt_cut <- rename(preywgt_cut,"Poly" = "Poly4")
preywgt_cut <- rename(preywgt_cut,"Caprellidae" = "Caprellidae4")
preywgt_cut <- rename(preywgt_cut,"Shrimp" = "Shrimp4")

head(preywgt_cut)

en_prey_cut <- envfit(prey_wgtMDS, preywgt_cut, permutations = 999, na.rm = T)
head(en_prey_cut)
prey_coord = as.data.frame(scores(en_prey_cut, "vectors")) * ordiArrowMul(en_prey_cut)

#### Plot the NMS in ggplot with prey vector overlay for Month ####
head(codprey.plot3)
Wgt_prey <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Month), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Month, color = Month), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.25))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
print(Wgt_prey)

#### Plot the NMS in ggplot with prey vector overlay for Season ####
Wgt_prey <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Season, color = Season), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.15))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
print(Wgt_prey)
ggsave("./figs/nmds_species.png", width = 6, height = 4, units = 'in')

### with August into winter category
##March 23, 2023 decided to use this plot
Wgt_preyA <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=SeasonA), show.legend=T, position=position_jitter(.1))+
  stat_ellipse(aes(fill=SeasonA, color = SeasonA), alpha=.2,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  #scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  #scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = prey_coord, size = 1, alpha = 0.5, colour = "black", arrow = arrow()) +
  geom_text(data = prey_coord, aes(x=NMDS1, y = NMDS2), colour = "black", fontface = "bold", label = row.names(prey_coord), position=position_jitter(0.325))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), legend.title = element_blank()) 
print(Wgt_preyA)
ggsave("./figs/nmds_species_seasonA1.png", width = 6, height = 5, units = 'in')

#### MRPP By Season ####
mrpp(prey_wgts, preyEnvData$Season, distance = 'bray', weight = 3)
#### MRPP By SeasonA ####
mrpp(prey_wgts, preyEnvData$SeasonA, distance = 'bray', weight = 3)

### ISA By Season ####
indval = multipatt(prey_wgts, preyEnvData$Season, control = how(nperm=999))
summary(indval)
### ISA By SeasonA ####
indval = multipatt(prey_wgts, preyEnvData$SeasonA, control = how(nperm=999))
summary(indval)
##3-23-2023 can stop here as do not use plots after this.

#### MRPP By Month ####
mrpp(prey_wgts, preyEnvData$Month, distance = 'bray', weight = 3)

### ISA By Month ####
indval = multipatt(prey_wgts, preyEnvData$Month, control = how(nperm=999))
summary(indval)


# now plot sig variables in scatter FIGURE only for NMDS1-3
library(ggplot2)
library("ggpubr")
library(purrr)

P11 <- ggplot(codprey.plot3, aes(NMDS1, Poly4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Polychaete", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P11)

P12 <- ggplot(codprey.plot3, aes(NMDS1, Caprellidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Caprellidae", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P12)

P13 <- ggplot(codprey.plot3, aes(NMDS1, Harp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Harpacticoid", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P13)

P14 <- ggplot(codprey.plot3, aes(NMDS1, TL)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Total Length (mm)", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P14)

P21 <- ggplot(codprey.plot3, aes(NMDS2, Shrimp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Shrimp", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P21)

P22 <- ggplot(codprey.plot3, aes(NMDS2, Caprellidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Caprellidae", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P22)

P23 <- ggplot(codprey.plot3, aes(NMDS2, Harp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Harpacticoid", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P23)

P24 <- ggplot(codprey.plot3, aes(NMDS2, Gammarid4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Gammarid", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P24)

P25 <- ggplot(codprey.plot3, aes(NMDS2, TL)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Total Length (mm)", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P25)


P31 <- ggplot(codprey.plot3, aes(NMDS3, Gammarid4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Gammarid", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P31)

P32 <- ggplot(codprey.plot3, aes(NMDS3, Caprellidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Caprellidae", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P32)

P33 <- ggplot(codprey.plot3, aes(NMDS3, Corphiidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Corphiidae", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P33)

P34 <- ggplot(codprey.plot3, aes(NMDS3, crab4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Crab", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P34)

P35 <- ggplot(codprey.plot3, aes(NMDS3, TL)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Total Length (mm)", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P35)

NMDSfigure <- ggarrange(P11, P22, P31, P12, P23, P32, P13, P24, P33, P14, P25, P34, 
                        labels = c("A", "E", "I", "B", "F", "J", "C", "G","K", "D","H", "L", "E", "J" ),
                        ncol = 3, nrow = 4)
NMDSfigure
ggsave("./Figs/NMDS123_all_Factors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with 3 axes, nearly sig species


NMDSfigure <- ggarrange(P11, P23, P31, P12, P24, P32, P14, P25, P33, 
                        labels = c("1A", "2A", "3A", "1B", "2B", "3B", "1C", "2C","3C"),
                        ncol = 3, nrow = 3)
NMDSfigure
ggsave("./Figs/NMDS123_sig_Factors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with 3 axes, all sig species