#### NMS Ordinations by % weight of prey item in stomach
###each stomach adds to 100%. all empty somachs removed
### based on the code sent from Hillary, March 2022. 
#this is updated w more wgt categories than done in march 22
#today is jan 11 2023  The 4 after the prey name, means the data are cube root.

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

dimcheckMDS(prey_wgts2,distance = "bray",k = 6,trymax = 300,autotransform = FALSE)

##again no convergence w 11 prey groups. 
#now going to remove gastropod and calanoid
prey_wgts3 <- prey_wgts2[,c(1:3, 5, 6, 7, 9, 10, 11)]
head(prey_wgts3)


#### Run the NMS Ordination dimensions ####
prey_wgtMDS3<-metaMDS(prey_wgts3, distance="bray", k=3, trymax=300, autotransform=FALSE)


## Set up your three NMS Principal Axes 
NMDS1 <- prey_wgtMDS3$points[,1]
NMDS2 <- prey_wgtMDS3$points[,2]
NMDS3 <- prey_wgtMDS3$points[,3]

#Add them to the ordination data plot
codprey.plot3<-cbind(prey_w2, NMDS1, NMDS2, NMDS3)
head(codprey.plot3)
str(codprey.plot3)
codprey.plot3 <- codprey.plot3 %>%
  mutate(Year_fac = as.factor(Year))

##save file w NMDS 3 dimensions. 
write.csv(codprey.plot3, file = "output/nmds_results_new3.csv")

#### Plot the NMS in Base R ###

plot(prey_wgtMDS3, type = 't', display = c('species'))
### scatter plot above shows polycheate are NMDS 1+ and caprellidate are NMDS1 (-)


## Alisa diverge from HT code to see what variables relate to NMDS axes
## want more detailed scatters to see how the axes are related to species
## go thru each of next 3 plots and change yaxis to diff species and diff environ variables
plot1 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS1, y = TL, color = Month)) +
  geom_point()+
  xlab("NMDS 1")+
  theme_minimal()
plot1 

plot3t <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = TL, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot3t 

plot34 <- codprey.plot3 %>%
  ggplot(aes(x = NMDS3, y = Jul_date, color = Month)) +
  geom_point()+
  xlab("NMDS 3")+
  theme_minimal()
plot34 

# now plot sig variables in scatter FIGURE
library(ggplot2)
library("ggpubr")
library(purrr)

P1 <- ggplot(codprey.plot3, aes(NMDS1, Poly4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Polychaete", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P1)

P2 <- ggplot(codprey.plot3, aes(NMDS1, Caprellidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Caprellidae", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P2)

P3 <- ggplot(codprey.plot3, aes(NMDS1, Harp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Harpacticoid", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P3)

P4 <- ggplot(codprey.plot3, aes(NMDS1, Jul_date)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Day of year", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P4)

P21 <- ggplot(codprey.plot3, aes(NMDS2, Harp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Harpacticoid", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P21)

P22 <- ggplot(codprey.plot3, aes(NMDS2, Gammarid4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Gammarid", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P22)

P23 <- ggplot(codprey.plot3, aes(NMDS2, Shrimp4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Shrimp", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P23)

P24 <- ggplot(codprey.plot3, aes(NMDS2, TL)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Total Length (mm)", x = "NMDS2") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P24)

P31 <- ggplot(codprey.plot3, aes(NMDS3, Gammarid4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Gammarid", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P31)

P32 <- ggplot(codprey.plot3, aes(NMDS3, Corphiidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Corophiidae", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P32)

P33 <- ggplot(codprey.plot3, aes(NMDS3, Caprellidae4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Caprellidae", x = "NMDS3") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P33)

NMDSfigure <- ggarrange(P1, P21, P31, P2, P22, P32, P3, P23, P33, P4, P24,
                      labels = c("A", "E", "I", "B", "F", "J", "C", "G", "K", "D","H"),
                      ncol = 3, nrow = 4)
NMDSfigure
ggsave("./Figs/NMDS123factors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with all axes

NMDSfigure <- ggarrange(P1, P21, P2, P22, P3, P23, P4, P24,
                        labels = c("A", "E", "B", "F", "C", "G", "D","H"),
                        ncol = 2, nrow = 4)
NMDSfigure
ggsave("./Figs/NMDSfactors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with axes 1 and 2 because axis 3 doesn't correspond to environ variable

#### Plot the NMS in ggplot #####  back to Hillary script####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

Wgt_plot <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Season, color = Season), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") +
  scale_fill_manual(values=c("#332288", "#888888", "#CC6677")) +
  scale_color_manual(values=c("#332288", "#888888", "#661100", "#000000")) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=14,face="bold")) 
plot(Wgt_plot)

Wgt_plot <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Month), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Month, color = Month), alpha=.25,type='t',size =1, geom="polygon")+ 
  theme_classic()+
  scale_linetype_manual(values = "solid") 
plot(Wgt_plot)
#this plot seems like June lines up w NMDS2, sept/oct/dec match and Nov is the real wildcard

## begin here!!!!!!!  use third go-round of NMDS with 9 prey groups
### Look at the effects of environmental variables on axes
##Continuous varables + Factors

preyEnvDataFact <- preyEnvData[,c(4, 5, 6)]
head(preyEnvDataFact)
en <- envfit(prey_wgtMDS3, preyEnvDataFact, permutations = 999, na.rm = T)
head(en)

#Only continuous environmental variables
head(preyEnvData)
preyEnvDataCont <- preyEnvData[,c(7, 8, 9, 10)]
head(preyEnvDataCont)
names(preyEnvDataCont)[1]<-paste("Day of Year")
names(preyEnvDataCont)[2]<-paste("Temp")
names(preyEnvDataCont)[3]<-paste("Salinity")
names(preyEnvDataCont)[4]<-paste("TL (mm)")
head(preyEnvDataCont)

en1 <- envfit(prey_wgtMDS3, preyEnvDataCont, permutations = 999, na.rm = T)
head(en1)

#### Get the vectors the correct length
en_coord_cont = as.data.frame(scores(en1, "vectors")) * ordiArrowMul(en1)

#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()  this is used w macOS system
plot(prey_wgtMDS3, type = 't', display = c('species'))
plot(en1)

head(en1)
#so now look at en1 and based on en1
#Subset data with envfit R^2 > 0.2 based on list generated in head(en1)
#Restructure dataframe to have only Day of Year and TL
preyEnvDataCut <- preyEnvDataCont[,c(2, 3, 4)]

names(preyEnvDataCut)[1]<-paste("Temperature")
names(preyEnvDataCut)[2]<-paste("Salinity")
names(preyEnvDataCut)[3]<-paste("TL (mm)")
head(preyEnvDataCut)

en2 <- envfit(prey_wgtMDS3, preyEnvDataCut, permutations = 999, na.rm = T)
head(en2)

#### Get the vectors the correct length
en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)

#### Plot the NMS with environmental variable overlay in Base R ###
#quartz()  this is used w macOS system
plot(prey_wgtMDS3, type = 't', display = c('species'))
plot(en2)

#### Plot the NMS with environmental variable overlay in ggplot #####

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

Cod_Env <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Season), show.legend=F, position=position_jitter(.1))+
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
en_prey <- envfit(prey_wgtMDS3, prey_wgts3, permutations = 999, na.rm = T)
head(prey_wgts3)
head(en_prey)
# Make a subset of Prey Variables with R^2 > 0.1 that are important to final NMDS by season
#select the species that have R^2 with significance
en_prey1 <- envfit(NMDS1, prey_wgts3, permutations = 999, na.rm = T)
head(prey_wgts3)
head(en_prey1)

en_prey2 <- envfit(NMDS2, prey_wgts3, permutations = 999, na.rm = T)
head(prey_wgts3)
head(en_prey2)

en_prey3 <- envfit(NMDS3, prey_wgts3, permutations = 999, na.rm = T)
head(prey_wgts3)
head(en_prey3)
#first make the plot of env var and species

preywgt_cut <- prey_wgts3[,c(1, 2, 3, 5)]
head(preywgt_cut)
preywgt_cut <- rename(preywgt_cut,"Harpacticoid" = "Harp4")
preywgt_cut <- rename(preywgt_cut,"Gammarid" = "Gammarid4")
preywgt_cut <- rename(preywgt_cut,"Polychaete" = "Poly4")
preywgt_cut <- rename(preywgt_cut,"Caprellidae" = "Caprellidae4")
head(preywgt_cut)

en_prey_cut <- envfit(prey_wgtMDS3, preywgt_cut, permutations = 999, na.rm = T)
head(en_prey_cut)
prey_coord = as.data.frame(scores(en_prey_cut, "vectors")) * ordiArrowMul(en_prey_cut)

#### Plot the NMS in ggplot with prey vector overlay for Yea_factor (or can change Year to Month) ####
head(codprey.plot3)
Wgt_prey <- ggplot(data=codprey.plot3, aes(NMDS1, NMDS2))+
  geom_point(data=codprey.plot3, aes(NMDS1, NMDS2, color=Year_fac), show.legend=F, position=position_jitter(.1))+
  stat_ellipse(aes(fill=Year_fac, color = Year_fac), alpha=.2,type='t',size =1, geom="polygon")+ 
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

#### MRPP By Season ####
mrpp(prey_wgts3, preyEnvData$Season, distance = 'bray', weight = 3)

### ISA By Season ####
indval = multipatt(prey_wgts3, preyEnvData$Season, control = how(nperm=999))
summary(indval)

#### MRPP By Month ####
mrpp(prey_wgts3, preyEnvData$Month, distance = 'bray', weight = 3)

### ISA By Month ####
indval = multipatt(prey_wgts3, preyEnvData$Month, control = how(nperm=999))
summary(indval)


### ISA by year
class(preyEnvData$Year)
Year.vector <-as.vector(preyEnvData$Year)
indval = multipatt(prey_wgts3,Year.vector, control = how(nperm=999))
summary(indval)

# now plot sig variables in scatter FIGURE only for NMDS1 and NMDS 2
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

P14 <- ggplot(codprey.plot3, aes(NMDS1, Gammarid4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Gammarid", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P14)

P15 <- ggplot(codprey.plot3, aes(NMDS1, TL)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Total Length (mm)", x = "NMDS1") +
  theme(legend.position = c(0.2, 0.2))+
  geom_smooth(method = "lm", formula = y ~ x, se = F)

plot(P15)

P21 <- ggplot(codprey.plot3, aes(NMDS2, Poly4)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "Polychaete", x = "NMDS2") +
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


NMDSfigure <- ggarrange(P11, P21, P12, P22, P13, P23, P14, P24, P15, P25,
                        labels = c("A", "F", "B", "G", "C", "H", "D","I", "E", "J" ),
                        ncol = 2, nrow = 5)
NMDSfigure
ggsave("./Figs/NMDS12_all_Factors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with 2 axes, all sig species

NMDSfigure <- ggarrange(P11, P23, P12, P24, P13, P25, P15,
                        labels = c("A", "E", "B", "F", "C", "G", "D"),
                        ncol = 2, nrow = 4)
NMDSfigure
ggsave("./Figs/NMDS12_select_factors.png", width = 6, height = 6, units = 'in')
#this is FIGURE NMDS with axes 1 and 2 because axis 3 doesn't correspond to environ variable
