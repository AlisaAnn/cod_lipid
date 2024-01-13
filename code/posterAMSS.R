
# Libraries
library(patchwork)
library(tidyverse)

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Load the previous script
source("code/1_data_import.R")

head(codcond1)
distinct(codcond1,age)
distinct(codcond1, bay)
distinct(codcond1, gear)
distinct(codcond1, year) #only age-1 fish in 2019, so 2019 doesn't appear


#these plots of dry vs wet from Rscript_2
library(ggplot2)
library("ggpubr")

a <- lm(formula = Kdry~K_wet, data = codcond1)
summary (a)

AppK <- codcond1 %>%
  ggplot(aes(x = K_wet, y = Kdry, color = Month)) +
  geom_point()+
  theme_bw()+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  theme(legend.position = "bottom", legend.direction  = "horizontal")
plot(AppK)

AppK <- codcond1 %>%
  ggplot(aes(x = K_wet, y = Kdry)) +
  geom_point(aes(color = Month)) +
  theme_bw()+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  theme(legend.position = "bottom", legend.direction  = "horizontal") +
  geom_smooth(method = "lm", formula = (y ~x), color = 1)
plot(AppK)

AppH <- codcond1 %>%
  ggplot(aes(x = HSIwet, y = HSIdry, color = "red")) +
  geom_point()+
  theme_bw()+
  labs(y = "HSI dry", x = "HSI wet") +
  theme(legend.position = "none")+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)
plot(AppH)


##For AMSS poster
Poster <- ggarrange(AppH, AppK,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

Poster







##using as.factor(age) made the legend be a category so it used only whole numbers
plot1 <- codcond1 %>%
  ggplot(aes(x = log(TL), y = log(wgt_total), color= as.factor(age))) +
  geom_point()+
  theme_minimal()

plot1


##### ANALYSIS #####
##log transforming data makes good inspection plots with no pattern in residuals##
lm_model <- lm(log(wgt_total) ~ log(TL), data = codcond1)
lm_model

summary(lm_model)
par(mfrow=c(2,2))
plot(lm_model)
library(coefplot)
coefplot(lm_model)
resid(lm_model)
resid <- resid(lm_model)
length(resid)
codcond1 <- mutate(codcond1, resid)


# When we boxplot this, we notice that the factors in month are alphabetical. 
#we want them to be sequential, so need to create a new column "Month" w ordered factor
codcond1 <- mutate(codcond1, Month = fct_relevel(month, c("June", "July", "Aug",
                                                          "Sept", "Oct", "Nov", "Dec")))
# We use fct_relevel to explicitly define the factor order for column "Month" 



#plot LW residuals vs month and compare w kdry
a<- ggplot(data = codcond1, aes(x = Month, y = resid, fill = as.factor(year))) +
  geom_boxplot()+
  facet_wrap(~year)+
  theme_minimal()+
  theme(legend.position = "bottom")

a

b<- ggplot(data = codcond1, aes(x = Month, y = resid)) +
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "bottom")

b

c<- ggplot(data = codcond1, aes(x = Month, y = K_wet, fill = as.factor(year))) +
  geom_boxplot()+
  facet_wrap(~year)+
  theme_minimal()+
  theme(legend.position = "bottom")

c

d<- ggplot(data = codcond1, aes(x = Month, y = K_wet)) +
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "bottom")

d

e<- ggplot(data = codcond1, aes(x = Month, y = Kdry, fill = as.factor(year))) +
  geom_boxplot()+
  facet_wrap(~year)+
  theme_minimal()+
  theme(legend.position = "bottom")

e

f<- ggplot(data = codcond1, aes(x = Month, y = Kdry)) +
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "bottom")

f
Poster2 <- ggarrange(a,b,c,d,e,f,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 2, nrow = 3)
Poster2

##for poster going to plot only 2020 due to space
codcond2 <- filter(codcond1, year ==2020)
distinct(codcond1,year)
distinct(codcond2, year)

b2<- ggplot(data = codcond2, aes(x = Month, y = resid)) +
  geom_boxplot(color = "black", fill = "darkgreen")+
  labs(y = "LW Residuals")+ 
  theme_minimal()+
  theme(legend.position = "none")

b2


d2<- ggplot(data = codcond2, aes(x = Month, y = K_wet)) +
  geom_boxplot(color = "black", fill = "darkgreen")+
  theme_minimal() +
  theme(legend.position = "none")

d2


f2<- ggplot(data = codcond2, aes(x = Month, y = Kdry)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

f2
Poster3 <- ggarrange(b2,d2,f2,
                     labels = c("A", "B", "C"),
                     ncol = 3, nrow = 1)
Poster3





#######Now compare whole body lipid w tissue specific FA
library(tidyverse)
library(lubridate)
.libPaths()

# Read in the data and rename any columns that need renaming
lipid <- read_csv("data/lipid_whole_poster_compare.csv")
str(lipid)

FA1 <- ggplot(data = lipid, aes(x = as.factor(Month), y = Total_FA_mg)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FA1

FAL <- ggplot(data = lipid, aes(x = as.factor(Month), y = PerLiverFA)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FAL
FAM <- ggplot(data = lipid, aes(x = as.factor(Month), y = PerMuscleFA)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FAM

HSI <- ggplot(data = lipid, aes(x = as.factor(Month), y = HIS)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

HSI
Poster_FA <- ggarrange(FAM, FAL, FA1, HSI,
                     labels = c("A", "B", "C", "D"),
                     ncol = 1, nrow = 4)
Poster_FA

##try only with 2020
lipid2 <- filter(lipid, year==2020)
distinct(lipid2,year)

FA1 <- ggplot(data = lipid2, aes(x = as.factor(Month), y = Total_FA_mg)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FA1

FAL <- ggplot(data = lipid2, aes(x = as.factor(Month), y = PerLiverFA)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FAL
FAM <- ggplot(data = lipid2, aes(x = as.factor(Month), y = PerMuscleFA)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

FAM

HSI <- ggplot(data = lipid2, aes(x = as.factor(Month), y = HIS)) +
  geom_boxplot(color = "black", fill = "orange")+
  theme_minimal() +
  theme(legend.position = "none")

HSI
Poster_FA2020 <- ggarrange(FAM, FAL, FA1, HSI,
                       labels = c("A", "B", "C", "D"),
                       ncol = 1, nrow = 4)
Poster_FA2020




