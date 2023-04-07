# This is the plotting & analysis script for CONDITION dataframe = codcond1
#this has only age-0, years 2018-2020, seine gear, cook bay
##THis is for the new appendix 1 in April 2023

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

#these plots of dry vs wet will be used in Appendix
library(ggplot2)
library("ggpubr")

AppK <- codcond1 %>%
  ggplot(aes(x = K_wet, y = Kdry, color = Month)) +
  geom_point()+
  theme_bw()+
  xlim(0.55,0.95)+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  geom_smooth(method = "lm", formula = (y ~x), color = 1)
    
plot(AppK)


#below was trail that didn't work. made two plots
codcondA1 <- filter(codcond1, Month == "June")
codcondA2 <- filter(codcond1, Month != "June")
distinct(codcondA1,Month)
distinct(codcondA2,Month)

AppKnew <- codcondA2 %>%
  ggplot(aes(x = K_wet, y = Kdry)) +
  geom_point()+
  theme_bw()+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  geom_smooth(method = "lm", formula = (y ~x), color = 1)+
  theme(legend.position = "bottom", legend.direction  = "horizontal")+
  xlim(0.5, 0.95)
plot(AppKnew)


AppK1 <-
  ggplot(data = codcondA1, aes(x = K_wet, y = Kdry, color = Month)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)+
  ggplot(data = codcondA2, aes(x = K_wet, y = Kdry)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  theme(legend.position = "bottom", legend.direction  = "horizontal")
plot(AppK1)

#below was trail that didn't work. made two plots
AppK1 <-
  ggplot(data = codcondA1, aes(x = K_wet, y = Kdry, color = Month)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)+
  ggplot(data = codcondA2, aes(x = K_wet, y = Kdry)) +
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)+
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  theme(legend.position = "bottom", legend.direction  = "horizontal")
plot(AppK1)

AppH <- codcond1 %>%
  ggplot(aes(x = HSIwet, y = HSIdry)) +
  geom_point()+
  theme_bw()+
  labs(y = "HSI dry", x = "HSI wet") +
  theme(legend.position = "none")+
  geom_smooth(method = "lm", formula = (y ~x), color = 1)
plot(AppH)

Appendix <- ggarrange(AppH, AppK,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)+
  theme(legend.position = "right")

Appendix
ggsave("./figs/Appendix1.png", width = 6, height = 6, units = 'in')
##mike how can I make two linear regressions...one for june and another for remainder of months

