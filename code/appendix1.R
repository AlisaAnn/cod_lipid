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
  labs(y = "Fulton's K dry", x = "Fulton's K wet") +
  theme(legend.position = "bottom", legend.direction  = "horizontal")
plot(AppK)

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
ggsave("./figs/Appendix.png", width = 6, height = 6, units = 'in')
##mike how can I make two linear regressions...one for june and another for remainder of months

a <- lm(formula = HSI_wet~HSIdry, data = codcond1)
summary (a)

#### PLOTTING ####

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  xlab("age-0, Cook Bay 2018 and 2020")

plot1

plot1 <- codcond1 %>%
  ggplot(aes(x = Kdry, y = HSIdry, color = Month)) +
  geom_point()+
  theme_minimal()
plot1
#probably not using Kdry anymore. Nov 2022 notes. test to see if relationship

##these scatter plots and results with Kdry and HSIdry for age-0 only
##show no relationship and so can test Kdry and HSI independently.
a <- lm(formula = HSIdry~Kdry, data = codcond1)
summary (a)
##R2 = 0.0033, n=217) no good relationship

a <- lm(formula = HSIwet~K_wet, data = codcond1)
summary (a)
##R2 = 0.0268, n=417) no good relationship, larger sample size than dry