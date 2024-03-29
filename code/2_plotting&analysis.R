# This is the plotting & analysis script for CONDITION dataframe = codcond1
#this has only age-0, years 2018-2020, seine gear, cook bay
##any plots or linear models with Kdry or HSIdry are in this script.

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
View(codcond1)
#### PLOTTING ####

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  xlab("age-0, Cook Bay 2018 and 2020")
  
plot1

##ICes reviewer wanted to see LW regression diff by year____but these are gams not exp
library(dplyr)
cod2018<- filter(codcond1,year==2018)
cod2020<- filter(codcond1, year ==2020)
plot1review <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  geom_smooth(method = "gam", data=cod2018, aes(x=TL, y = wgt_total), 
              color= "red") +
  geom_smooth(method = "gam", data=cod2020, aes(x=TL, y = wgt_total), 
            color= "blue")+
  xlab("age-0, Cook Bay 2018 and 2020")+
  ylab("Body weight (g)")

plot1review
#################################
##Again, second plot for reviewer #1: with TL < 110 mm
##ICes reviewer wanted to see LW regression diff by year____but these are gams not exp
library(dplyr)
cod2018<- filter(codcond1,year==2018 & TL<=110)
cod2020<- filter(codcond1, year ==2020 & TL<=110)
plot2review <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  #geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  geom_smooth(method = "gam", data=cod2018, aes(x=TL, y = wgt_total), 
              color= "red", se = TRUE) +
  geom_smooth(method = "gam", data=cod2020, aes(x=TL, y = wgt_total), 
              color= "blue", se = TRUE)+
  xlab("age-0, Cook Bay 2018 and 2020")+
  ylab("Body weight (g)")

plot2review
#######################
#relationship betweeen TL and HSIwet?
plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = HSI_wet)) +
  geom_point()+
  theme_minimal()+
  geom_smooth()
plot1

TLmod1 <- gam(HSI_wet ~ s(TL), data = codcond1)
summary(TLmod1)# GAM says r=0.122, n = 419

#relationship betweeen TL and liver weight?
plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = liver_wgt)) +
  geom_point()+
  theme_minimal()+
  geom_smooth()
plot1

TLmod1 <- gam(liver_wgt ~ s(TL), data = codcond1)
summary(TLmod1)# GAM says r=0.636, n = 419


###########################

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
##R2 = 0.0033, df=217, n = 219) no good relationship

#above is linear model and probably better to do pearson correlation test, as per ICES review
vector.HSIdry <- codcond1$HSIdry
vector.kdry <- codcond1$Kdry
a.rev <- cor.test(vector.HSIdry, vector.kdry)
a.rev
##Pearson's correlation df = 217, n = 219, p= 0.4003, cor = -0.057) no good relationship
##same p-value as in linear model. 200 obs deleted because NA for kdry

#_________
b <- lm(formula = HSIwet~K_wet, data = codcond1)
summary (b)
##R2 = 0.0268, n=419) no good relationship, larger sample size than dry

##Again, need to change lm to pearson cor
vector.HSIwet <- codcond1$HSIwet
vector.Kwet <- codcond1$K_wet
b.rev <- cor.test(vector.HSIwet, vector.Kwet)
b.rev
##Pearson r = 0.1638, n=419, df = 417, p<0.001) no good relationship, 

#_________
c <- lm(formula = Kdry~K_wet, data = codcond1)
summary (c)
##R2 = 0.431, n=219, df = 217) no good relationship,

##Again, need to change lm to pearson cor
c.rev <- cor.test(vector.kdry, vector.Kwet)
c.rev
##Pearson r = 0.6568, n=219, df = 217, p<0.001) fair relationship, 

#now test only for June kwet vs kdry
unique(codcond1$month)
june.df <- filter(codcond1,month == "June")
june.vector.kdry <- june.df$Kdry
june.vector.Kwet <- june.df$K_wet
#View(june.df)
june.rev <- cor.test(june.vector.kdry, june.vector.Kwet)
june.rev
##Pearson cor = 0.1666, df = 20, p = 0.4588, n = 22

##now test without month of June
notjune.df <- filter(codcond1,month != "June")
notjune.vector.kdry <- notjune.df$Kdry
notjune.vector.Kwet <- notjune.df$K_wet
notjune.rev <- cor.test(notjune.vector.kdry, notjune.vector.Kwet, na.remove = TRUE)
notjune.rev
##Pearson cor = 0.8121, df = 195, p = <0.001, n = 197

##return to HSIwet vs HSIdry for all dataset and test w pearson corr
allHSI.rev <- cor.test(vector.HSIwet, vector.HSIdry)
allHSI.rev
##Pearson cor = 0.991, df = 217, n = 219, p < 0.0001


#these plots of dry vs wet will be used in Appendix
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

AppH <- codcond1 %>%
  ggplot(aes(x = HSIwet, y = HSIdry, color = Month)) +
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


plot1 <- codcond1 %>%
  ggplot(aes(x = K_wet, y = HSI_wet, color = Month)) +
  geom_point()+
  theme_minimal()
plot1
# 2 datapoints in Nov are outliers. check on these. OK. checked and they are valid
#test to see if relationship

a <- lm(formula = HSI_wet~K_wet, data = codcond1)
summary (a)


plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = HSIdry, color = Month)) +
  geom_point()+
  theme_minimal()
plot1
#probably not using Kdry anymore. Nov 2022 notes

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = HSI_wet, color = Month)) +
  geom_point()+
  theme_minimal()
plot1 #to show liver doesn't develop until 50 mm

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = K_wet, color = Month)) +
  geom_point()+
  theme_minimal()
plot1


head(codcond1)
##using as.factor(age) made the legend be a category so it used only whole numbers
plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color= as.factor(age))) +
  geom_point()+
theme_minimal()

plot1

# Save your plot
ggsave(plot1, filename = "output/LW_scatter.png", width = 6.5, height = 6, units = "in")
plot1

##### ANALYSIS #####
##ask Mike how to get LW equation to put into results##
lm_model <- lm(wgt_total ~ TL, data = codcond1)
lm_model

summary(lm_model)

##going to run age-0 length weight regression
#mike helping me do this since prior Wr values seemed weird

library (mgcv) #library for running gams
mod1 <- gam(wgt_total ~ s(TL) + month, data = codcond1)
##s means smooth length. putting month into the gam
#this model treats every fish as an individual observation, and maybe the seine should be the observation

summary (mod1)
plot(mod1)
str(mod1) #this tells us everything that is saved in model object
str(summary(mod1))

##it is the 7x4 table in p.table that looks interesting
plot.month <- as.data.frame(summary(mod1)$p.table)
plot.month

str(plot.month)

plot.month <- plot.month %>%
  mutate(month = str_remove_all(row.names(plot.month), "month"))


change <- grep("Int", plot.month$month)

plot.month$month[change] <- "Aug"


plot.month

plot.month <- plot.month %>%
  select(Estimate, 'Std. Error', month) %>%
  rename(SE = `Std. Error`)

dim(plot.month)

plot.month[2:7,1] <- plot.month[2:7,1] + plot.month[1,1]


plot.order <- data.frame(month = plot.month$month,
                         order = c(3, 7, 2, 1, 6, 5, 4))

plot.month <- left_join(plot.month, plot.order)

plot.month$month <- reorder(plot.month$month, plot.month$order)

ggplot(plot.month, aes(month, Estimate)) +
  geom_col(fill = "dark grey") + 
  geom_errorbar(aes(ymin = Estimate - 1.96*SE,
                    ymax = Estimate + 1.96*SE))

str(plot.month)

###Alisa try something with cod condition data: HSI and Kdry by MONTH---
##REALLY WANT JULIAN DAY< NOT MONTH
##SKip to line 364 for J_date analyses

ggplot(data = codcond1,
       aes(x = TL,
           y = wgt_total,
           color = sex,
           shape = sex)) +
  geom_point(size = 1.5,
             alpha = 0.8)

ggplot(data = codcond1,
       aes(x = Month,
           y = TL,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)


ggplot(data = codcond1,
       aes(x = Month,
           y = HSI_wet,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~year)
ggsave(filename = "output/HSIwet_month.png", width = 6.5, 
       height = 6, units = "in")

ggplot(data = codcond1,
       aes(x = Month,
           y = K_wet,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~year)

ggplot(data = codcond1,
       aes(x = Month,
           y = K_wet,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")

ggplot(data = codcond1,
       aes(x = Month,
           y = Kdry,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")

##try only plotting HSI data for 2018 bc of yaxis scale
codcond2 <- filter(codcond1,year==2018)
ggplot(data = codcond2,
       aes(x = Month,
           y = HSIdry,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  xlab("Month (only year 2018)")

##try only plotting HSI data for 2018 bc of yaxis scale
codcond2 <- filter(codcond1,year==2018)
ggplot(data = codcond2,
       aes(x = Month,
           y = HSIwet,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  xlab("Month (only year 2018)")


##run ANOVA on 2018 data age-0 only. To see if HSI varies by month
##Mike saw this and said it shows sig diff in data distribution by month
##notice that december is more units difference than september

lm_model18 <- lm(HSI_wet ~ month, data = codcond2)
lm_model18

summary(lm_model18)
##above anova is good to go. should repeat for 2020 data or both years

#repeat anova for both years w HSIdry
lm_model1820 <- lm(HSIdry ~ month, data = codcond1)
lm_model1820

summary(lm_model1820)

#repeat anova for both years w HSIwet
lm_model1820wet <- lm(HSI_wet ~ month, data = codcond1)
lm_model1820wet

summary(lm_model1820wet)

#repeat anova for both years w Kwet
head(codcond1)
lm_model1820Kwet <- lm(K_wet ~ month, data = codcond1)
lm_model1820Kwet

summary(lm_model1820Kwet)

#repeat anova for both years w Kdry
lm_model1820Kdry <- lm(Kdry ~ month, data = codcond1)
lm_model1820Kdry

summary(lm_model1820Kdry)


##now returning to all 3 years
ggplot(data = codcond1,
       aes(x = Month,
           y = Kdry,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~year)

distinct(codcond1,age)

#This Wr is old idea
#ggplot(data = codcond1,
 #      aes(x = Month,
  #         y = Wr,
   #        color = Month)) +
#  geom_boxplot(width = 0.3)+
#  geom_jitter(alpha = 0.5)+
#  theme_minimal()+
#  geom_hline(yintercept=100) ##to add horizonal line at 100


ggplot(data = codcond1,
       aes(x = Month,
           y = Kdry,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
theme_minimal()+
theme(legend.position = "bottom")+
  facet_wrap(~year)

ggplot(data = codcond1,
       aes(x = Month,
           y = K_wet,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~year)

ggplot(data = codcond1,
      aes(x = as.factor(year),
          y = HSIwet,
          color = as.factor(year))) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")

ggplot(data = codcond1,
       aes(x = as.factor(year),
           y = Kdry,
           color = as.factor(year))) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  theme(legend.position = "bottom")





##going to make a pivot table (temp) to see the count (n) of different ages per year
temp <- codcond1 %>%
  group_by(year, age) %>%
  summarise(n())
temp
temp <- codcond1 %>%
  group_by(year, Month) %>%
  summarise(n())
  theme(legend.position = "bottom")+
  facet_wrap(~year)


temp

## explore HSI by day (rather than month) by year -------ALISA-NEW--------

library(mgcv)
library(gamm4)

codcond1 <- codcond1 %>%
  mutate(year_fac = as.factor(year),
         site_fac = as.factor(site),
         day_fac = as.factor(Julian_date))

# plot HSIwet by year and Julian day
HSIplot <- ggplot(codcond1, aes(Julian_date, HSI_wet, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
plot(HSIplot)
ggsave(HSIplot, filename = "output/HSIwet_Jdate.png", width = 6.5, 
       height = 6, units = "in")


mod1 <- gam(HSI_wet ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

##12/2/22 Mike wanted me to also plot condition by TL and then compare the
# two models with MuLin to see if condition determined by size or season.
# plot HSIwet by year and Total length

ggplot(codcond1, aes(TL, HSI_wet, color = year_fac))+
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod2 <- gam(HSI_wet ~ s(TL, k = 4) + year_fac, data = codcond1)

summary(mod2)

plot(mod2, se = F, resid = T, pch = 19)

install.packages('evaluate')
install.packages('hexbin')

MuMIn::AICc(mod1, mod2)
#since I cannot get R to recognize package MuMIn, I will just use AIC 
#AIC = Akaike's Information Criterion
AIC(mod1,mod2)
#and there is BIC which is Schwarz's Bayesian Criterion. 
#in both AIC and BIC the smaller value is the better fit.
#BIC(mod1,mod2)

####ALISA STOP HERE BECAUSE NEED TO RUN W MUMIN ON MIKE"S MACHINE TO GET NEW PLOTS



# different curve for Julian date in each year
mod3 <- gam(HSI_wet ~ s(Julian_date, k = 4, by = year_fac), data = codcond1)
summary(mod3)
plot(mod3)

mod4 <- gam(HSI_wet ~ s(TL, k = 4, by = year_fac), data = codcond1)
summary(mod4)

MuMIn::AICc(mod1, mod2, mod3, mod4) # model 3 is the best (separate Julian day curves by year)
summary(mod3)
plot(mod3, resid = T, pch = 19)

# and plot with random terms included to get better CI estimates
mod3a <- gamm4(HSI_wet ~ s(Julian_date, k = 4, by = year_fac),
               random=~(1|site_fac/day_fac),
               data = codcond1)
summary(mod3a$gam)

# get the data to plot
plot_dat <- plot(mod3a$gam)

# restructure into a data frame to plot in ggplot
plot_this <- data.frame(year = as.factor(rep(c(2018, 2020), each = 100)),
                        facet = "Day of year", 
                        HSI_wet = c(plot_dat[[1]]$fit, plot_dat[[2]]$fit),
                        se = c(plot_dat[[1]]$se, plot_dat[[2]]$se),
                        x_value = c(plot_dat[[1]]$x, plot_dat[[2]]$x))

my.col = cb[c(2,6)]

ggplot(plot_this, aes(x_value, HSI_wet, color = year, fill = year)) +
  geom_line() +
  geom_ribbon(aes(ymin = HSI_wet - 1.96*se,
                  ymax = HSI_wet + 1.96*se), 
              alpha = 0.2,
              lty = 0) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.6, 0.8),
        legend.title = element_blank()) +
  ylab("HSI wet") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)

# separate curves for each effect in each year
mod5 <- gam(HSI_wet ~ s(Julian_date, k = 4, by = year_fac) +
              s(TL, k = 4, by = year_fac), data = codcond1)
summary(mod5)


MuMIn::AICc(mod3, mod5)
summary(mod5)
plot(mod5, resid = T)

mod6 <- gam(HSI_wet ~ s(Julian_date, k = 4) +
               s(TL, k = 4) + year_fac, data = codcond1)
             

# MuMIn::AICc(mod5, mod6)

mod7 <- gam(HSI_wet ~ s(Julian_date, k = 4) +
              s(TL, k = 4), data = codcond1)

# MuMIn::AICc(mod5, mod6, mod7)
MuMIn::AICc(mod5, mod7)

# save AICc output
out <- as.data.frame(MuMIn::AICc(mod1, mod2, mod3, mod4, mod5, mod6, mod7))

write.csv(out, "./output/HSI_wet_AICc2.csv", row.names = F)

# model 5 remains the best model
# refit with gamm4 to account for non-independence within nested site/set
# and plot
mod5a <- gamm4(HSI_wet ~ s(Julian_date, k = 4, by = year_fac) +
              s(TL, k = 4, by = year_fac),
              random=~(1|site_fac/day_fac),
              data = codcond1)

summary(mod5a$gam)

# get the data to plot
plot_dat <- plot(mod5a$gam)

# restructure into a data frame to plot in ggplot
plot_this <- data.frame(year = as.factor(rep(c(2018, 2020, 2018, 2020), each = 100)),
                        facet = rep(c("Day of year", "Total length (mm)"), each = 200), 
                        HSI_wet = c(plot_dat[[1]]$fit, plot_dat[[2]]$fit, plot_dat[[3]]$fit, plot_dat[[4]]$fit),
                        se = c(plot_dat[[1]]$se, plot_dat[[2]]$se, plot_dat[[3]]$se, plot_dat[[4]]$se),
                        x_value = c(plot_dat[[1]]$x, plot_dat[[2]]$x, plot_dat[[3]]$x, plot_dat[[4]]$x))
  
my.col = cb[c(2,6)]

ggplot(plot_this, aes(x_value, HSI_wet, color = year, fill = year)) +
  geom_line() +
  geom_ribbon(aes(ymin = HSI_wet - 1.96*se,
                    ymax = HSI_wet + 1.96*se), 
                alpha = 0.2,
              lty = 0) +
  facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.6, 0.8),
        legend.title = element_blank()) +
  ylab("HSI wet") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)

anova(mod5a$gam)

ggsave("./Figs/HSIwet_vs_day_length.png", width = 6, height = 3, units = 'in')

## liver FA models## -----------------------

# set up things!
# Libraries
library(patchwork)

# Load the previous script
source("code/1_data_import.R")


#### PLOTTING ####
head(codgrosslipid)

##this renames columns and makes new dataframe 'codFA' 
codFA <- rename(codgrosslipid,"HSIwet" = "HIS_wet") 
codFA <- rename(codFA,"liverFA" = "per_liver_FA") 
codFA <- rename(codFA,"muscleFA" = "per_musc_FA") 
head(codFA)

library(mgcv)
library(gamm4)

codFA <- codFA %>%
  mutate(year_fac = as.factor(Year),
         site_fac = as.factor(`site #`),
         day_fac = as.factor(J_date)) 
  
# plot liver FA by year and Julian day
ggplot(codFA, aes(J_date, liverFA, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  labs(y = "Percent Fatty Acids in Liver", x = "Day of Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
  
ggsave("./output/liverFA_Jdate.png", width = 6.5, 
       height = 6, units = "in")

##AAA attempt 1/5/23,  renaming mod 1-8 as mod 9-15
# using liver_bi which is liver percent / 100 to yield value between 0 and 1.

ggplot(codFA, aes(liver_bi)) +
  geom_histogram(bins = 20, fill = "grey", color = "black")
#data skewed

ggplot(codFA, aes(J_date, liver_bi, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", family = "quasibinomial", formula = y ~ s(x, k = 4), se = F)
#this plot looks same as liverFA by julian date. only now yaxis from 0 to 1

mod9 <- gam(formula = liver_bi ~ s(J_date, k = 6) + year_fac, family = "quasibinomial", data = codFA)


summary(mod9)

gam.check(mod9)

plot(mod9, se = F, resid = T, pch = 19)

ggplot(codFA, aes(TL, liverFA, color = year_fac))+
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod10 <- gam(formula = liver_bi ~ s(J_date, k = 6, by = year_fac), family = "quasibinomial", data = codFA)
 
#re-evaluating mod 11 now and it surpasses model 9.
##use model 11 in the final resubmit to ICES
summary(mod10) # best

gam.check(mod10) 

plot(mod10, se = F, resid = T, pch = 19)

#install.packages('evaluate')
#install.packages('hexbin')

mod11 <- gam(formula = liver_bi ~ s(J_date, k = 6, by = year_fac), family = "quasibinomial", data = codFA)
gam.check(mod11)

summary(mod11)
plot(mod11)

mod12 <- gam(formula = liver_bi ~ s(TL, k = 6, by = year_fac), family = "quasibinomial", data = codFA)
summary(mod12)
gam.check(mod12)
plot(mod12, resid = T, pch = 19)

# tried this with k = 6...overfit?
mod13 <- gam(formula = liver_bi ~ s(J_date, k = 5, by = year_fac) +
             s(TL, k = 5, by = year_fac), family = "quasibinomial", data = codFA)
summary(mod13)
plot(mod13, resid = T, pch = 19)
gam.check(mod13)


mod13a <- gamm(formula = liver_bi ~ s(J_date, k = 5, by = year_fac) +
                 s(TL, k = 5, by = year_fac), correlation = corAR1(), family = "quasibinomial", data = codFA, niterPQL=500)


mod14 <- gam(formula = liver_bi ~ s(J_date, k = 6, by = year_fac) +
               s(TL, k = 6), family = "quasibinomial", data = codFA)
summary(mod14)
plot(mod14, resid = T, pch = 19)
gam.check(mod14)


mod15 <- gam(formula = liver_bi ~ s(J_date, k = 5) +
               s(TL, k = 5), family = "quasibinomial", data = codFA)
summary(mod15)
plot(mod15, resid = T, pch = 19)
gam.check(mod15)




mod13a <- gamm(formula = liver_bi ~ s(J_date, k = 5, by = year_fac) +
                 s(TL, k = 5, by = year_fac), correlation = corAR1(), family = "quasibinomial", data = codFA, niterPQL=500)


mod15a <- gam(formula = liver_bi ~ s(J_date, k = 4) +
              s(TL, k = 4), family = "quasibinomial", data = codFA)
summary(mod15a)
#lowering k value doesn't really change model 15

# out of curiosity, look at a TL model without a year_face term
mod16 <- gam(formula = liver_bi ~ s(TL, k = 6) +
               s(J_date, k = 6) + year_fac, family = "quasibinomial", data = codFA)
summary(mod16)
gam.check(mod16)
plot(mod16, resid = T, pch = 19)

##curious about removing length as a term
mod16b <- gam(formula = liver_bi ~ s(J_date, k = 6) + year_fac, family = "quasibinomial", data = codFA)
summary(mod16b)
gam.check(mod16b)
plot(mod16b, resid = T, pch = 19)

mod16c <- gam(formula = liver_bi ~ s(J_date, k = 5, by = year_fac), family = "quasibinomial", data = codFA)
summary(mod16c)
plot(mod16c, resid = T, pch = 19)
gam.check(mod16c)
##need to keep length due to above results
##mod14 the best

##check: are results same for muscle FA as liver FA?
##test two good models: mod 15 and mod16 for muscle:
mod20 <- gam(formula = muscle_bi ~ s(J_date, k = 5) +
               s(TL, k = 5), family = "quasibinomial", data = codFA)
summary(mod20)
plot(mod20, resid = T, pch = 19)
gam.check(mod20)

mod21 <- gam(formula = muscle_bi ~ s(TL, k = 6) +
               s(J_date, k = 6) + year_fac, family = "quasibinomial", data = codFA)
summary(mod21)
gam.check(mod21)
plot(mod21, resid = T, pch = 19)
##Yes, these model outputs same for liver or muscle

# plot liver FA and muscle FA by year and Julian day
library(ggplot2)
library("ggpubr")

M <- ggplot(codFA, aes(J_date, muscleFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Muscle Fatty Acids", x = "Day of Year") +
  theme(legend.position = c(0.2, 0.2))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot(M)

L <- ggplot(codFA, aes(J_date, liverFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Liver Fatty Acids", x = "Day of Year") +
  theme(legend.position = c(0.2, 0.8))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
plot(L)

TL <- ggplot(data = codFA,
             aes(x = TL,
                 y = liverFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  labs(y = "% Liver Fatty Acids", x = "Total Length (mm)") 
  
plot(TL)  

TLM <- ggplot(data = codFA,
             aes(x = TL,
                 y = muscleFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  labs(y = "% Muscle Fatty Acids", x = "Total Length (mm)") 

plot(TLM)  

  
FAfigure <- ggarrange(L, TL, M, TLM,
                    labels = c("A", "C", "B", "D"),
                    ncol = 2, nrow = 2)
FAfigure
ggsave("./Figs/liverFA_muscleFA.png", width = 6, height = 6, units = 'in')
#this is FIGURE 7


##seems that results same for HSI wet and HSI dry. Try linear model
linear_mod <- lm (formula = HSI_wet~ HSIdry, data = codcond1)
summary(linear_mod)
ggplot(linear_mod, aes(HSIdry, HSI_wet)) +
  geom_point() +
  theme_minimal()

# plot Fulton dry by year and Julian day
FulKdry <- ggplot(codcond1, aes(Julian_date, Kdry, color = year_fac)) +
  geom_point(size = 2) +
  theme_bw()+
  theme(legend.position = c(0.2, 0.9))+
  scale_colour_discrete(name = "Year") +
  labs(y = "Fulton's Condition (K dry)", x = "Day of Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)

plot(FulKdry)

mod1 <- gam(Kdry ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

ggsave(HSIplot, filename = "output/FulKdry_Jdate.png", width = 6.5, 
       height = 6, units = "in")

plot(mod1, se = F, resid = T, pch = 19)

# plot Fulton Kwet by year and Julian day
ggplot(codFA, aes(J_date, Kwet, color = year_fac)) +
  geom_point(size = 2) +
  theme_bw()+
  theme(legend.position = c(0.9, 0.8))+
  scale_colour_discrete(name = "Year") +
  labs(y = "Fulton's Condition (K wet)", x = "Day of Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)

mod1 <- gam(Kwet ~ s(J_date, k = 4) + year_fac, data = codFA)

summary(mod1)

ggsave("./output/Kwet_Jdate.png", width = 6.5, 
       height = 6, units = "in")

plot(mod1, se = F, resid = T, pch = 19)

##as per Mike's 12/2 suggestion, going to plot Kwet by TL
# plot Fultonwet by size
ggplot(codcond1, aes(TL, K_wet, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(K_wet ~ s(TL, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

# plot Fulton Kdry by size
ggplot(codcond1, aes(TL, Kdry, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(Kdry ~ s(TL, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

##seems that results same for Fulton wet and Fulton dry. Try linear model
linear_mod <- lm (formula = K_wet~ Kdry, data = codcond1)
summary(linear_mod)
## ---------------------------------------

###new plots w STOMACH WEIGHT 
head(codcond1)

ggplot(data = codcond1,
       aes(x = Month,
           y = stom_percent_wgt,
           color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()

gam_model1820gut <- aov(stom_percent_wgt ~ Month, data = codcond1)
#yes, there is a difference by month (p=0.0236, n = 419, df = 6)
summary(gam_model1820gut)
TukeyHSD(gam_model1820gut)

#repeat anova for both years w stomach weight
gam_model1820gut <- lmer((stom_percent_wgt/100) ~ Month, data = codcond1, family = binomial)
gam_model1820gut


summary(gam_model1820gut)
par(mfrow=c(2,2))
plot(gam_model1820gut)
