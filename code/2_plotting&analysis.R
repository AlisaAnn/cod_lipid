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

#### PLOTTING ####

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point()+
  xlab("age-0, Cook Bay 2018 and 2020")+
  theme_minimal()
plot1

plot1 <- codcond1 %>%
  ggplot(aes(x = Kdry, y = HSIdry, color = Month)) +
  geom_point()+
theme_minimal()
plot1
#probably not using Kdry anymore. Nov 2022 notes. test to see if relationship

##these scatter plots and results with Kdry and HSIdry
a <- lm(formula = HSIdry~Kdry, data = codcond1)
summary (a)

a <- lm(formula = Kdry~K_wet, data = codcond1)
summary (a)

plot1 <- codcond1 %>%
  ggplot(aes(x = K_wet, y = Kdry, color = Month)) +
  geom_point()+
  theme_minimal()
plot1
ggsave("./figs/Kwet_Kdry.png", width = 6, height = 4, units = 'in')

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

plot1 <- codcond1 %>%
  ggplot(aes(x = TL, y = HSI_wet, color = Month)) +
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


mod10 <- gam(formula = liver_bi ~ s(TL, k = 6) + year_fac, family = "quasibinomial", data = codFA)

summary(mod10) # mod9 better

gam.check(mod10) # mod9 better

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

##are results same for muscle FA as liver FA?
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
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot(M)

L <- ggplot(codFA, aes(J_date, liverFA, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
plot(L)
FAfigure <- ggarrange(L, M,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
FAfigure
ggsave("./Figs/liverFA_muscleFA.png", width = 6, height = 3, units = 'in')



##seems that results same for HSI wet and HSI dry. Try linear model
linear_mod <- lm (formula = HSI_wet~ HSIdry, data = codcond1)
summary(linear_mod)
ggplot(linear_mod, aes(HSIdry, HSI_wet)) +
  geom_point() +
  theme_minimal()

# plot Fultondry by year and Julian day
ggplot(codcond1, aes(Julian_date, Kdry, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)



mod1 <- gam(Kdry ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

# plot Fultonwet by year and Julian day
ggplot(codcond1, aes(Julian_date, K_wet, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(K_wet ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

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


#repeat anova for both years w stomach weight
lm_model1820gut <- lm(stom_percent_wgt ~ Month, data = codcond1)
lm_model1820gut

summary(lm_model1820gut)

