# This is the plotting & analysis script for CONDITION dataframe = codcond1
#this has only age-0, years 2018-2020, seine gear, cook bay

# Libraries
library(patchwork)

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
  xlab("age-0, Cook Bay 2018 and 2020")
plot1

plot1 <- codcond1 %>%
  ggplot(aes(x = Kdry, y = HSIdry, color = Month)) +
  geom_point()+
theme_minimal()
plot1
#probably not using Kdry anymore. Nov 2022 notes. test to see if relationship

a <- lm(formula = HSIdry~Kdry, data = codcond1)
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

##Alisa try something with cod condition data: HSI and Kdry
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


## explore condition by day by year -------ALISA-NEW---------------------

library(mgcv)


codcond1 <- codcond1 %>%
  mutate(year_fac = as.factor(year))

# plot HSIwet by year and Julian day
ggplot(codcond1, aes(Julian_date, HSI_wet, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(HSI_wet ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)


# plot HSIdry by year and Julian day
ggplot(codcond1, aes(Julian_date, HSIdry, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(HSIdry ~ s(Julian_date, k = 4) + year_fac, data = codcond1)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

##seems that results same for HSI wet and HSI dry. Try linear model
linear_mod <- lm (formula = HSI_wet~ HSIdry, data = codcond1)
summary(linear_mod)

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
