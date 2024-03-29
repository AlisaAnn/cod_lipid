##making a new figure 4 for paper
##want to keep fig 4 with length by date
##and also add the figure of Kdry by date

#Libraries
library(patchwork)
library(tidyverse)
library(mgcv)
library(gamm4)

library(ggplot2)
library("ggpubr")

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
my.col = cb[c(2,6)]

# Load the previous script
source("code/1_data_import.R")


#### PLOTTING from script 3_length$cpue.R####
head(codlen)

codlen <- codlen %>%
  mutate(year_fac = as.factor(year))

# cod len has 2019 also. but want to plot 2018-2020 only
A <- ggplot(filter(codlen, year_fac %in% c(2018, 2020) & TL < 200), aes(J_date, TL, color = year_fac)) +
  geom_point() +
  theme_bw()+
  theme(legend.position = c(0.2, 0.7))+
  theme(legend.title = element_blank()) +
  scale_colour_discrete(name = "Year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)+
  labs(x = "Day of Year", y = "Total Length (mm)")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) 
  
plot(A)

ggsave("./figs/length_by_date_2018_2020.png", width = 6, height = 4, units = 'in')


##But want to run this without 2019. 
codlen1820 <- filter(codlen, year == "2018" | year == "2020")
#above makes df without 2019
distinct(codlen1820, year)
mod1 <- mgcv::gam(TL ~ s(J_date, k = 6, by = year_fac), data = codlen1820)
mod2 <- mgcv::gam(TL ~ s(J_date, k = 6), data = codlen)
# # different curves for different years. mod1 better than mod2
AIC(mod1,mod2)
summary(mod1)


#### PLOTTING from script 2_plotting.R####
#Here going to plot for paper that Kdry by julian date
#only age-0 fish at cook by, 2018 and 2020


# Load the previous script
source("code/1_data_import.R")

head(codcond1)
distinct(codcond1,age)
distinct(codcond1, bay)
distinct(codcond1, gear)
distinct(codcond1, year) #only age-1 fish in 2019, so 2019 doesn't appear

codcond1 <- codcond1 %>%
  mutate(year_fac = as.factor(year),
         site_fac = as.factor(site),
         day_fac = as.factor(Julian_date))

## Figure 4d -plot to show males last to be in nursery area
head(codcond)
distinct(codcond, age) #age-0 and age-1
distinct(codcond1,age) #only age-0

D <- codcond1%>%
  ggplot(aes(x = Month, fill = as.factor(sex))) +
  geom_bar(width = 0.5)+
  theme_bw()+
  ylab("Count of age-0 Pacific cod")+
  scale_y_continuous(breaks = seq(from = 0, to = 100, by= 20), limits = c(0,100))+
  xlab("Month Captured")+
  labs(fill = "Sex")+
  theme(legend.position = c(0.8,0.7))
plot(D)
ggsave("./figs/sex_by_month.png", width = 6, height = 4, units = 'in')

##scatterplot LW##
C <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.6))+
  labs(x = "Total length (mm)", y = "Body Weight (g)")+
  geom_smooth(method="loess", formula = y ~ log(x), color=1) 

plot(C)

##ICes reviewer wanted to see LW regression diff by year____
distinct(codcond1, year)
cod2018<- filter(codcond1,year==2018)
cod2020<- filter(codcond1, year ==2020)
Creview <- codcond1 %>%
  ggplot(aes(x = TL, y = wgt_total, color = Month)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_bw() +
  geom_smooth(method = "gam", data=cod2018, aes(x=TL, y = wgt_total), 
              color= "red") +
  geom_smooth(method = "gam", data=cod2020, aes(x=TL, y = wgt_total), 
              color= "green")+
  xlab("Total length (mm)")+
  ylab("Body weight (g)")

plot(Creview)

mod1 <- mgcv::gam(formula = wgt_total ~ s(TL, k = 6, by = year_fac), data = codcond1)
mod2 <- mgcv::gam(wgt_total ~ s(TL, k = 6), data = codcond1)
# # different curves for different years. mod1 better than mod2
AIC(mod1,mod2)
summary(mod1)

mod3 <- mgcv::gam(formula = wgt_total ~ s(TL, k = 6, by = Month), data = codcond1)
summary(mod3)
##########
# try and match color in plot A and B to figure 6

my.col = cb[c(2,6)]


# plot Fulton dry by year and Julian day
B <- ggplot(codcond1, aes(Julian_date, Kdry, color = year_fac)) +
  geom_point(size = 2) +
  theme_bw()+
  theme(legend.position = c(0.2, 0.18)) +  
  theme(legend.title = element_blank()) +
  scale_colour_discrete(name = "Year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)+
  labs(y = "Fulton's Condition (K dry)", x = "Day of Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot(B)

mod1 <- mgcv::gam(Kdry ~ s(Julian_date, k = 6, by = year_fac), data = codcond1)
mod2 <- mgcv::gam(Kdry ~ s(Julian_date, k = 6), data = codcond1)
# # different curves for different years. mod1 better than mod2
AIC(mod1,mod2)
summary(mod1)



Fig4 <- ggarrange(A, B, C, D,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)

Fig4
#ggsave("./figs/Figure4review.png", width = 8, height = 8.5, units = 'in')
ggsave("./figs/Figure4color.png", width = 8, height = 8.5, units = 'in')
