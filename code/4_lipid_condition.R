# This is the plotting of condition and FA data
##no dried weights, so no Kdry or HSIdry here. go to Rscript2 for those


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

#now scatterplot of HSIwet and Kwet. no relationship
ggplot(data = codFA,
       aes(x = HSIwet,
           y = Kwet_evic,
           color = Month))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 


ggsave("./figs/HSIwet_Kwet.png", width = 6, height = 4, units = 'in')  


#can we compare liver FA with HSIwet
ggplot(data = codFA,
       aes(x = HSIwet,
           y = liverFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

a <- lm(formula = HSIwet ~ liverFA, data = codFA)
summary (a)


#for poster and paper - compare liver FA with HSIwet
CL <- ggplot(data = codFA,
       aes(x = HSIwet,
           y = liverFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()+
  labs(y = "% Energy stored in liver", x = "Hepatosomatic Index (HSI wet)") +
  #geom_smooth(method="loess", formula = y ~ log(x), color=1) 
  geom_smooth(method="lm", formula= (y ~ x), color=1) 
plot(CL)
ggsave("./figs/HSIwet_liverFA.png", width = 6, height = 4, units = 'in') 
a <- lm(formula = liverFA ~ HSIwet, data = codFA)
summary (a)
##this shows R^2 = 0.7314, n = 196 for a linear model

a <- loess(formula = liverFA ~ log(HSIwet), data = codFA)
summary (a)
#this shows for a loess: localpolynomial regression fitting
#and I am not sure this is any better. how to get R^2 for this?

#for paper - compare actual liver FA concentration (FA density) with HSIwet
#Feb 6, after poster input where request for actual values of lipid density
CM <- ggplot(data = codFA,
       aes(x = HSIwet,
           y = liver_FA_conc)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()+
  labs(y = "Liver energy density (mg FA/g)", x = "Hepatosomatic Index (HSI wet)") +
  geom_smooth(method="lm", formula= (y ~ x), color=1) 
plot(CM)
b <- lm(formula = liver_FA_conc ~ HSIwet, data = codFA)
summary (b)
 ##this shows R^2 = 0.5247, n = 196 for a linear model

ggsave("./figs/HSIwet_liver_FA_concentration.png", width = 6, height = 4, units = 'in')

#New Figure 5
# Now do top and bottom plot of liver lipids by gross condition factors (HSIwet)
library(ggplot2)
library("ggpubr")

Condfigure <- ggarrange(CL, CM,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
Condfigure
ggsave("./Figs/conditionFig5.png", width = 4, height = 6, units = 'in')
#this is new Figure 5. Condition

#and I think I am going to omit this figure that has HSIwet and FultonK bc Kwet doesn't make sense anymore
#Kwet relates to protein and not to muscle FA
##compare muscleFA with Fulton Wet
#goint to omit this because Fultonwet is to protein in literature, not to muscle FA
#ggplot(data = codFA,
#      aes(x = Kwet,
#         y = muscleFA)) +
#  geom_point(size = 3, alpha = 0.8) +
#  labs(x = "Fulton's Index (K wet)", y = "% Muscle Fatty Acids") +
#  theme_bw() +
#  geom_smooth(method="lm", formula= (y ~ x), color=1) 

#ggsave("./figs/Kwet_muscleFA.png", width = 6, height = 4, units = 'in') 
#m <- lm(formula = Kwet ~ muscleFA, data = codFA)
#summary (m)
##this shows R^2 = 0.02824, n = 194


#for poster - compare TEMPERATURE w liver FA with HSIwet
ggplot(data = codFA,
       aes(x = temp,
           y = liverFA, color = Year)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(x = "Water temperature", y = "percent Liver Fatty Acid") +
  theme_bw() +
  geom_smooth(method="lm", formula= (y ~ x), color=1) 

ggsave("./figs/Temp_liverFA.png", width = 6, height = 4, units = 'in') 
b <- lm(formula = liverFA ~ temp, data = codFA)
summary (b)



#can we compare muscle FA with Kwet_evic
ggplot(data = codFA,
       aes(x = Kwet_evic,
           y = muscleFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

a <- lm(formula = Kwet_evic ~ muscleFA, data = codFA)
summary (a)

#can we compare liver FA with total length
ggplot(data = codFA,
       aes(x = TL,
           y = liverFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

b <- lm(formula = liverFA ~ TL, data = codFA)
summary (b)

c <- gam(formula = liverFA ~ TL, data = codFA)
summary (c)

##begin MuMIn analysis to look at condition/lipid by Julian date
##___________looking at HSI, liverFA, and muscle by J_date_____

library(mgcv)
codFA <- codFA %>%
  mutate(year_fac = as.factor(Year))

mod1 <- gam(HSIwet ~ s(J_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codFA,
            family = gaussian)
plot(mod1)

summary(mod1)
##based on mumlin, can run above without TL?

mod2 <- gam(liver_bi ~ s(J_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codFA,
            family = quasibinomial)
plot(mod2)
summary(mod2)

mod3 <- gam(muscle_bi ~ s(J_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codFA,
            family = quasibinomial)
plot(mod3)
summary(mod3)

##make a plot for AMSS poster w liver FA by month

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



FAfigure <- ggarrange(L, M,
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)
FAfigure
ggsave("./Figs/liverFA_poster.png", width = 6, height = 4, units = 'in')
#this is for poster

##Last thing to add to LW curve in paper
##evaluate if lower body weight at length in Nov and Dec

head(codFA)
distinct(codFA,age)
distinct(codFA,Year)

ggplot(data = codFA,
       aes(x = TL,
           y = wwt)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)


mod1 <- gam(wwt ~ s(TL, k = 4) + Month, data = codFA,
            family = gaussian)
plot(mod1)
summary(mod1)
#not happy that sample size only 196 fish (above)

#try again here with codcond1 dataset
head(codcond1)
distinct(codcond1,age)
distinct(codcond1,year)

ggplot(data = codcond1,
       aes(x = TL,
           y = wgt_total, color = month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)


mod1 <- gam(wgt_total ~ s(TL, k = 4) + month, data = codcond1,
            family = gaussian)
plot(mod1)
summary(mod1)
##above is better