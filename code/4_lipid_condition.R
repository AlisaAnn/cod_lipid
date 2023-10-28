# This is the plotting of condition and FA data
##no dried weights, so no Kdry or HSIdry here. go to Rscript2 for those


# Libraries
library(patchwork)
library(mgcv)

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
view(codFA)

#for poster and paper - compare liver FA with HSIwet
CL <- ggplot(data = codFA,
       aes(x = HSIwet,
           y = liverFA)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_bw()+
  labs(y = "% liver FA", x = "Hepatosomatic Index (HSI wet)") +
  #geom_smooth(method="loess", formula = y ~ log(x), color=1) 
  geom_smooth(method="lm", formula= (y ~ x), color=1) 
plot(CL)
ggsave("./figs/HSIwet_liverFA.png", width = 6, height = 4, units = 'in') 
a <- lm(formula = liverFA ~ HSIwet, data = codFA)
summary (a)
##this shows R^2 = 0.7314, n = 196 for a linear model
##y = 17.1x - 6.1

#a <- loess(formula = liverFA ~ log(HSIwet), data = codFA)
#summary (a)
# a loess: local polynomial regression fitting for < 1000 observations
#and I am not sure this is any better. how to get R^2 for this?
#plus, I like the linear model so it can be used by other studies w equation

#ICES reviewer wants data outlier point removed and another line for HSI<3
is.numeric(codFA$HSIwet)
max(codFA$HSIwet) #current max is 4.855
HSI3<- codFA[,22:28]
view(HSI3[141,])  #this is the row I want to remove
HSI3 <- HSI3[-141,]
max(HSI3$HSIwet)
##this totally worked - new max is 2.8 for HSI

plot(HSI3$HSIwet, HSI3$liverFA)
plot(codFA$HSIwet, codFA$liverFA)

HSI3.plot <- ggplot(data = HSI3,
             aes(x = HSIwet,
                 y = liverFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()+
  labs(y = "% liver FA", x = "Hepatosomatic Index when < 3 (HSI wet)") +
  #geom_smooth(method="loess", formula = y ~ log(x), color=1) 
  geom_smooth(method="lm", formula= (y ~ x), color=1) 
plot(HSI3.plot)

ggsave("./figs/HSI3wet_liverFA_new.png", width = 6, height = 4, units = 'in') 
a <- lm(formula = liverFA ~ HSIwet, data = HSI3)
summary (a)
##this shows R^2 = 0.677, n = 195 for a linear model

##To plot both regressions on a single plot do the following
p <- ggplot(data = codFA,
            aes(x = HSIwet,
                y = liverFA)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()+
  labs(y = "% liver FA", x = "Hepatosomatic Index (HSI wet)")+
  geom_smooth(method = "lm", data=HSI3, aes(x=HSIwet, y = liverFA), 
                            color= "red") + 
  geom_smooth(method = "lm", data=codFA, aes(x=HSIwet,y=liverFA), color = "grey", 
              alpha = 0.3) 
p

##Above is what I gave to reviewers at ICES

#for paper - compare actual liver FA concentration (FA density) with HSIwet
#Feb 6, after poster input where request for actual values of lipid density
CM <- ggplot(data = codFA,
       aes(x = HSIwet,
           y = liver_FA_conc)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_bw()+
  labs(y = "FA-liver (mg FA/g wwt)", x = "Hepatosomatic Index (HSI wet)") +
  geom_smooth(method="lm", formula= (y ~ x), color=1) 
plot(CM)
b <- lm(formula = liver_FA_conc ~ HSIwet, data = codFA)
summary (b)
 ##this y = 47.856x - 1.587, R^2 = 0.5247, n = 196 for a linear model

ggsave("./figs/HSIwet_liver_FA_concentration.png", width = 6, height = 4, units = 'in')

##Toward second plot for ICES reviewer, use the following
p <- ggplot(data = codFA,
            aes(x = HSIwet,
                y = liver_FA_conc)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_bw()+
  labs(y = "FA-liver(mg FA/g wwt)", x = "Hepatosomatic Index (HSI wet)")+
  geom_smooth(method = "lm", data=HSI3, aes(x=HSIwet, y = liver_FA_conc), 
              color= "red") + 
  geom_smooth(method = "lm", data=codFA, aes(x=HSIwet,y=liver_FA_conc), color = "grey", 
              alpha = 0.3) 
p

b3 <- lm(formula = liver_FA_conc ~ HSIwet, data = HSI3)
summary (b3)
##this y = 45.703x + 0.029, R^2 = 0.4269, n = 195 for a linear model




#New Figure 5 - but now it is FIGURE 7
# Now do top and bottom plot of liver lipids by gross condition factors (HSIwet)
library(ggplot2)
library("ggpubr")

Condfigure <- ggarrange(CL, CM,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
Condfigure
ggsave("./Figs/conditionFig7new.png", width = 4, height = 6, units = 'in')
#this is new Figure 7. Condition

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
##___________looking at HSI, liverFA, and  muscle by J_date_____

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

###
mod2a <- gam(liver_bi ~ s(J_date, k = 4) + year_fac, data = codFA,
            family = quasibinomial)
summary(mod2a)

new_dat <- data.frame(year_fac = as.factor(rep(c("2018", "2020"), each = 100)),
                      J_date = c(seq(min(codFA$J_date[codFA$year_fac=="2018"]),
                                     max(codFA$J_date[codFA$year_fac=="2018"]),
                                     length.out = 100),
                                 seq(min(codFA$J_date[codFA$year_fac=="2020"]),
                                     max(codFA$J_date[codFA$year_fac=="2020"]),
                                     length.out = 100)))
                                           


pred <- predict(mod2a, se.fit = F, new.data = new_dat)

plot_dat <- new_dat %>%
  mutate(predict = pred)

ggplot(plot_dat, aes(J_date, predict), color = year_fac) +
  geom_line()


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
##above is better because n = 419

