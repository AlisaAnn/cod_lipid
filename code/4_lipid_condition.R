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

#now scatterplot of HSIwet and whole body fatty acid
#I think this doesn't make sense, so stop
#ggplot(data = codFA,
   #    aes(x = HSIwet,
    #       y = wholeFA,
     #      color = Month)) +
  #geom_point(size = 3, alpha = 0.8) +
  #theme_minimal() 

#now scatterplot of Kwet_evic and whole body fatty acid
#ggplot(data = codFA,
   #    aes(x = Kwet_evic,
    #       y = wholeFA,
     #      color = Month)) +
  #geom_point(size = 3, alpha = 0.8) +
  #theme_minimal() 

#can we compare liver FA with HSIwet
ggplot(data = codFA,
       aes(x = HSIwet,
           y = liverFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

a <- lm(formula = HSIwet ~ liverFA, data = codFA)
summary (a)

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



