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
           color = Month)) +
geom_point(size = 3, alpha = 0.8) +
theme_minimal() 


#now scatterplot of HSIwet and whole body fatty acid
ggplot(data = codFA,
       aes(x = HSIwet,
           y = wholeFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 
ggsave("./figs/HSIwet_totalFA.png", width = 6, height = 4, units = 'in')

#now scatterplot of Kwet_evic and whole body fatty acid
ggplot(data = codFA,
       aes(x = Kwet_evic,
           y = wholeFA,
           color = Month)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

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

##___________looking at FA by J_date_____

library(mgcv)
codFA <- codFA %>%
  mutate(year_fac = as.factor(year)

mod1 <- gam(HSIwet ~ s(Julian_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codcond1,
            family = gaussian)
plot(mod1)


mod1 <- gam(liverFA ~ s(Julian_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codcond1,
            family = "quasibinomial")