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
