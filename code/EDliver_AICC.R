# This is second MuMlin analysis that is using the liver_FA_conc
# which is the Fatty acid or energy concentration per gram of liver tissue
#I am calling this variable EDliver, for energy density of the liver
#based off Rscript 4lipid_condition.R which does same analysis but for HSIwet

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
codFA <- rename(codFA, "EDliver" = "liver_FA_conc")
head(codFA)

##begin MuMIn analysis to look at condition/lipid by Julian date
##___________looking at energy density in liver by J_date_____

library(mgcv)
codFA <- codFA %>%
  mutate(year_fac = as.factor(Year))

mod <- gam(log(EDliver) ~ s(J_date, k = 6) +
              s(TL, k =4) + year_fac, data = codFA,
            family = gaussian)
plot(mod, pages = 1, all.terms =  TRUE)

summary(mod)

gam.check(mod)
concurvity(mod,full = TRUE)
##worst concurvity = 0.88, which says we need to inspect model

#showed that need to increase K, as K=4 too low
# try mod with just day (mod1) because collinearity between date and total length

ggplot(data= codFA, aes(EDliver))+
  geom_histogram(fill = "grey", color = "black")
##data too skewed. maybe log transform data.
ggplot(data= codFA, aes(log(EDliver)))+
  geom_histogram(fill = "grey", color = "black")
##  log transform looks good.  Also log trans better than sqrt
##could use a diff family that uses diff assumptions to data distribution, but best to transform data


mod1a <- gam(log(EDliver) ~ s(J_date, k = 6) + year_fac, data = codFA,
             family = gaussian)
plot(mod1a, pages = 1, all.terms = TRUE)

summary(mod1a)

gam.check(mod1a)
concurvity(mod1a,full = TRUE)
## so far model 1a is best because TL removed and K = 6 better than K=4 or K=8
##going to try to increase K=7, as mod1b


mod1b <- gam(log(EDliver) ~ s(J_date, k = 7) + year_fac, data = codFA,
            family = gaussian)
plot(mod1b, pages = 1, all.terms = TRUE)

summary(mod1b)

gam.check(mod1b)

##I think histograms look good in both mod1a and mod1b
## I realize that more diff between k and edf with k=6, so prob use mod1a.
##and graphically the partial effect plots show overfit if K=7


#Now with model 2 remove year and then compare with model 1a
mod2 <- gam(log(EDliver) ~ s(J_date, k = 6), data = codFA, family = gaussian)
summary(mod2)
plot(mod2, se = F, pages = 1, all.terms = TRUE, resid = T, pch = 19)
#doesn't look great - close to horizontal
gam.check(mod2)

##mod2 is Not good. lower R^2 and the partial effect plot nearly horizontal. 

AIC(mod1a, mod2)
#AIC is lower for mod1a (no surprise) and that's the best one.

mod3 <- gam(log(EDliver) ~ s(J_date, k = 6, by = year_fac), data = codFA,
             family = gaussian)
plot(mod3, pages = 1, all.terms = TRUE)

summary(mod3)

gam.check(mod3)

AIC(mod1a, mod2, mod3)

#AIC shows that Model 3 best, with adj. R^2 = 0.306
##I want to stop here and see what Mike thinks about new model approach with leaving length out.

##Mike says to run length and let AIC decide
# separate curves for each effect in each year
mod5 <- gam(log(EDliver) ~ s(J_date, k = 4, by = year_fac) +
              s(TL, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod5)
gam.check(mod5)
plot(mod5, resid = T)
##EDF and K' almost equal. check for concurvity
concurvity(mod5,full = TRUE)
##shows a lot of concurvity.
##going to leave length out of model due to concurvity.






install.packages('evaluate')
install.packages('hexbin')

mod4 <- gam(EDliver ~ s(TL, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod4)

MuMIn::AICc(mod1, mod2, mod3, mod4) 

# separate curves for each effect in each year
mod5 <- gam(EDliver ~ s(J_date, k = 4, by = year_fac) +
              s(TL, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod5)
gam.check(mod5)
plot(mod5, resid = T)

# separate curves for day in each year
mod5b <- gam(EDliver ~ s(J_date, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod5b)
gam.check(mod5b)
plot(mod5, resid = T)

mod6 <- gam(EDliver ~ s(J_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codFA, family = gaussian)

plot(mod6, resid = T)
summary(mod6)

mod7 <- gam(EDliver ~ s(J_date, k = 4) +
              s(TL, k = 4), data = codFA, family = gaussian)
summary(mod7)

# MuMIn::AICc(mod5, mod6, mod7)
MuMIn::AICc(mod1, mod2, mod3, mod4, mod5, mod6, mod7)

# save AICc output
out <- as.data.frame(MuMIn::AICc(mod1, mod2, mod3, mod4, mod5, mod6, mod7))
write.csv(out, "./output/EDliver.csv", row.names = F)

#MIKE STOP HERE TO SEE WHICH MODEL BEST BEFORE PROCEED

# model 3 is the best (separate Julian day curves by year)
#summary(mod3)
#plot(mod3, resid = T, pch = 19)



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


