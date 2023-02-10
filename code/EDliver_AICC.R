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


mod1b <- gam(log(EDliver) ~ s(J_date, k = 4) + year_fac, data = codFA,
            family = gaussian)
plot(mod1b, pages = 1, all.terms = TRUE)

summary(mod1b)

gam.check(mod1b)
AIC(mod1a, mod1b)
##I think histograms look good in both mod1a and mod1b
#AIC says to go with mod1a
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

mod3 <- gam(log(EDliver) ~ s(J_date, k = 5, by = year_fac), data = codFA,
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

# model 3 is the best (separate Julian day curves by year)
#summary(mod3)
#plot(mod3, resid = T, pch = 19)

##_________now redo the HSI_wet GAMS
ggplot(data= codFA, aes(HSIwet))+
  geom_histogram(fill = "grey", color = "black")
##data too skewed. maybe log transform data.
ggplot(data= codFA, aes(log(HSIwet)))+
  geom_histogram(fill = "grey", color = "black")
##  log transform looks better.  Also log trans better than sqrt
##could use a diff family that uses diff assumptions to data distribution, but best to transform data

modH1 <- gam(log(HSIwet) ~ s(J_date, k = 4) + year_fac, data = codFA,
             family = gaussian)
plot(modH1, pages = 1, all.terms = TRUE)

summary(modH1)

gam.check(modH1)
concurvity(modH1,full = TRUE)
## tried modelH1 with different K=6 was overfit, leave k=4


modH1a <- gam(log(EDliver) ~ s(J_date, k = 5) + year_fac, data = codFA,
             family = gaussian)
plot(modH1a, pages = 1, all.terms = TRUE)

summary(modH1a)

gam.check(modH1a)

##I think histograms look good in both modH1a and modH1
## I realize that more diff between k and edf with k=6, so prob use mod1a.
##and graphically the partial effect plots show overfit if K=6

#Now with model 2 remove year and then compare with model H1
modH2 <- gam(log(HSIwet) ~ s(J_date, k = 4), data = codFA, family = gaussian)
summary(modH2)
plot(modH2, se = F, pages = 1, all.terms = TRUE, resid = T, pch = 19)
#doesn't look great - close to horizontal
gam.check(modH2)

##mod2 is Not good. lower R^2 and the partial effect plot nearly horizontal. 

modH3 <- gam(log(HSIwet) ~ s(J_date, k = 4, by = year_fac), data = codFA,
            family = gaussian)
plot(modH3, pages = 1, all.terms = TRUE)

summary(modH3)

gam.check(modH3)
#gamcheck looks good. histogram of redis and QQ plot better when k=4

AIC(modH1, modH2, modH3)
#model H3 is lowest AIC and therefore the best fit

ggplot(data = codFA,
       aes(x = TL,
           y = HSIwet,
           color = Month))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

ggplot(data = codFA,
       aes(x = TL,
           y = EDliver,
           color = Month))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 
ggplot(data = codFA,
       aes(x = TL,
           y = J_date,
           color = Month))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 


##########now redo the quasibinomial models without length
# using liver_bi which is liver percent / 100 to yield value between 0 and 1.

ggplot(codFA, aes(liver_bi)) +
  geom_histogram(bins = 20)
#data skewed not sure how to fix with percent data.

ggplot(codFA, aes(sqrt(liver_bi+10))) +
  geom_histogram(bins = 20)
ggplot(codFA, aes(log(liver_bi+1))) +
  geom_histogram(bins = 20)
#log() is natural log and not base 10
ggplot(codFA, aes(log1p(liver_bi+1))) +
  geom_histogram(bins = 20)
ggplot(codFA, aes((liver_bi+1)^(1/3))) +
  geom_histogram(bins = 20)
ggplot(codFA, aes(log10(liver_bi+1))) +
  geom_histogram(bins = 20)
#I think log10() and log() are same and best chance
ggplot(codFA, aes(exp(liver_bi+1))) +
  geom_histogram(bins = 20)


ggplot(codFA, aes(J_date, liver_bi, color = year_fac)) +
  geom_point() +
  theme_minimal()+
  geom_smooth(method = "gam", family = "quasibinomial", formula = y ~ s(x, k = 4), se = F)
#this plot looks same as liverFA by julian date. only now yaxis from 0 to 1

mod9 <- gam(formula = log(liver_bi + 1) ~ s(J_date, k = 7) + year_fac, family = "quasibinomial", data = codFA)
plot(mod9, se = F, resid = T, pch = 19)
summary(mod9)
gam.check(mod9)

mod10 <- gam(formula = log(liver_bi +1) ~ s(J_date, k = 6), family = "quasibinomial", data = codFA)
plot(mod10, se = F, resid = T, pch = 19)
summary(mod10)  
gam.check(mod10) 

mod11 <- gam(formula = log(liver_bi +1) ~ s(J_date, k = 3, by = year_fac), family = "quasibinomial", data = codFA)
summary(mod11)
plot(mod11)
gam.check(mod11)

AIC(mod9,mod10, mod11)
###is there a way to evaluate quasibinomial models?
#can see that mod10 very poor with assumptions in gam.check

plot(mod9, pages = 1, all.terms = TRUE)
gam.check(mod9)

plot(mod11, pages = 1, all.terms = TRUE)
gam.check(mod11)
##based on normality of residuals, I think mod11 is the best

predict(mod11, type = "response", se.fit = TRUE)


##Now need to make Figure for paper

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

ggsave("./Figs/HSIwet_vs_day.png", width = 6, height = 3, units = 'in')


