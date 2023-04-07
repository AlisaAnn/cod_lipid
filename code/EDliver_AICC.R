# This is second MuMlin analysis that is using the liver_FA_conc
# which is the Fatty acid or energy concentration per gram of liver tissue
#I am calling this variable EDliver, for energy density of the liver
#based off Rscript 4lipid_condition.R which does same analysis but for HSIwet

# Libraries
library(patchwork)

# Load the previous script
source("code/1_data_import.R")

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
summary(mod1a)

#AIC shows that Model 1a best, with adj. R^2 = 0.262
##I want to stop here and see what Mike thinks about new model approach with leaving length out.

###_________now redo the HSI_wet GAMS
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


modH1a <- gam(log(HSIwet) ~ s(J_date, k = 5) + year_fac, data = codFA,
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

mod9 <- gam(formula = log(liver_bi + 1) ~ s(J_date, k = 4) + year_fac, family = "quasibinomial", data = codFA)
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
##based on simplicity of model, going to say mod 9 the best

predict(mod11, type = "response", se.fit = TRUE)

##Now need to make Figure for paper
# plot liver FA and muscle FA by year and Julian day
library(ggplot2)
library("ggpubr")

# refit with gamm4 to account for non-independence within nested site/set
# and plot the best models from AIC which is modH3 for HSI
codFA <- codFA %>%
  mutate(year_fac = as.factor(Year),
         site_fac = as.factor(`site #`),
         day_fac = as.factor(J_date),
         log.HSI.wet = log(HSIwet))


modH3fig <- gamm4::gamm4(log.HSI.wet ~ s(J_date, k = 4, by = year_fac), data = codFA,
           random=~(1|site_fac/day_fac))


summary(modH3fig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_dat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                      J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                 seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))

# now predict HSIwet for these covariates
plot_dat <- predict(modH3fig$gam, newdata = new_dat, type = "response", se.fit = T)

new_dat <- new_dat %>%
  mutate(log_HSIwet = plot_dat$fit,
         LCI = log_HSIwet-1.096*plot_dat$se.fit,
         UCI = log_HSIwet+1.096*plot_dat$se.fit)

my.col = cb[c(2,6)]

HSI3 <- ggplot(new_dat, aes(J_date, log_HSIwet, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank()) +
  ylab("log(HSI wet)") +
  xlab("Day of year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col)
plot(HSI3)
anova(modH3fig$gam)

ggsave("./Figs/HSIwet_vs_day_new.png", width = 6, height = 3, units = 'in')

##now plot best model (mod1a) for EDliver
modED1fig <- gamm4::gamm4((log(EDliver) ~ s(J_date, k = 6) + year_fac), data = codFA,
                         random=~(1|site_fac/day_fac))


summary(modED1fig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_EDdat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                      J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                 seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))

# now predict HSIwet for these covariates
plot_EDdat <- predict(modED1fig$gam, newdata = new_EDdat, type = "response", se.fit = T)

new_EDdat <- new_dat %>%
  mutate(log_ED = plot_EDdat$fit,
         LCI = log_ED-1.096*plot_dat$se.fit,
         UCI = log_ED+1.096*plot_dat$se.fit)

my.EDcol = cb[c(2,6)]

ED1 <- ggplot(new_EDdat, aes(J_date, log_ED, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank()) +
  ylab("log (FA-liver)") +
  xlab("Day of year") +
  scale_color_manual(values = my.EDcol) +
  scale_fill_manual(values = my.EDcol)
plot(ED1)
anova(modED1fig$gam)

################
##now plot best model (mod9) for percent liver
modLfig <- gamm4::gamm4((log(liver_bi + 1) ~ s(J_date, k = 4) + year_fac), data = codFA,
                          random=~(1|site_fac/day_fac))


summary(modLfig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_Ldat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                        J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                   seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))

# now predict liver percent for these covariates
plot_Ldat <- predict(modLfig$gam, newdata = new_Ldat, type = "response", se.fit = T)

new_Ldat <- new_dat %>%
  mutate(log_L = plot_Ldat$fit,
         LCI = log_L-1.096*plot_Ldat$se.fit,
         UCI = log_L+1.096*plot_Ldat$se.fit)

my.Lcol = cb[c(2,6)]

Lnew <- ggplot(new_Ldat, aes(J_date, log_L, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank()) +
  ylab("log (% liver FA)") +
  xlab("Day of year") +
  scale_color_manual(values = my.Lcol) +
  scale_fill_manual(values = my.Lcol)
plot(Lnew)
anova(modLfig$gam)

###############
##now plot best model (mod9) for percent muscle
modMfig <- gamm4::gamm4((log(muscle_bi + 1) ~ s(J_date, k = 4) + year_fac), data = codFA,
                        random=~(1|site_fac/day_fac))


summary(modMfig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_Mdat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                       J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                  seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))

# now predict liver percent for these covariates
plot_Mdat <- predict(modMfig$gam, newdata = new_Mdat, type = "response", se.fit = T)

new_Mdat <- new_Mdat %>%
  mutate(log_M = plot_Mdat$fit,
         LCI = log_M-1.096*plot_Mdat$se.fit,
         UCI = log_M+1.096*plot_Mdat$se.fit)

my.Mcol = cb[c(2,6)]

Mnew <- ggplot(new_Mdat, aes(J_date, log_M, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.2),
        legend.title = element_blank()) +
  ylab("log (% muscle FA)") +
  xlab("Day of year") +
  scale_color_manual(values = my.Mcol) +
  scale_fill_manual(values = my.Mcol)
plot(Mnew)
anova(modMfig$gam)
###
M <- ggplot(codFA, aes(J_date, muscleFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Fatty Acids in Muscle", x = "Day of Year") +
  theme(legend.position = c(0.3, 0.25))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)

plot(M)
##plots M and L have actual data points rather than smoothed gam
L <- ggplot(codFA, aes(J_date, liverFA, color = year_fac)) +
  geom_point(size = 3) +
  theme_bw()+
  labs(y = "% Fatty Acids in Liver", x = "Day of Year") +
  theme(legend.position = c(0.2, 0.75))+
  scale_colour_discrete(name = "Year") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)
plot(L)

FAfigure <- ggarrange(HSI3, ED1, Lnew, Mnew,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
FAfigure
ggsave("./Figs/liver_Fig5new1.png", width = 7, height = 5, units = 'in')
#this is for paper Fig 5


