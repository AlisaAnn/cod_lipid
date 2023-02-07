# This is second MuMlin analysis that is using the liver_FA_conc
# which is the Fatty acid or energy concentration per gram of liver tissue
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

mod1 <- gam(EDliver ~ s(J_date, k = 4) +
              s(TL, k = 4) + year_fac, data = codFA,
            family = gaussian)
plot(mod1)

summary(mod1)

mod2 <- gam(EDliver ~ s(TL, k = 4) + year_fac, data = codFA, family = gaussian)
summary(mod2)
plot(mod2, se = F, resid = T, pch = 19)


mod3 <- gam(EDliver ~ s(J_date, k = 4) + year_fac, data = codFA, family = gaussian)
summary(mod3)
plot(mod3, se = F, resid = T, pch = 19)

install.packages('evaluate')
install.packages('hexbin')

mod4 <- gam(EDliver ~ s(TL, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod4)

MuMIn::AICc(mod1, mod2, mod3, mod4) 

# separate curves for each effect in each year
mod5 <- gam(EDliver ~ s(J_date, k = 4, by = year_fac) +
              s(TL, k = 4, by = year_fac), data = codFA, family = gaussian)
summary(mod5)
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


