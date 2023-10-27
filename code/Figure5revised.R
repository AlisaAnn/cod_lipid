# This is Fig 5 in paper

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


library(mgcv)
library(ggplot2)
library("ggpubr")

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
              alpha = 0.4,
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

test <- plot(modED1fig$gam, residuals = T, pch = 21, cex = 1)

# https://stat.ethz.ch/pipermail/r-help/2011-February/269005.html

new_EDdat <- new_dat %>%
  mutate(log_ED = plot_EDdat$fit,
         LCI = log_ED-1.096*plot_dat$se.fit,
         UCI = log_ED+1.096*plot_dat$se.fit)

my.EDcol = cb[c(2,6)]

ED1 <- ggplot(new_EDdat, aes(J_date, log_ED, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.4,
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
##now plot submitted best model (mod11) for percent liver
##plot is revised where it is BY year (plot looks the same. it is really table 3 that has changed)
modLfig <- gamm4::gamm4((log(liver_bi + 1)) ~ s(J_date, k = 6, by = year_fac), data = codFA,
                        random=~(1|site_fac/day_fac))
summary(modLfig$gam)
my.Lcol = cb[c(2,6)]

New_logL <- ggplot(codFA, aes(J_date, liver_bi, color = year_fac, fill = year_fac)) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank()) +
  ylab("log (% liver FA)") +
  xlab("Day of year") +
  scale_color_manual(values = my.Lcol) +
  scale_fill_manual(values = my.Lcol)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)
plot(New_logL)

###############
##now plot best model (mod9) for percent muscle
modMfig <- gamm4::gamm4((log(muscle_bi + 1) ~ s(J_date, k = 6, by = year_fac)), data = codFA,
                        random=~(1|site_fac/day_fac))


summary(modMfig$gam)

#######
New_logM <- ggplot(codFA, aes(J_date, muscle_bi, color = year_fac, fill = year_fac)) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.3),
        legend.title = element_blank()) +
  ylab("log (% muscle FA)") +
  xlab("Day of year") +
  scale_color_manual(values = my.Lcol) +
  scale_fill_manual(values = my.Lcol)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)
plot(New_logM)



FAfigure <- ggarrange(HSI3, ED1, New_logL, New_logM,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
FAfigure
ggsave("./Figs/liver_Fig5new1.png", width = 7, height = 5, units = 'in')




