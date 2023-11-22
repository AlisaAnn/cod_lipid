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
         LCI = log_HSIwet-1.96*plot_dat$se.fit,
         UCI = log_HSIwet+1.96*plot_dat$se.fit)

# get partial residuals for plotting
pred_modH3 <- data.frame(year_fac = as.factor(codFA$Year),
                          J_date = codFA$J_date,
                         log_HSIwet = predict(modH3fig$gam, type = "response") +
                            residuals(modH3fig$gam, type = "response"))

my.col = cb[c(2,6)]

HSI3 <- ggplot(new_dat, aes(J_date, log_HSIwet, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.4,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
   #     legend.position = c(0.2, 0.8),
  legend.title = element_blank()) +
  ylab("log (HSI wet)") +
  xlab("Day of year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col) +
  geom_point(data = pred_modH3, alpha = 0.3)

plot(HSI3)

anova(modH3fig$gam)

ggsave("./Figs/HSIwet_vs_day_new.png", width = 6, height = 3, units = 'in')

##now plot best model (mod1a) for EDliver ------------------------------------
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

new_EDdat <- new_EDdat %>%
  mutate(log_ED = plot_EDdat$fit,
         LCI = log_ED-1.96*plot_EDdat$se.fit,
         UCI = log_ED+1.96*plot_EDdat$se.fit)

# get partial residuals for plotting
pred_modED1 <- data.frame(year_fac = as.factor(codFA$Year),
                          J_date = codFA$J_date,
                          log_ED = predict(modED1fig$gam, type = "response") +
                            residuals(modED1fig$gam, type = "response"))


my.EDcol = cb[c(2,6)]

ED1 <- ggplot(new_EDdat, aes(J_date, log_ED, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.4,
              lty = 0) +
  # facet_wrap(~facet, scales = "free_x") +
  theme(axis.title.x = element_blank(),
   #     legend.position = c(0.2, 0.8),
  legend.title = element_blank()) +
  ylab("log (FA-liver)") +
  xlab("Day of year") +
  scale_color_manual(values = my.EDcol) +
  scale_fill_manual(values = my.EDcol) +
  geom_point(data = pred_modED1, alpha = 0.3)

plot(ED1)

anova(modED1fig$gam)

################
##now plot submitted best model (mod11) for percent liver
##plot is revised where it is BY year (plot looks the same. it is really table 3 that has changed)
modLfig <- gamm4::gamm4((log(liver_bi + 1)) ~ s(J_date, k = 6, by = year_fac), data = codFA,
                        random=~(1|site_fac/day_fac))
summary(modLfig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_Ldat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                        J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                   seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))


# now predict for these covariates
plot_Ldat <- predict(modLfig$gam, newdata = new_Ldat, type = "response", se.fit = T)


new_Ldat <- new_Ldat %>%
  mutate(liver_bi = plot_Ldat$fit,
         LCI = liver_bi - 1.96*plot_Ldat$se.fit,
         UCI = liver_bi + 1.96*plot_Ldat$se.fit)

# get partial residuals for plotting
pred_modL <- data.frame(year_fac = as.factor(codFA$Year),
                          J_date = codFA$J_date,
                          liver_bi = predict(modLfig$gam, type = "response") +
                            residuals(modLfig$gam, type = "response"))



my.Lcol = cb[c(2,6)]

New_logL <- ggplot(new_Ldat, aes(J_date, liver_bi, color = year_fac, fill = year_fac)) +
  theme_bw()+
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.4,
              lty = 0) +
  # geom_point(alpha = 0.3) +
#  theme(axis.title.x = element_blank(),
 #       legend.position = c(0.2, 0.8),
        theme(legend.title = element_blank()) +
  ylab("% Liver FA") +
  xlab("Day of Year") +
  scale_color_manual(values = my.Lcol) +
  scale_fill_manual(values = my.Lcol)+
  # geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T) +
  geom_point(data = pred_modL, alpha = 0.3)


plot(New_logL)

###############
##now plot best model (mod9) for percent muscle
modMfig <- gamm4::gamm4((log(muscle_bi + 1) ~ s(J_date, k = 6, by = year_fac)), data = codFA,
                        random=~(1|site_fac/day_fac))


summary(modMfig$gam)

# set up a plot - predict from modH3fig$gam over the range of days observed in each year
new_Mdat <- data.frame(year_fac = as.factor(rep(c(2018,2020), each = 100)),
                       J_date = c(seq(min(codFA$J_date[codFA$Year==2018]), max(codFA$J_date[codFA$Year==2018]), length.out = 100),
                                  seq(min(codFA$J_date[codFA$Year==2020]), max(codFA$J_date[codFA$Year==2020]), length.out = 100)))


# now predict for these covariates
plot_Mdat <- predict(modMfig$gam, newdata = new_Mdat, type = "response", se.fit = T)


new_Mdat <- new_Mdat %>%
  mutate(muscle_bi = plot_Mdat$fit,
         LCI = muscle_bi - 1.96*plot_Mdat$se.fit,
         UCI = muscle_bi + 1.96*plot_Mdat$se.fit)

# get partial residuals for plotting
pred_modM <- data.frame(year_fac = as.factor(codFA$Year),
                        J_date = codFA$J_date,
                        muscle_bi = predict(modMfig$gam, type = "response") +
                          residuals(modMfig$gam, type = "response"))




my.Mcol = cb[c(2,6)]
#######
New_logM <- ggplot(new_Mdat, aes(J_date, muscle_bi, color = year_fac, fill = year_fac)) +
  theme_bw()+
  xlab("Day of Year") +
    ylab("% Muscle FA") +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.4,
              lty = 0) +
  #theme(axis.title.x = element_blank(),
  #legend.position = c(0.2, 0.3),
     theme(legend.title = element_blank()) +
  scale_color_manual(values = my.Mcol) +
  scale_fill_manual(values = my.Mcol)+
  # geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = T)
  geom_point(data = pred_modM, alpha = 0.3)

plot(New_logM)

png("./Figs/liver_Fig6revised.png", width = 7, height = 5, units = 'in', res = 300)


Fig6 <- ggarrange(HSI3, ED1, New_logL, New_logM, 
                      labels = c("A", "B", "C", "D"), 
                      ncol = 2, nrow = 2, legend = c("bottom"), 
                  common.legend = T) + bgcolor("white")
Fig6

ggsave("./figs/Figure6revised.png", width = 6, height = 4, units = 'in')
dev.off()
#library(grid)
#Fig6new <- annotate_figure(Fig6, bottom = textGrob("Day of Year", gp = gpar(cex = 1.1)))
#Fig6new

ggsave("./figs/Figure6revised.png", width = 6, height = 4, units = 'in')
ggsave("./figs/Abookire_etal_Figure6.pdf", width = 6, height = 4)






###Trying to figure out why muscle only goes to 0.7 #in end, think it is OK
##plot points are residuals, not acutal data
ggplot(data = codFA,
       aes(x =muscle_bi,
           y = liver_bi))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() 

test <- data.frame(pred_modL = pred_modL$liver_bi, 
                   pred_modM = pred_modM$muscle_bi)
ggplot(data = test,
       aes(x = pred_modM,
           y = pred_modL))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal()
######
testM <- data.frame(pred_modM = pred_modM$muscle_bi,
                    act_m = codFA$muscle_bi)
head(testM)
ggplot(data = testM,
       aes(x = pred_modM,
           y = act_m))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal()                        
###do same for liver
testL <- data.frame(pred_modL = pred_modL$liver_bi,
                    act_l = codFA$liver_bi)
head(testL)
ggplot(data = testL,
       aes(x = pred_modL,
           y = act_l))+
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal()       
