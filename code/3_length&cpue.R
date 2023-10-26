# This is the plotting & analysis script for LENGTH and CPUE

# Libraries
library(patchwork)
## mike I need help here: is MuMIN package nested in another library?

getOption("defaultPackages")
library (MuMIn) #this doesn't work because MuMIN wasn't updated. WOrks on Mike's 4.1 version of Rscript

library(tidyverse)

# Load the previous script
source("code/1_data_import.R")

theme_set(theme_bw())
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### PLOTTING ####
head(codlen)
##if need to rename columns, you would type 
codlen <- rename(codlen,"TL" = "Length (mm)") 
head(codlen)

##LF for all 3 years, all months in Cook Bay
ggplot(codlen, aes(TL, fill=Month)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,250)+
  facet_wrap(~year)

##now that I have the 4 age-0 lengths in 2019, the density plot is wild.
##instead plot geom_histogram

ggplot(codlen, aes(TL, fill=Month)) +
    geom_histogram(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,250)+
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "cyan",
    "purple","grey", "black"))+
  theme_bw() +
  facet_wrap(~year)

##LF for all 3 years, months May - Aug in Cook Bay and max TL 200
codlen1 <- filter(codlen, month != "Dec", month != "Nov", month != "Oct", month != "Sept")

ggplot(codlen1,aes(TL, fill=Month)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,250)+
  facet_wrap(~year)

distinct (codlen, month)

ggplot(codlen1, aes(TL, fill=Month)) +
  geom_histogram(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(0,210)+
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "cyan",
                               "purple","grey", "black"))+
  theme_bw() +
  facet_wrap(~year)

##LF for all 3 years, months Sept - Dec in Cook Bay and max TL 250
codlen2 <- filter(codlen, month == "Dec"| month == "Nov"| 
                  month == "Oct"| month == "Sept")
distinct(codlen2, month)

ggplot(codlen2,aes(TL, fill=Month)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(50,200)

ggplot(codlen2, aes(TL, fill=Month)) +
  geom_histogram(alpha=0.5) +
  xlab("Total Length (mm)")+
  xlim(50,200)+
  scale_fill_manual(values = c("cyan",
                               "purple","grey", "black"))+
  theme_bw() +
  facet_wrap(~year)


##plot length by julian date instead of histograms, as per Mike's idea 12/2/22
codlen <- codlen %>%
  mutate(year_fac = as.factor(year))

ggplot(codlen, aes(J_date, TL, color = year_fac)) +
  geom_point() +
  theme_bw()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = F)

ggsave("./figs/length_by_date.png", width = 6, height = 4, units = 'in')


# above plot has 2019. want to plot 2018-2020 only
ggplot(filter(codlen, year_fac %in% c(2018, 2020) & TL < 200), aes(J_date, TL, color = year_fac)) +
  geom_point() +
  theme_bw()+
  theme(legend.position = c(0.2, 0.7))+
  scale_colour_discrete(name = "Year") +
  labs(x = "Day of Year", y = "Total Length (mm)")+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), se = F)

ggsave("./figs/length_by_date_2018_2020.png", width = 6, height = 4, units = 'in')

head(codlen)
mod1 <- mgcv::gam(TL ~ s(J_date, k = 6, by = year_fac), data = codlen)
mod2 <- mgcv::gam(TL ~ s(J_date, k = 6), data = codlen)
#MuMIn::AICc(mod1, mod2) # different curves for different years = better model
AIC(mod1,mod2)
summary(mod1)

##But want to run this without 2019. 
codlen1820 <- filter(codlen, year == "2018" | year == "2020")
#above makes df without 2019
distinct(codlen1820, year)
mod1 <- mgcv::gam(TL ~ s(J_date, k = 6, by = year_fac), data = codlen1820)
mod2 <- mgcv::gam(TL ~ s(J_date, k = 6), data = codlen)
# # different curves for different years. mod1 better than mod2
AIC(mod1,mod2)
summary(mod1)


ggplot(data = codlen, aes(x = J_date, y = TL, color = year_fac)) +
  geom_boxplot(width = 0.3) +
  geom_jitter(alpha = 0.5)+
  theme(legend.position = "bottom") +
  theme_bw()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = F) +
  facet_wrap(~year)
#doesn't really work to have boxplot with continuous xaxis variable. stick with above

###############PLOTS of CPUE##

# Load the previous script
source("code/1_data_import.R")

##all years, age-0 only
head(codcpue)
codcpue <- rename(codcpue,"Cod" = "Cod_CPUE") 
head(codcpue)
ggplot(data = codcpue,
       aes(x = Month,
           y = Cod, color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  coord_trans(y = "pseudo_log") +
  scale_y_continuous(breaks=c(0, 1,10,20,50, 100,200, 500, 1000, 1500),
                     minor_breaks = NULL) +
  ylab("Age-0 Pacific cod")

## explore abundance by day by year -----------------------------

library(mgcv)

# log-transform cpue
codcpue <- codcpue %>%
  mutate(log_cpue = log(Cod+1),
         year_fac = as.factor(year))


# plot cpue by year and Julian day
ggplot(codcpue, aes(Julian_date, log_cpue, color = year_fac)) +
  geom_point() +
  theme_bw()+
  theme(legend.position = c(0.2, 0.5)) +
  scale_colour_discrete(name = "Year") +
  labs(x = "Day of year", y = "Log scale age-0 CPUE" ) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F) + 
  annotate("text", x = c(45, 60), y = rep(-0.5, 2), label = c("Feb", "Mar"), color = "dark grey")



ggsave("./figs/logcpue_by_date.png", width = 6, height = 4, units = 'in')
  
codcpue <- codcpue %>%
  dplyr::mutate(year_day = paste(year, Julian_date, sep = "_")) 

sample_order <- codcpue %>% 
  group_by(year, Julian_date) %>%
  summarise(sample_number = n()) %>%
  select(-sample_number)

sample1 <- sample_order %>%
  filter(year == 2018)
  sample1$sample_order = 1:nrow(sample1)
  
sample2 <- sample_order %>%
  filter(year == 2019)
  sample2$sample_order = 1:nrow(sample2)

sample3 <- sample_order %>%
    filter(year == 2020)
  sample3$sample_order = 1:nrow(sample3)
  
sample_order <- rbind(sample1, sample2, sample3)

codcpue <- left_join(codcpue, sample_order)

# now we have sample order for each event in each year

# try to use this order in corAR1() structure

mod1 <- gamm4::gamm4(log_cpue ~ s(Julian_date, by = year_fac, k = 5) + year_fac, data = codcpue, 
                     random=~(1|year_fac/sample_order))

summary(mod1$gam) # predict from this model object

# set up a plot - predict from mod1$gam over the range of days observed in each year
new_dat <- data.frame(year_fac = as.factor(rep(c(2018, 2019, 2020), each = 100)),
                      Julian_date = c(seq(min(codcpue$Julian_date[codcpue$year==2018]), max(codcpue$Julian_date[codcpue$year==2018]), length.out = 100),
                                 seq(min(codcpue$Julian_date[codcpue$year==2019]), max(codcpue$Julian_date[codcpue$year==2019]), length.out = 100),
                                 seq(min(codcpue$Julian_date[codcpue$year==2020]), max(codcpue$Julian_date[codcpue$year==2020]), length.out = 100)))


# now predict CPUE for new_dat and plot
##This will be Figure 3 in paper
##added the months as annotated text
plot_dat <- predict(mod1$gam, newdata = new_dat, type = "response", se.fit = T)

new_dat <- new_dat %>%
  mutate(log_cpue = plot_dat$fit,
         LCI = log_cpue-1.096*plot_dat$se.fit,
         UCI = log_cpue+1.096*plot_dat$se.fit)

my.col = cb[c(2,4,6)]

CPUE <- ggplot(new_dat, aes(Julian_date, log_cpue, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  theme(legend.position = c(0.1, 0.85),
        legend.title = element_blank()) +
  ylab("ln(CPUE)") +
  xlab("Day of year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col) +
  geom_point(data = codcpue, aes(Julian_date, log_cpue, color = year_fac)) +
  annotate("text", x = c(50, 80, 110, 134, 163,  195,  230, 268, 290, 325), y = rep(-0.5, times=10), 
           label = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov Dec"), color = "dark grey")

plot(CPUE)

ggsave("./figs/CPUE_by_year.png", width = 6, height = 4, units = "in")
ggsave("./figs/Figure_3.png", width = 6, height = 4, units = "in")

anova(mod1$gam)
summary(mod1$gam)
##making another version of Fig 3
##wondering if better to have month text evenly spaced, as in figure 2
#These month text all spaced by 15th of each month
CPUE_2 <- ggplot(new_dat, aes(Julian_date, log_cpue, color = year_fac, fill = year_fac)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCI,
                  ymax = UCI), 
              alpha = 0.2,
              lty = 0) +
  theme(legend.position = c(0.1, 0.85),
        legend.title = element_blank()) +
  ylab("ln(CPUE)") +
  xlab("Day of year") +
  scale_color_manual(values = my.col) +
  scale_fill_manual(values = my.col) +
  geom_point(data = codcpue, aes(Julian_date, log_cpue, color = year_fac)) +
  annotate("text", x = c(46, 74, 105, 135, 166,  196,  227, 258, 288, 319, 349), y = rep(-0.5, times=11), 
           label = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), color = "dark grey")

plot(CPUE_2)

ggsave("./figs/CPUE_by_year_2.png", width = 6, height = 4, units = "in")
ggsave("./figs/Figure_3final.png", width = 6, height = 4, units = "in")
## ---------------------------------------

ggplot(data = codcpue,
       aes(x = Month,
           y = Cod, color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  ylab("Age-0 Pacific cod")+
  facet_wrap(~year)


##scale very different 2018 and 2020, so plot separately
codcpue18 <- filter(codcpue, year == 2018)
ggplot(data = codcpue18,
       aes(x = Month,
           y = Cod, color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  ylab("Age-0 Pacific cod")+
  xlab("Month (2018 only)")

codcpue19 <- filter(codcpue, year == 2019)
ggplot(data = codcpue19,
       aes(x = Month,
           y = Cod, color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  ylab("Age-0 Pacific cod")+
  xlab("Month (2019 only)")


codcpue20 <- filter(codcpue, year == 2020)
ggplot(data = codcpue20,
       aes(x = Month,
           y = Cod, color = Month)) +
  geom_boxplot(width = 0.3)+
  geom_jitter(alpha = 0.5)+
  theme_minimal()+
  ylab("Age-0 Pacific cod")+
  xlab("Month (2020 only)")

codcpue %>%
  ggplot(aes(x = Month, y = Cod, color = as.factor(year))) +
  geom_line()+
  theme_minimal()+
  ylab("Age-0 Pacific cod")

codcpue %>%
  ggplot(aes(x = Month, y = Cod, fill = as.factor(year))) +
  geom_col()+
  scale_fill_manual(values = c("darkorange","purple", "blue"))+
  theme_minimal()+
  ylab("Age-0 Pacific cod")


codcpue %>%
  ggplot(aes(x = Month, y = Cod, color = as.factor(year))) +
  geom_point()+
  scale_fill_manual(values = c("darkorange","purple", "blue"))+
  theme_minimal()+
  ylab("Age-0 Pacific cod")


## Figure 5 -plot to show males last to be in nursery area
head(codcond)
distinct(codcond, age) #age-0 and age-1
distinct(codcond1,age) #only age-0


codcond1%>%
  ggplot(aes(x = Month, fill = as.factor(sex))) +
  geom_bar()+
  theme_minimal()+
  ylab("Count of age-0 Pacific cod sex")+
  xlab("Month")

codcond1%>%
  ggplot(aes(x = Month, fill = as.factor(sex))) +
  geom_bar(width = 0.5)+
  theme_bw()+
  ylab("Number of age-0 Pacific cod")+
  scale_y_continuous(breaks = seq(from = 0, to = 100, by= 20), limits = c(0,100))+
  xlab("Month Captured")+
  labs(fill = "Sex")+
  theme(legend.position = c(0.9,0.7))

ggsave("./figs/sex_by_month.png", width = 6, height = 4, units = 'in')

##AA stopped here to try to get grey/black
codcond1%>%
  ggplot(aes(x = Month, fill = as.factor(sex))) +
  geom_bar(width = 0.5, scale_fill_manual(values = c("black", "grey", "white")))+
  theme_bw()+
  ylab("Number of age-0 Pacific cod", limits = c(0,100))+
  scale_y_continuous(breaks = seq(from = 0, to = 100, by= 20))+
  xlab("Month Captured")+
  labs(fill = "Sex")+
  theme(legend.position = c(0.9,0.7))




