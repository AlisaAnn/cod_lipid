# This is the plotting & analysis script for LENGTH and CPUE

# Libraries
library(patchwork)

# Load the previous script
source("code/1_data_import.R")


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
  theme_minimal()+
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), se = F)


mod1 <- gam(log_cpue ~ s(Julian_date, k = 4) + year_fac, data = codcpue)

summary(mod1)

plot(mod1, se = F, resid = T, pch = 19)

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


##try to plot to show males last to be in nursery area
head(codcond)
distinct(codcond, age) #age-0 and age-1
distinct(codcond1,age) #only age-0

codcond1 %>%
  ggplot(aes(x = Month, y = sex, fill = as.factor(sex))) +
  geom_count()+
  theme_minimal()+
  ylab("Count of age-0 Pacific cod sex")
 
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




