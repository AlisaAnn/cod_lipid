# This is the data import script for ocean temp in cook bay

library(tidyverse)
library(lubridate)

# Read in the data and rename any columns that need renaming
cooktemp <- read_csv("data/DailyTempAvg1.csv")
head(cooktemp)

cooktemp <- cooktemp %>%
  rename("Avg_Temp" = "Avg Temp")

head(cooktemp)
tail(cooktemp)

##can see that last rows are all NA. Must remove these last 3 rows
cooktemp <-filter(cooktemp, !is.na(year))


tail(cooktemp) #you can see it worked 

#need to remove years 2021
cooktemp<-filter(cooktemp,year=="2018"|year=="2019" | year == "2020") # year %in% c("2018", "2019", "2020")
distinct(cooktemp, year)


#want to plot Avg Temp by date with year dif color
cooktemp <- cooktemp %>%
  mutate(year_fac = as.factor(year))

cooktemp <- cooktemp %>%
  mutate(Avg_Temp = as.numeric(Avg_Temp))

str(cooktemp)

ggplot(cooktemp, aes(J_date, Avg_Temp, color = year_fac)) +
  geom_point(alpha = 0.2) +
  theme_bw() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = F)

ggsave("./figs/Cook_Bay_Temp1.png", width = 6, height = 4, units = 'in')

# fit gam to predict maximum, etc. 
drop <- is.na(cooktemp$Avg_Temp)
cooktemp <- cooktemp[!drop,]

# check
sum(is.na(cooktemp$Avg_Temp))

mod_2018 <- mgcv::gam(Avg_Temp ~ s(J_date, k = 8), data = filter(cooktemp, year_fac == 2018))

plot(mod_2018, resid = T, pch = 19, se = F)

max(predict(mod_2018, se.fit = F))
## 2019
mod_2019 <- mgcv::gam(Avg_Temp ~ s(J_date, k = 8), data = filter(cooktemp, year_fac == 2019))

plot(mod_2019, resid = T, pch = 19, se = F)

max(predict(mod_2019, se.fit = F))

## 2020
mod_2020 <- mgcv::gam(Avg_Temp ~ s(J_date, k = 8), data = filter(cooktemp, year_fac == 2020))

plot(mod_2020, resid = T, pch = 19, se = F)

max(predict(mod_2020, se.fit = F))

##Mike make plot better by only showing curve and not datapoints?
##Mike test if years different in cooktemp?