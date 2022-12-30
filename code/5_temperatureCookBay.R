# This is the data import script for ocean temp in cook bay

library(tidyverse)
library(lubridate)

# Read in the data and rename any columns that need renaming
cooktemp <- read_csv("data/DailyTempAvg1.csv")
head(cooktemp)
cooktemp <- rename(cooktemp, "Avg_Temp" = "Avg Temp")
head(cooktemp)
tail(cooktemp)
##can see that last rows are all NA. Must remove these last 3 rows
cooktemp <-filter(cooktemp, !is.na(year))
tail(cooktemp) #you can see it worked 

#need to remove years 2021
cooktemp<-filter(cooktemp,year=="2018"|year=="2019" | year == "2020")
distinct(cooktemp, year)


#want to plot Avg Temp by date with year dif color
cooktemp <- cooktemp %>%
  mutate(year_fac = as.factor(year))

cooktemp <- cooktemp %>%
  mutate(Avg_Temp_C = as.integer(Avg_Temp))
str(cooktemp)
ggplot(cooktemp, aes(J_date, Avg_Temp_C, color = year_fac)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = F)

ggsave("./figs/Cook_Bay_Temp1.png", width = 6, height = 4, units = 'in')

##Mike make plot better by only showing curve and not datapoints?
##Mike test if years different in cooktemp?