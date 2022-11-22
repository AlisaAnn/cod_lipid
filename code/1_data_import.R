# This is the data import script

library(tidyverse)
library(lubridate)



# Read in the data and rename any columns that need renaming
codcond <- read_csv("data/cook_cond_data.csv")
head(codcond)

#codcond <- read_csv("C:/Users/alask/Documents/EFH_cook_season_diet_lipid/Rstudio_codseason/data/cook_cond_data.csv")
#head(codcond)
##if need to rename columns, you would type 
#codcond <- read_csv("data/cook_cond_data.csv") %>%
 # rename("wet_wgt_g" = "wgt_total",
  #       "Total_length" = "TL") 
head(codcond)
tail(codcond)
##can see that last 3 rows are all NA. Must remove these last 3 rows
codcond1<-filter(codcond, !is.na(TL))
head(codcond1)
tail(codcond1) #you can see it worked because now 561 obs instead of 564

#need to remove years 2017 and 2021
codcond1<-filter(codcond,year=="2018"|year=="2019" | year == "2020")
codcond1
distinct(codcond1, year)

#need to remove jig gear
codcond1<-filter(codcond1,gear=="seine")
codcond1
distinct(codcond1, gear)

distinct(codcond1, bay) #to make sure only cook bay

distinct(codcond1,month)
# When we boxplot this, we notice that the factors in month are alphabetical. 
#we want them to be sequential, so need to create a new column "Month" w ordered factor
codcond1 <- mutate(codcond1, Month = fct_relevel(month, c("May", "June", "July", "Aug",
  "Sept", "Oct", "Nov", "Dec")))
# We use fct_relevel to explicitly define the factor order for column "Month" 

#need to keep only age 0
distinct(codcond1,age)
codcond1<-filter(codcond1,age==0)
codcond1
distinct(codcond1, age)

str(codcond1) # we can see now that the factors are ordered correctly 
head(codcond1)
tail(codcond1)

## ## now add LENGTH data ####
codlen <- read_csv("data/cook_codlen1820.csv")

head(codlen)
distinct(codlen, bay)
distinct(codlen,year)
tail(codlen)
 
##if need to rename columns, you would type 
rename(codlen,"TL" = "Length (mm)") 
head(codlen)

# When we boxplot this, we notice that the factors in month are alphabetical. 
#we want them to be sequential, so need to create a new column "Month" w ordered factor
codlen <- mutate(codlen, Month = fct_relevel(month, c("May", "June", "July", "Aug",
                                                          "Sept", "Oct", "Nov", "Dec")))
# We use fct_relevel to explicitly define the factor order for column "Month" 

## ## now add CATCH data ####

codcpue <- read_csv("data/cook_codcpue1820.csv")
#  rename("Cod" = "Pacific cod") 
head(codcpue)
distinct(codcpue, Bay) #should show only Cook Bay
distinct(codcpue,year) #should show 2018, 2019, 2020
tail(codcpue) #to make sure no NA at rows at end

##there are many rows with NA for entire row. Must remove these last rows
#codcpue <- filter(codcpue, !is.na(Date))
#now check
#tail(codcpue)
distinct(codcpue, month)
# When we boxplot this, we notice that the factors in month are alphabetical. 
#we want them to be sequential, so need to create a new column "Month" w ordered factor
codcpue <- mutate(codcpue, Month = fct_relevel(month, c("Feb", "March", "April", "May",
"June", "July", "Aug","Sept", "Oct", "Nov", "Dec")))
# We use fct_relevel to explicitly define the factor order for column "Month" 
distinct (codcpue, Month)

