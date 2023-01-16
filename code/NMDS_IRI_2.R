#### NMS Ordinations by IRI of prey item in stomach
##  IRI values were then cube root
###all empty somachs removed. currently this is NOT each stomach adds to 100%.
### based on the code sent from Hillary, March 2022. most updated version

#### Import Files ####
prey_iri <- read_csv("data/IRI_new_data.csv")
head(prey_iri)
tail(prey_iri) #to make sure no empty rows at bottom of file. there are 2 rows
prey_IRI<-filter(prey_iri, !is.na(Month))
tail(prey_IRI) # now those 2 empty rows gone


#### Load Libraries ####
library('vegan')
library("MASS")
library("labdsv")
library("indicspecies")
library("tidyverse")
library("ggrepel")
library("goeveg")
library("corrplot")
library("rcartocolor")
library("dplyr")
library("tidyr")
library("ggeasy")


#### Create Environmental and Species Matrices ####
prey_iris <- prey_iri[,c(22:32)]
head(prey_iris)

preyEnvData <- prey_iri[,c(4:10)]
head(preyEnvData)


#Check Scree Plot (determine dimensions of NMS)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(prey_iris,distance = "bray",k = 6,trymax = 300,autotransform = FALSE)
#no convergence when trymax = 200 or when trymax = 300
#going to remove calanoid copepod and gastropod and then rerun

#still no convergence. try to run without 4th root but with raw IRI data
prey_iris <- prey_iri[,c(11,12,13,15, 16, 18:21)]
head(prey_iris)

### Run a Scree Plot using the package "goeveg"
dimcheckMDS(prey_iris,distance = "bray",k = 6,trymax = 200,autotransform = FALSE)

# no convergence. 
