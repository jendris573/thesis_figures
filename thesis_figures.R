## Figures for thesis presentation
## written by Joe Endris

#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(ggtext)
library(gridExtra)
library(lubridate)
library(readxl)

########################
## data preparation ----
########################

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))

#read in NOAA Climate Data data
TN<-read.csv("data/Tennessee_climate.csv")

#keep only sewage plant
TN <- TN%>%filter(STATION=="USC00401790")

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,10]),]

#create column for year
TN <- mutate(TN, year=year(TN$DATE))

#create column for month
TN <- mutate(TN, month=month(TN$DATE))

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

#read in phenology observations
phenology<-read_excel("data/phenology_check.xlsx")

#create column for year
phenology <- mutate(phenology, year=year(date))

#create column for julian date
phenology$julian_date <- yday(phenology$date)

#filter out 2021 data since there is no corresponding LT50 data for 2021
phenology <- filter(phenology, year > "2021")

#omit any blank spots in the mean_phenology column
phenology <- phenology[complete.cases(phenology[,4]),]

#read in data sets
outputs <- read_excel("data/crit_values_final.xlsx")
leaf_max_temp <- read_excel("data/leaf_temperatures.xlsx", sheet =2)

#filter just TN data
outputs<-outputs[which(outputs$state=="TN"),]

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

#add column for image location - needed for plotting leaf image
leaf_max_temp$location<-'leaf_image2.jpg'#change this filename to whatever .jpg or .png - can only use jpg or png
#If using a different .jpg, make sure the file size is really small (<10 KB). Otherwise the plotting
#takes forever and you end up with a huge file size for the ggplot. Not sure why it does this.
