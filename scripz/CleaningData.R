#### read me ####

# The purpose of this project is to clean up the RAW data by identifying
# where data is missing, filling in the missing data with an average of 
# similar conditions, and get data ready for processing.

#### libraries ####
library(tidyverse)
library(lubridate)
library(dplyr)

#### set working directory ####

setwd("~/Documents/Data Analysis/R/Southwest Urban Hydrology/inside data")

#### retrieve data ####

C1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/inside data/C1_SoilTempData.csv", header = TRUE)
C2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/inside data/C2_SoilTempData.csv", header = TRUE)
T1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/inside data/T1_SoilTempData.csv", header = TRUE)
T2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/inside data/T2_SoilTempData.csv", header = TRUE)

#### examine date/time ####

#examine date/time for C1
class(C1$Measurement.Time)
head(C1$Measurement.Time)
tail(C1$Measurement.Time)
View(C1)

#examine date/time for C2
class(C2$Measurement.Time)
head(C2$Measurement.Time)
tail(C2$Measurement.Time)
View(C2)

#examine date/time for T1
class(T1$Measurement.Time)
head(T1$Measurement.Time)
tail(T1$Measurement.Time)
View(T1)

#examine date/time for T2
class(T2$Measurement.Time)
head(T2$Measurement.Time)
tail(T2$Measurement.Time)
View(T2)

#date/time column is currently in "%m/%d/%Y %H:%M %p" format

#### reformat date/time ####

# view formats
?strptime

#view timezones
OlsonNames()

#create new date/time format
C1$date_time=as.POSIXct(C1$Measurement.Time, format="%m/%d/%Y %H:%M %p", tz="MST")

C2$date_time=as.POSIXct(C2$Measurement.Time, format="%m/%d/%Y %H:%M %p", tz="MST")

T1$date_time=as.POSIXct(T1$Measurement.Time, format="%m/%d/%Y %H:%M %p", tz="MST")

T2$date_time=as.POSIXct(T2$Measurement.Time, format="%m/%d/%Y %H:%M %p", tz="MST")

#### describe data set size and structure ####

#analyze and format C1
head(C1)
str(C1)

C1$year = lubridate::year(C1$date_time)
C1$month = lubridate::month(C1$date_time)

# now plot for distribution analysis of C1

ggplot(data = C1, aes(x=month, y=X6.inches))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank())+
  theme_classic()

#analyze and format C2

head(C2)
str(C2)

C2$year = lubridate::year(C2$date_time)
C2$month = lubridate::month(C2$date_time)

# now plot for distribution analysis for C2

ggplot(data = C2, aes(x=month, y=X6.inches))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = C2)+
  theme_classic()


#### identify damaged data ####

#get first and last days
summary(C1)

#calculating total days
startDate <-as.Date("2014-09-01")
endDate <-as.Date("2021-12-01")
Noofdays <-endDate - startDate
Noofdays

#calculating total hours
as.duration(Noofdays)

seq(as.Date("2014-09-01"), as.Date("2021-12-01"))

#create data frame with all possible dates and times



#identifying duplicates
anyDuplicated(C1$date_time)

