#### read me ####

# The purpose of this project is to clean up the RAW data by identifying
# where data is missing, filling in the missing data with an average of 
# similar conditions, and get data ready for processing.

#### libraries ####
library(tidyverse)
library(lubridate)
library(dplyr)
library(psych)
library(car)
library(tsibble)
library(ape)
library(ade4)
library(rgdal)
library(reshape2)
library(simmr)

#downloading uninstalled packages
install.packages("psych")
install.packages("tsibble")
install.packages("ape")
install.packages("ade4")
install.packages("rgdal")

#### retrieve data ####
C1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C1_SoilTempData.csv", header = TRUE)
C2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C2_SoilTempData.csv", header = TRUE)
T1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T1_SoilTempData.csv", header = TRUE)
T2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T2_SoilTempData.csv", header = TRUE)

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
C1$date_time=as.POSIXct(C1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
C2$date_time=as.POSIXct(C2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T1$date_time=as.POSIXct(T1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T2$date_time=as.POSIXct(T2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")


#### explore data set size and structure ####

#analyze C1
head(C1)
str(C1)

#analyze C2
head(C2)
str(C2)

#analyze T1
head(T1)
str(T1)
##X6.inches is inaccurately classed as a character instead of a number, we need to reclassify it

#convert X6.inches to column of numbers
T1$X6.inches = as.numeric(T1$X6.inches)

#check
class(T1$X6.inches)
##SUCCESS

#analyze T2
head(T2)
str(T2)

#plots of temperature values
plot(x = C1$date_time, y = C1$X6.inches)
ggplot2::qplot(x=date_time, y=X6.inches, data=C1, geom="point")

#histogram of C1
ggplot2::qplot(x=X6.inches, data=C1, geom="histogram")+
  facet_grid(.~1)

#### calculating correct number of hours ####

#get first and last days
summary(C1)

#calculating total number of days in study period
startDate <-as.Date("2014-09-01 00:00:00", tz = "MST")
endDate <-as.Date("2021-12-01 12:00:00", tz = "MST")
Noofdays <- endDate - startDate
view(Noofdays)
#total days is 2648

#calculating total hours
difftime(startDate, endDate, units = "hours")
# Number of hours is 63552, so we know this is the number of observations we can expect
##The above is incorrect as evidenced when creating data frame from sequence of
##datetime in next section

#### create sequence with correct number of rows ####

#create sequence with correct dates and times
seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63565)

#create data frame from sequence (this will not be used as it makes the data too messy)
hour.df <- data.frame(allhours=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63565))

#use the join function 
C1_join <- data.frame(left_join(hour.df, C1, by = c("allhours" = "date_time")))
C2_join <- data.frame(left_join(hour.df, C2, by = c("allhours" = "date_time")))
T1_join <- data.frame(left_join(hour.df, T1, by = c("allhours" = "date_time")))
T2_join <- data.frame(left_join(hour.df, T2, by = c("allhours" = "date_time")))


#### import ambient air temperature data ####

# Importing temperature data for study period from NOAA station Santa Fe 2 to
# have an ambient air temperature that can be used for comparison to soil
# temperature data. NOAA station is approximately 3.37 miles away from study 
# site.

#retrieve csv with hourly temperature data
AirTemp = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/NOAA_AirportGauge_AirTemp.csv", header = TRUE)

#### reformat date/time for ambient temperature data ####
# view formats
?strptime

#create new date/time format
AirTemp$date_hour=as.POSIXct(AirTemp$DATE, format="%m/%d/%y %H:%M", tz="MST")

#### prepare AirTemp data frame for processing ####
# check class
class(AirTemp$HourlyDryBulbTemperature)
#currently classified as a character, we must convert it

# convert to numeric value
AirTemp$HourlyDryBulbTemperature = as.numeric(AirTemp$HourlyDryBulbTemperature)
##SUCCESS 

# remove unwanted rows
AirTemp_corrected <- head(AirTemp, -13)

# use join function to combine with correct number of hours
AirTemp_joined <- data.frame(right_join(hour.df, AirTemp_corrected, 
                                           by = c("allhours" = "date_hour")))

# sort chronologically
AirTemp_ready <- AirTemp_joined[order(AirTemp_joined$allhours),]

#### calculate mean of identical dates in data sets ####
#identify duplicates in data frames
duplicated(C1_join)
subset(C1_join, duplicated(allhours))

duplicated(C2_join)
subset(C2_join, duplicated(allhours))

duplicated(T1_join)
subset(T1_join, duplicated(allhours))

duplicated(T2_join)
subset(T2_join, duplicated(allhours))

#example code
#data <- data.frame("id" = c(1,1, 3,3,5,5),
#                     "city" = c("new_york", "new_york", "london", "london", "barcelona", "barcelona"),
#                     "temperature" = c(15, 16, 13, 12, 30, 32),
#                     "pressure" =  c(1000, 1003, 980, 998, 1013, 1015),
#                     "time" = c("2015-01-01 06:30:00","2015-01-01 18:30:00",
#                                "2015-02-10 07:00:00", "2015-02-10 20:30:00",
#                                "2015-04-08 08:00:00", "2015-04-08 12:00:00"),stringsAsFactors = FALSE)

#data %>% group_by(id, city, time = as.Date(time)) %>% summarize(across(c(temperature, pressure), mean))

#calculate averages for temperature values of duplicate readings
Control_1 <- C1_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Control_2 <- C2_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Test_1 <- T1_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Test_2 <- T2_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

#now check
subset(Control_1, duplicated(allhours))

subset(Control_2, duplicated(allhours))

subset(Test_1, duplicated(allhours))

subset(Test_2, duplicated(allhours))

