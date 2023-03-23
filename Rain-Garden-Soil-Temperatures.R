#### read me ####

# The purpose of this project is to clean up the RAW data by identifying
# where data is missing, filling in the missing data with an average of 
# similar conditions, and get data ready for processing.

#### libraries ####
library(tidyverse)
library(lubridate)
library(dplyr)

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

#analyze and format C1
head(C1)
str(C1)

#plots of temperature values
plot(C1$date_time, C1$X6.inches)
ggplot2::qplot(x=date_time, y=X6.inches, data=C1, geom="point")

#group all files by months
C1$month = lubridate::month(C1$date_time)
C2$month = lubridate::month(C2$date_time)
T1$month = lubridate::month(T1$date_time)
T2$month = lubridate::month(T2$date_time)

#rerun plot of C1
ggplot2::qplot(x=month, y=X6.inches, data=C1, geom="point")

ggplot2::qplot(x=X6.inches, data=C1, geom="histogram")+
  facet_grid(.~1)

#group C1 by days
C1$day = lubridate::day(C1$date_time)

#create average monthly values
C1 %>% dplyr::group_by(month) %>% summarise(mean(X6.inches))
C2 %>% dplyr::group_by(month) %>% summarise(mean(X6.inches))
T1 %>% dplyr::group_by(month) %>% summarise(mean(X6.inches))
T2 %>% dplyr::group_by(month) %>% summarise(mean(X6.inches))

#plotting tibbles 
DFlng$month <- factor(DFlng$month)
ggplot(C1, aes(x = month, y = mean(X6.inches), group = NULL, color = NULL)) + geom_line()

#### calculating correct number of hours ####

#get first and last days
summary(C1)

#calculating total number of days
startDate <-as.Date("2014-09-01 00:00:00", tz = "MST")
endDate <-as.Date("2021-12-01 12:00:00", tz = "MST")
Noofdays <-endDate - startDate
# Noofdays is 2648

#calculating total hours
difftime(startDate, endDate, units = "hours")
# Number of hours is 63552, so we know this is the number of observations we can expect

#### create data frame with correct hours and merge with existing data frames ####

#create sequence with correct dates and times
seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63552)

#create data frame from sequence
hour.df <- data.frame(hours=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63552))

#merge data frame from sequence with all data frames
C1 <- merge(data.frame(C1, row.names = NULL), data.frame(hour.df, row.names=NULL),
                    by = 0, all = TRUE)[-1]
C2 <- merge(data.frame(C2, row.names = NULL), data.frame(hour.df, row.names=NULL),
            by = 0, all = TRUE)[-1]
T1 <- merge(data.frame(T1, row.names = NULL), data.frame(hour.df, row.names=NULL),
            by = 0, all = TRUE)[-1]
T2 <- merge(data.frame(T2, row.names = NULL), data.frame(hour.df, row.names=NULL),
            by = 0, all = TRUE)[-1]

#order/sort chronologically
C1_correct <- C1[order(C1$hours),]
C2_correct <- C2[order(C2$hours),]
T1_correct <- T1[order(T1$hours),]
T2_correct <- T2[order(T2$hours),]


