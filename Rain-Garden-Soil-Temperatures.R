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

#create data frame from sequence
hour.df <- data.frame(hours=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63565))

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

#### calculate mean of identical dates in data sets ####
#identify duplicates in data frames
duplicated(C1_correct)
subset(C1_correct, duplicated(date_time))

duplicated(C2_correct)
subset(C2_correct, duplicated(date_time))

duplicated(T1_correct)
subset(T1_correct, duplicated(date_time))

duplicated(T2_correct)
subset(T2_correct, duplicated(date_time))

#calculate averages for temperature values of duplicate readings
C1_correct %>% group_by(date_time)  %>% 
  summarize(across(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean)
##didn't work

#idk what this code is for...
length(unique(C1_correct$date_time)) == nrow(C1_correct)

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
Control_1 <- C1_correct %>% group_by(date_time)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Control_2 <- C2_correct %>% group_by(date_time)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Test_1 <- T1_correct %>% group_by(date_time)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))
##not working due to NA's, see next section

Test_2 <- T2_correct %>% group_by(date_time)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))


#now check
subset(Control_1, duplicated(date_time))

subset(Control_2, duplicated(date_time))

subset(Test_1, duplicated(date_time))

subset(Test_2, duplicated(date_time))

#### identify missing rows/hours in data sets ####
#copy to  ew dataframe
NewT1 <- T1_correct

#match indices
inds1 <- match(T1_correct$hours, T1_correct$date_time)
inds2 <- match(T1_correct$date_time, T1_correct$hours)

#replace values 
NewT1$date_time <- T1_correct$date_time[inds1]

#order by hours column
NewT1 <- NewT1[order(NewT1$date_time), ]

#replace NA in hours with unmatched value
NewT1$date_time[is.na(NewT1$date_time)] <- T1_correct$date_time[is.na(inds2)]

#other code
NewC1 <- match(C1_correct$date_time, C1_correct$hours)
NewControlOne <- is.na(NewC1)
NewControlOne <- as.data.frame(NewControlOne)
data.frame(newdatetime=c(NewC1$date_time[!NewControlOne], NewC1$date_time[NewControlOne]),
           newhours=c(NewC1$hours[NewControlOne[!NewControlOne]], NewC1$hours
                      [!seq_along(NewC1$hours) %in% NewControlOne]))
