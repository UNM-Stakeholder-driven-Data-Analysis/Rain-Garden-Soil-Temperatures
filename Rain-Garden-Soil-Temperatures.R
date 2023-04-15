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
library(ggplot2)
library(reshape2)

#downloading uninstalled packages
install.packages("psych")
install.packages("tsibble")
install.packages("ape")
install.packages("ade4")
install.packages("rgdal")

#### retrieve data ####
# Importing data from stakeholder
C1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C1_SoilTempData.csv", header = TRUE)
C2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C2_SoilTempData.csv", header = TRUE)
T1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T1_SoilTempData.csv", header = TRUE)
T2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T2_SoilTempData.csv", header = TRUE)

# Importing temperature data for study period from NOAA station Santa Fe 2 to
# have an ambient air temperature that can be used for comparison to soil
# temperature data. NOAA station is approximately 3.37 miles away from study 
# site.
#retrieve csv with hourly temperature data
AirTemp = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/NOAA_AirportGauge_AirTemp.csv", header = TRUE)

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

# date/time column is currently in "%m/%d/%Y %H:%M %p" format


#### reformat date/time ####

# view formats
?strptime

#view time zones
OlsonNames()

#create new date/time format
C1$date_time=as.POSIXct(C1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
C2$date_time=as.POSIXct(C2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T1$date_time=as.POSIXct(T1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T2$date_time=as.POSIXct(T2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
AirTemp$date_hour=as.POSIXct(AirTemp$DATE, format="%m/%d/%y %H:%M", tz="MST")


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

# check class
class(AirTemp$HourlyDryBulbTemperature)
#currently classified as a character, we must convert it

# convert to numeric value
AirTemp$HourlyDryBulbTemperature = as.numeric(AirTemp$HourlyDryBulbTemperature)
##SUCCESS 

# remove unwanted rows
AirTemp_corrected <- head(AirTemp, -13)

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


#### prepare AirTemp data frame for processing ####
# use join function to combine with correct number of hours
AirTemp_joined <- data.frame(right_join(hour.df, AirTemp_corrected, 
                                           by = c("allhours" = "date_hour")))

# sort chronologically
AirTemp_sorted <- AirTemp_joined[order(AirTemp_joined$allhours),]

## create table for Technical Report ##
AirTemp_sorted[1:10,]
AirTemp_Table <- as.data.frame(AirTemp_sorted[1:10,])

# example code for exporting
# write.csv(DataFrame Name, "Path to export the DataFrame\\File Name.csv", 
# row.names=FALSE)
write.csv(AirTemp_Table, "~/Documents/Data Analysis/R\\AirTemp_Table.csv", 
          row.names = FALSE)                    
                   

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

duplicated(AirTemp_ready)
subset(AirTemp_ready, duplicated(allhours))

#example code
#data <- data.frame("id" = c(1,1, 3,3,5,5),
#                     "city" = c("new_york", "new_york", "london", "london", "barcelona", "barcelona"),
#                     "temperature" = c(15, 16, 13, 12, 30, 32),
#                     "pressure" =  c(1000, 1003, 980, 998, 1013, 1015),
#                     "time" = c("2015-01-01 06:30:00","2015-01-01 18:30:00",
#                                "2015-02-10 07:00:00", "2015-02-10 20:30:00",
#                                "2015-04-08 08:00:00", "2015-04-08 12:00:00"),stringsAsFactors = FALSE)

# data %>% group_by(id, city, time = as.Date(time)) %>% summarize(across(c(temperature, pressure), mean))

#calculate averages for temperature values of duplicate readings
Control_1 <- C1_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Control_2 <- C2_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Test_1 <- T1_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

Test_2 <- T2_join %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

AmbientAir <- AirTemp_sorted %>% group_by(allhours) %>%
  summarize(across(c(HourlyDryBulbTemperature), mean))

#now check
subset(Control_1, duplicated(allhours))

subset(Control_2, duplicated(allhours))

subset(Test_1, duplicated(allhours))

subset(Test_2, duplicated(allhours))

subset(AmbientAir, duplicated(allhours))

#### identify missing values in Air Temp data ####

AmbientAir[!complete.cases(AmbientAir),]

AmbientAir_ready <- na.omit(AmbientAir)

#### create average daily values ####
# calculate daily mean
# example code
# data %>% group_by(id, city, time = as.Date(time)) %>% 
# summarise(across(c(temperature, pressure), mean))

Control_1_Daily <- data.frame(Control_1 %>% group_by(alldays = as.Date(allhours))
                  %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                  X12.inches, X6.inches), mean)))

Control_2_Daily <- data.frame(Control_2 %>% group_by(alldays = as.Date(allhours))
                    %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                    X12.inches, X6.inches), mean)))

Test_1_Daily <- data.frame(Test_1 %>% group_by(alldays = as.Date(allhours))
                %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                X12.inches, X6.inches), mean)))

Test_2_Daily <- data.frame(Test_2 %>% group_by(alldays = as.Date(allhours))
                %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                X12.inches, X6.inches), mean)))

AmbientAir_Daily <- data.frame(AmbientAir_ready %>% group_by(alldays = as.Date(allhours))
                    %>% summarise(across(c(HourlyDryBulbTemperature), mean)))

# remove excessive decimal places

# example code
# mydf %>% mutate_at(vars(-vch1), funs(round(., 1)))

Control_1_Daily_rounded <- Control_1_Daily %>% mutate_at(vars(-alldays), funs(round(., 1)))

Control_2_Daily_rounded <- Control_2_Daily %>% mutate_at(vars(-alldays), funs(round(., 1)))

Test_1_Daily_rounded <- Test_1_Daily %>% mutate_at(vars(-alldays), funs(round(., 1)))

Test_2_Daily_rounded <- Test_2_Daily %>% mutate_at(vars(-alldays), funs(round(., 1)))

AmbientAir_Daily_rounded <- AmbientAir_Daily %>% mutate_at(vars(-alldays), funs(round(., 1)))

class(AmbientAir_Daily_rounded$alldays)
class(AmbientAir_Daily_rounded$HourlyDryBulbTemperature)
class(Test_1_Daily_rounded$alldays)
class(Test_1_Daily_rounded$X30.inches)

#### read me ####

# Now that our data has been cleaned up, we can create a time series 
# and begin analyzing.

###

#### more libraries ####

# install necessary libraries 
install.packages("xts")
install.packages("imputeTS")
install.packages("tseries")
install.packages("astsa")
install.packages("WaveletComp")

# open them 
library(xts)
library(imputeTS)
library(tseries)
library(astsa)
library(WaveletComp)


#### Apparently there are NA's, we MUST FILL ####

## fill with linear interpolation

# Make univariate zoo time series #
# example code
# ts.temp<-read.zoo(C2_no3, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

Temporary_TS_C1 <- read.zoo(Control_1_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST")
view(Temporary_TS_C1 <- read.zoo(Control_1_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST"))

Temporary_TS_C2 <- read.zoo(Control_2_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST")
Temporary_TS_T1 <- read.zoo(Test_1_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST")
Temporary_TS_T2 <- read.zoo(Test_2_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST")

Temporary_TS_Air <- read.zoo(AmbientAir_Daily_rounded, index.column = 1, format="%Y-%m-%d", tz="MST")
view(Temporary_TS_Air <- read.zoo(AmbientAir_Daily_rounded, index.column=1, format="%Y-%m-%d", tz="MST"))
## Column name for temperature has changed to simply "x", will adjust later ##

## DO NOT USE LINEAR INTERPOLATION ##
# Apply linear interpolation method #
# example code
# C2_no3_filled_linearinterp = na.approx(ts.temp, na.rm = T, maxgap = 24*4)

Control_1_Daily_linearfilled <- na.approx(Temporary_TS_C1, na.rm = T, maxgap = 24)
Control_2_Daily_linearfilled <- na.approx(Temporary_TS_C2, na.rm = T, maxgap = 24)
Test_1_Daily_linearfilled <- na.approx(Temporary_TS_T1, na.rm = T, maxgap = 24)
Test_2_Daily_linearfilled <- na.approx(Temporary_TS_T2, na.rm = T, maxgap = 24)
Air_Daily_linearfilled <- na.approx(Temporary_TS_Air, na.rm = T, maxgap = 24)

# revert back to data frame #
# example code
# C2_no3_filled_linearinterp = as.data.frame(C2_no3_filled_linearinterp)

Control_1_Daily_linearfilled <- as.data.frame(Control_1_Daily_linearfilled)
Control_2_Daily_linearfilled <- as.data.frame(Control_2_Daily_linearfilled)
Test_1_Daily_linearfilled <- as.data.frame(Test_1_Daily_linearfilled)
Test_2_Daily_linearfilled <- as.data.frame(Test_2_Daily_linearfilled)
Air_Daily_linearfilled <- as.data.frame(Air_Daily_linearfilled)

## Linear interpolation leaves Test_1 site without a value for the last day/final value 
## so I will use spline interpolation instead.

# fill with spline interpolation #

# Apply spline interpolation method 

# example code
# C2_no3_filled_splineinterp = na.spline(ts.temp, na.rm = T, maxgap = 24*4)

Control_1_Daily_splinefilled <- na.spline(Temporary_TS_C1, na.rm = T, maxgap = 24)
Control_2_Daily_splinefilled <- na.spline(Temporary_TS_C2, na.rm = T, maxgap = 24)
Test_1_Daily_splinefilled <- na.spline(Temporary_TS_T1, na.rm = T, maxgap = 24)
Test_2_Daily_splinefilled <- na.spline(Temporary_TS_T2, na.rm = T, maxgap = 24)
Air_Daily_splinefilled <- na.spline(Temporary_TS_Air, na.rm = T, maxgap = 24)

# revert back to data frame #

#example code
# C2_no3_filled_splineinterp = as.data.frame(C2_no3_filled_splineinterp)

Control_1_Daily_splinefilled <- as.data.frame(Control_1_Daily_splinefilled)
Control_2_Daily_splinefilled <- as.data.frame(Control_2_Daily_splinefilled)
Test_1_Daily_splinefilled <- as.data.frame(Test_1_Daily_splinefilled)
Test_2_Daily_splinefilled <- as.data.frame(Test_2_Daily_splinefilled)
Air_Daily_splinefilled <- as.data.frame(Air_Daily_splinefilled)

# once again, we must reduce decimal points
round(Control_1_Daily_splinefilled, digits = 1)
round(Control_2_Daily_splinefilled, digits = 1)
round(Test_1_Daily_splinefilled, digits = 1)
round(Test_2_Daily_splinefilled, digits = 1)
round(Air_Daily_splinefilled, digits = 1)

# make into data frames
Control_1_splinefilled <- as.data.frame(round(Control_1_Daily_splinefilled, digits = 1))
Control_2_splinefilled <- as.data.frame(round(Control_2_Daily_splinefilled, digits = 1))
Test_1_splinefilled <- as.data.frame(round(Test_1_Daily_splinefilled, digits = 1))
Test_2_splinefilled <- as.data.frame(round(Test_2_Daily_splinefilled, digits = 1))
Air_splinefilled <- as.data.frame(round(Air_Daily_splinefilled, digits = 1))

#### re-add date sequence as a column ####

# create with a sequence that features all days 
seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "day", length.out = 2649)

#create data frame from sequence
alldays <- data.frame(alldays=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "day", length.out = 2649))

# merge alldays sequence with all data frames
Control_1_splinefilled_merged <-merge(data.frame(Control_1_splinefilled, 
                                  row.names = NULL), data.frame(alldays, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

Control_2_splinefilled_merged <-merge(data.frame(Control_2_splinefilled, 
                                  row.names = NULL), data.frame(alldays, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

Test_1_splinefilled_merged <-merge(data.frame(Test_1_splinefilled, 
                               row.names = NULL), data.frame(alldays, row.names=NULL),
                               by = 0, all = TRUE)[-1]

Test_2_splinefilled_merged <-merge(data.frame(Test_2_splinefilled, 
                               row.names = NULL), data.frame(alldays, row.names=NULL),
                               by = 0, all = TRUE)[-1]

Air_splinefilled_merged <-merge(data.frame(Air_splinefilled, 
                          row.names = NULL), data.frame(alldays, row.names=NULL),
                          by = 0, all = TRUE)[-1]

# sort chronologically 
Control_One_splinefilled_sorted <- Control_1_splinefilled_merged[order(Control_1_splinefilled_merged$alldays),]
Control_Two_splinefilled_sorted <- Control_2_splinefilled_merged[order(Control_2_splinefilled_merged$alldays),]
Test_One_splinefilled_sorted <- Test_1_splinefilled_merged[order(Test_1_splinefilled_merged$alldays),]
Test_Two_splinefilled_sorted <- Test_2_splinefilled_merged[order(Test_2_splinefilled_merged$alldays),]
Air_splinefilled_sorted <- Air_splinefilled_merged[order(Air_splinefilled_merged$alldays),]

# rename column in ambient air temperature data frame 
colnames(Air_splinefilled_sorted) <- c('AverageDailyTemperature', 'alldays')

#### Create a time series ####

## Create time series objects, check class, and plot ## 

# example code
# C2_no3_xts = xts(C2_no3_filled_splineinterp$nitrate_uM_c_bc, order.by = 
# C2_no3_filled_splineinterp$date_timeAK)
# class(C2_no3_xts)
# plot(C2_no3_xts)
# summary(C2_no3_xts)

# Control One
Control_One_30in_xts <- xts(Control_One_splinefilled_sorted$X30.inches, order.by = 
                         Control_One_splinefilled_sorted$alldays)
# check class
class(Control_One_30in_xts)

# change column name
colnames(Control_One_30in_xts) <- c('X30.inches')

# create plot
plot(Control_One_30in_xts)

## repeat for all sensor depths ##

# 24 inches
Control_One_24in_xts <- xts(Control_One_splinefilled_sorted$X24.inches, order.by = 
                         Control_One_splinefilled_sorted$alldays)
colnames(Control_One_24in_xts) <- c('X24.inches')
plot(Control_One_24in_xts)

# 18 inches
Control_One_18in_xts <- xts(Control_One_splinefilled_sorted$X18.inches, order.by = 
                            Control_One_splinefilled_sorted$alldays)
colnames(Control_One_18in_xts) <- c('X18.inches')
plot(Control_One_18in_xts)

# 12 inches
Control_One_12in_xts <- xts(Control_One_splinefilled_sorted$X12.inches, order.by = 
                              Control_One_splinefilled_sorted$alldays)
colnames(Control_One_12in_xts) <- c('X12.inches')
plot(Control_One_12in_xts)

# 6 inches
Control_One_6in_xts <- xts(Control_One_splinefilled_sorted$X6.inches, order.by = 
                              Control_One_splinefilled_sorted$alldays)
colnames(Control_One_6in_xts) <- c('X6.inches')
plot(Control_One_6in_xts)


## Control Two ##

# 30 inches
Control_Two_30in_xts <- xts(Control_Two_splinefilled_sorted$X30.inches, order.by = 
                         Control_Two_splinefilled_sorted$alldays)
class(Control_Two_30in_xts)
plot(Control_Two_30in_xts)
colnames(Control_Two_30in_xts) <- c('X30.inches')

# 24 inches
Control_Two_24in_xts <- xts(Control_Two_splinefilled_sorted$X24.inches, order.by = 
                              Control_Two_splinefilled_sorted$alldays)
class(Control_Two_24in_xts)
plot(Control_Two_24in_xts)
colnames(Control_Two_24in_xts) <- c('X24.inches')

# 18 inches
Control_Two_18in_xts <- xts(Control_Two_splinefilled_sorted$X18.inches, order.by = 
                              Control_Two_splinefilled_sorted$alldays)
class(Control_Two_18in_xts)
plot(Control_Two_18in_xts)
colnames(Control_Two_18in_xts) <- c('X18.inches')

# 12 inches
Control_Two_12in_xts <- xts(Control_Two_splinefilled_sorted$X12.inches, order.by = 
                              Control_Two_splinefilled_sorted$alldays)
class(Control_Two_12in_xts)
plot(Control_Two_12in_xts)
colnames(Control_Two_12in_xts) <- c('X12.inches')

# 6 inches
Control_Two_6in_xts <- xts(Control_Two_splinefilled_sorted$X6.inches, order.by = 
                              Control_Two_splinefilled_sorted$alldays)
class(Control_Two_6in_xts)
plot(Control_Two_6in_xts)
colnames(Control_Two_6in_xts) <- c('X6.inches')

## Test One ##

# 30 inches
Test_One_30in_xts <- xts(Test_One_splinefilled_sorted$X30.inches, order.by = 
                              Test_One_splinefilled_sorted$alldays)
class(Test_One_30in_xts)
plot(Test_One_30in_xts)
colnames(Test_One_30in_xts) <- c('X30.inches')

# 24 inches
Test_One_24in_xts <- xts(Test_One_splinefilled_sorted$X24.inches, order.by = 
                           Test_One_splinefilled_sorted$alldays)
class(Test_One_24in_xts)
plot(Test_One_24in_xts)
colnames(Test_One_24in_xts) <- c('X24.inches')

# 18 inches
Test_One_18in_xts <- xts(Test_One_splinefilled_sorted$X18.inches, order.by = 
                           Test_One_splinefilled_sorted$alldays)
class(Test_One_18in_xts)
plot(Test_One_18in_xts)
colnames(Test_One_18in_xts) <- c('X18.inches')

# 12 inches
Test_One_12in_xts <- xts(Test_One_splinefilled_sorted$X12.inches, order.by = 
                           Test_One_splinefilled_sorted$alldays)
class(Test_One_12in_xts)
plot(Test_One_12in_xts)
colnames(Test_One_12in_xts) <- c('X12.inches')

# 6 inches
Test_One_6in_xts <- xts(Test_One_splinefilled_sorted$X6.inches, order.by = 
                           Test_One_splinefilled_sorted$alldays)
class(Test_One_6in_xts)
plot(Test_One_6in_xts)
colnames(Test_One_6in_xts) <- c('X6.inches')

## Test Two ##

# 30 inches
Test_Two_30in_xts <- xts(Test_Two_splinefilled_sorted$X30.inches, order.by = 
                           Test_Two_splinefilled_sorted$alldays)
class(Test_Two_30in_xts)
plot(Test_Two_30in_xts)
colnames(Test_Two_30in_xts) <- c('X30.inches')

# 24 inches
Test_Two_24in_xts <- xts(Test_Two_splinefilled_sorted$X24.inches, order.by = 
                           Test_Two_splinefilled_sorted$alldays)
class(Test_Two_24in_xts)
plot(Test_Two_24in_xts)
colnames(Test_Two_24in_xts) <- c('X24.inches')

# 18 inches
Test_Two_18in_xts <- xts(Test_Two_splinefilled_sorted$X18.inches, order.by = 
                           Test_Two_splinefilled_sorted$alldays)
class(Test_Two_18in_xts)
plot(Test_Two_18in_xts)
colnames(Test_Two_18in_xts) <- c('X18.inches')

# 12 inches
Test_Two_12in_xts <- xts(Test_Two_splinefilled_sorted$X12.inches, order.by = 
                           Test_Two_splinefilled_sorted$alldays)
class(Test_Two_12in_xts)
plot(Test_Two_12in_xts)
colnames(Test_Two_12in_xts) <- c('X12.inches')

# 6 inches
Test_Two_6in_xts <- xts(Test_Two_splinefilled_sorted$X6.inches, order.by = 
                          Test_Two_splinefilled_sorted$alldays)
class(Test_Two_6in_xts)
plot(Test_Two_6in_xts)
colnames(Test_Two_6in_xts) <- c('X6.inches')

## Daily Average Temperature from USGS ##
Ambient_Temperature_xts <- xts(Air_splinefilled_sorted$AverageDailyTemperature, order.by = 
                          Air_splinefilled_sorted$alldays)
class(Ambient_Temperature_xts)
plot(Ambient_Temperature_xts)
colnames(Ambient_Temperature_xts) <- c('AverageDailyTeperature')










