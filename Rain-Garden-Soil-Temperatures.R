#### read me ####

## The purpose of this project is to clean up the RAW data by identifying
## where data is missing, filling in the missing data with an average of 
## similar conditions, and get data ready for processing.

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
library(ggplot2)

#### retrieve data ####
#importing data from stakeholder
C1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C1_SoilTempData.csv", header = TRUE)
C2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C2_SoilTempData.csv", header = TRUE)
T1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T1_SoilTempData.csv", header = TRUE)
T2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T2_SoilTempData.csv", header = TRUE)

## Importing temperature data for study period from NOAA station Santa Fe 2 to
## have an ambient air temperature that can be used for comparison to soil
## temperature data. NOAA station is approximately 3.37 miles away from study 
## site.

#retrieve csv with hourly temperature data
AirTemp = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/NOAA_AirportGauge_AirTemp.csv", header = TRUE)

#### examine date/time ####
#view formats
?strptime

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

## date/time column is currently in "%m/%d/%Y %I:%M %p" format, need to reformat

#examine date/time for AirTemp
class(AirTemp$DATE)
head(AirTemp$DATE)
tail(AirTemp$DATE)
View(AirTemp)

## date/time column is currently in "%m/%d/%Y %H:%M" format, need to reformat

#### reformat date/time ####
#view time zones
OlsonNames()

#create new date/time format
C1$date_time=as.POSIXct(C1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
C2$date_time=as.POSIXct(C2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T1$date_time=as.POSIXct(T1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T2$date_time=as.POSIXct(T2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
AirTemp$date_time=as.POSIXct(AirTemp$DATE, format="%m/%d/%y %H:%M", tz="MST")


#### explore data set size and structure ####

#analyze C1
head(C1)
str(C1)
## Measurement time is a chr, values are all num, date_time is POSIXct

#analyze C2
head(C2)
str(C2)
## Measurement time is a chr, values are all num, date_time is POSIXct

#analyze T1
head(T1)
str(T1)
## X6.inches is inaccurately classed as a character instead of a number, 
## we need to convert it to a numeric form
T1$X6.inches = as.numeric(T1$X6.inches)

#check
class(T1$X6.inches)
## SUCCESS 

#analyze T2
head(T2)
str(T2)
## Measurement time is a chr, values are all num, date_time is POSIXct

#analyze air temperature
head(AirTemp)
str(AirTemp)
## Station is num (will delete this column later), date is chr, temperature is chr, 
## date_time is POSIXct

#convert temperatures to numeric values
AirTemp$HourlyDryBulbTemperature = as.numeric(AirTemp$HourlyDryBulbTemperature)
#check
class(AirTemp$HourlyDryBulbTemperature)
## SUCCESS 

#remove additional hours at the end of AirTemp data
AirTemp <- head(AirTemp, -13)

#remove STATION column from AirTemp
AirTemp <- AirTemp[,-1]


#### playing around with plots and analyzing data ####

factor(AirTemp_reducedhours$HourlyDryBulbTemperature)
## total temperature values identified is 107

factor(C1$X6.inches)
factor(C1$X12.inches)
factor(C1$X18.inches)
factor(C1$X24.inches)
factor(C1$X30.inches)
## total temperature values identified are 366, 289, 260, 244, 228 (respectively),
## indicating that the range of temperatures decreases as one moves deeper beneath
## the surface. feel a reduction from decimal points to whole numbers in stakeholder
## data sets would allow for comparison to ambient air temperature with air temperature
## exhibiting the largest range.

factor(T1$X6.inches)
factor(T1$X12.inches)
factor(T1$X18.inches)
factor(T1$X24.inches)
factor(T1$X30.inches)
## total temperature values identified are 289, 260, 249, 240, and 220 (respectively),
## this makes two things clear: 1) given the excavation to create the rain garden,
## there is a 6 inch difference in depths of a similar consistency/composition
## so when comparing wavelets (much later) I should use the 12 inch control depth for
## the 6 inch experiment site depth and so forth.

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

#create data frame from sequence 
allhours.df <- data.frame(allhours=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "hour", length.out = 63565))

#use the join function 
C1_allhours <- data.frame(left_join(allhours.df, C1, by = c("allhours" = "date_time")))
C2_allhours <- data.frame(left_join(allhours.df, C2, by = c("allhours" = "date_time")))
T1_allhours <- data.frame(left_join(allhours.df, T1, by = c("allhours" = "date_time")))
T2_allhours <- data.frame(left_join(allhours.df, T2, by = c("allhours" = "date_time")))


#### prepare AirTemp data frame for processing ####
#use join function to combine with correct number of hours
AirTemp_allhours <- data.frame(right_join(allhours.df, AirTemp, 
                                           by = c("allhours" = "date_time")))

#sort chronologically
AirTemp_allhours <- AirTemp_allhours[order(AirTemp_allhours$allhours),]

#apply round_date to change hours to whole number
round_date(AirTemp_allhours$allhours, unit = "hours", week_start = 
             getOption("lubridate.week.start", 7))

#make into data frame
rounded.hours <- as.data.frame(round_date(AirTemp_allhours$allhours, unit = "hour", week_start = 
                                            getOption("lubridate.week.start", 7))
)

#rename column within rounded.hours
colnames(rounded.hours)[1] <- "Hours"

#join to AirTemp data frame
AirTemp_rightjoined <- data.frame(right_join(rounded.hours,
                                  AirTemp_allhours,
                                  by = c("Hours" = "allhours")))

#check class of Hours column
class(AirTemp_merged$Hours)

#sort chronologically
AirTemp_merged <- AirTemp_merged[order(AirTemp_merged$Hours),]


## create table for Technical Report ##
AirTemp_allhours[1:10,]
AirTemp_Table <- as.data.frame(AirTemp_allhours[1:10,])

# example code for exporting
# write.csv(DataFrame Name, "Path to export the DataFrame\\File Name.csv", 
# row.names=FALSE)
write.csv(AirTemp_Table, "~/Documents/Data Analysis/R\\AirTemp_Table.csv", 
          row.names = FALSE)                    
                   

#### identify and view identical/missing date's and NA temperature values in data sets ####

#C1
subset(C1_allhours, duplicated(allhours))
## data frame contains 7 duplicates

#C2
subset(C2_allhours, duplicated(allhours))
## data frame contains 7 duplicates (same as C1)

#T1
subset(T1_allhours, duplicated(allhours))
## data frame contains 5 duplicates (same as C1 but with two less)

#T2
subset(T2_allhours, duplicated(allhours))
## data frame contains 7 duplicates (same as C1)

#AirTemp
subset(AirTemp_sorted, duplicated(allhours))
AirTemp_duplicates <- as.data.frame(subset(AirTemp_sorted, duplicated(allhours)))
## data set has 85 duplicates (also NA's)

# identify NA's in data sets

#C1
C1_NAs <- as.data.frame(which(is.na(C1_allhours), arr.ind = TRUE))
## C1 has 42 NA's, two are consecutive hours

#C2
C2_NAs <- as.data.frame(which(is.na(C2_allhours), arr.ind = TRUE))
## C2 has 36 NA's (all same as above except excluding row 234)

#T1
T1_NAs <- as.data.frame(which(is.na(T1_allhours), arr.ind = TRUE))
## T1 has 124 NA's', many are the same as T2 but there are multiple 
## instances in which multiple hours are missing such as the 6 early 
## morning hours from 1/15/16 then noon from that same day to 9am on 1/18/16 for
## the 6 inch depth sensor

#T2
T2_NAs <- as.data.frame(which(is.na(T2_allhours), arr.ind = TRUE))
## T2 has 36 NA's (all same as C2)

#AirTemp
AirTemp_NAs <- as.data.frame(which(is.na(AirTemp_sorted), arr.ind = TRUE))
## AirTemp has 2872 NA's



#### calculate averages for duplicate variables in data sets ####
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
C1_nodupes <- C1_allhours %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

C2_nodupes <- C2_allhours %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

T1_nodupes <- T1_allhours %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

T2_nodupes <- T2_allhours %>% group_by(allhours)  %>% 
  summarize(across(c(X30.inches, X24.inches, X18.inches, X12.inches, X6.inches), mean))

AirTemp_nodupes <- AirTemp_allhours %>% group_by(allhours) %>%
  summarize(across(c(HourlyDryBulbTemperature), mean))

#check class of new date/time column
class(AirTemp_nodupes$allhours)


#now check
subset(Control_1_nodupes, duplicated(allhours))

subset(Control_2_nodupes, duplicated(allhours))

subset(Test_1_nodupes, duplicated(allhours))

subset(Test_2_nodupes, duplicated(allhours))

subset(AmbientAir_nodupes, duplicated(allhours))

#### identify missing values in Air Temp data ####

AirTemp_nodupes[!complete.cases(AirTemp_nodupes),]
view(AirTemp_nodupes[!complete.cases(AirTemp_nodupes),])

AirTemp_ready <- na.omit(AirTemp_nodupes)


#### create average daily values ####
# calculate daily mean
# example code
# data %>% group_by(id, city, time = as.Date(time)) %>% 
# summarise(across(c(temperature, pressure), mean))

C1_Daily <- data.frame(C1_nodupes %>% group_by(alldays = as.Date(allhours))
                  %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                  X12.inches, X6.inches), mean)))

C2_Daily <- data.frame(C2_nodupes %>% group_by(alldays = as.Date(allhours))
                    %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                    X12.inches, X6.inches), mean)))

T1_Daily <- data.frame(T1_nodupes %>% group_by(alldays = as.Date(allhours))
                %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                X12.inches, X6.inches), mean)))

T2_Daily <- data.frame(T2_nodupes %>% group_by(alldays = as.Date(allhours))
                %>% summarise(across(c(X30.inches, X24.inches, X18.inches,
                X12.inches, X6.inches), mean)))

AirTemp_Daily <- data.frame(AirTemp_ready %>% group_by(alldays = as.Date(allhours))
                           %>% summarise(across(c(HourlyDryBulbTemperature), mean)))

# remove excessive decimal places

# example code
# mydf %>% mutate_at(vars(-vch1), funs(round(., 1)))

C1_Daily <- C1_Daily %>% mutate_at(vars(-alldays), funs(round(., 0)))

C2_Daily <- C2_Daily %>% mutate_at(vars(-alldays), funs(round(., 0)))

T1_Daily <- T1_Daily %>% mutate_at(vars(-alldays), funs(round(., 0)))

T2_Daily <- T2_Daily %>% mutate_at(vars(-alldays), funs(round(., 0)))

AirTemp_Daily <- AirTemp_Daily %>% mutate_at(vars(-alldays), funs(round(., 0)))

class(AirTemp_Daily$alldays)
class(AirTemp_Daily$HourlyDryBulbTemperature)
class(T1_Daily$alldays)
class(T1_Daily$X30.inches)


#### read me ####

# Now that our data has been cleaned up, we can create a time series 
# and begin analyzing for autocorrelation.

###

#### more libraries ####

library(xts)
library(imputeTS)
library(tseries)
library(astsa)
library(WaveletComp)


#### Apparently there are NA's, we MUST FILL ####

#make univariate zoo time series
## example code
## ts.temp<-read.zoo(C2_no3, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

Temporary_TS_C1 <- read.zoo(C1_Daily, index.column=1, format="%Y-%m-%d", tz="MST")
view(Temporary_TS_C1 <- read.zoo(C1_Daily, index.column=1, format="%Y-%m-%d", tz="MST"))

Temporary_TS_C2 <- read.zoo(C2_Daily, index.column=1, format="%Y-%m-%d", tz="MST")
Temporary_TS_T1 <- read.zoo(T1_Daily, index.column=1, format="%Y-%m-%d", tz="MST")
Temporary_TS_T2 <- read.zoo(T2_Daily, index.column=1, format="%Y-%m-%d", tz="MST")

Temporary_TS_Air <- read.zoo(AirTemp_Daily, index.column=1, format="%Y-%m-%d", tz="MST")
view(Temporary_TS_Air <- read.zoo(AirTemp_Daily, index.column=1, format="%Y-%m-%d", tz="MST"))
## hourly dry bulb column name changed to "x", will alter later

#fill with spline interpolation #
## example code
## C2_no3_filled_splineinterp = na.spline(ts.temp, na.rm = T, maxgap = 24*4)

C1_filled <- na.spline(Temporary_TS_C1, na.rm = T, maxgap = 24)
C2_filled <- na.spline(Temporary_TS_C2, na.rm = T, maxgap = 24)
T1_filled <- na.spline(Temporary_TS_T1, na.rm = T, maxgap = 24)
T2_filled <- na.spline(Temporary_TS_T2, na.rm = T, maxgap = 24)
AirTemp_filled <- na.spline(Temporary_TS_Air, na.rm = T, maxgap = 24)

#revert back to data frame #

#example code
# C2_no3_filled_splineinterp = as.data.frame(C2_no3_filled_splineinterp)

Control_1 <- as.data.frame(C1_filled)
Control_2 <- as.data.frame(C2_filled)
Test_1 <- as.data.frame(T1_filled)
Test_2 <- as.data.frame(T2_filled)
AirTemperature <- as.data.frame(AirTemp_filled)


# once again, we must reduce decimal points
round(Control_1, digits = 0)
round(Control_2, digits = 0)
round(Test_1, digits = 0)
round(Test_2, digits = 0)
round(AirTemperature, digits = 0)

# make into data frames
Control_1 <- as.data.frame(round(Control_1, digits = 0))
Control_2 <- as.data.frame(round(Control_2, digits = 0))
Test_1 <- as.data.frame(round(Test_1, digits = 0))
Test_2 <- as.data.frame(round(Test_2, digits = 0))
AirTemperature <- as.data.frame(round(AirTemperature, digits = 0))

#### re-add date sequence as a column ####

# create with a sequence that features all days 
seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "day", length.out = 2649)

#create data frame from sequence
alldays <- data.frame(alldays=seq(ISOdatetime(2014,9,01, 00, 00, 00, 'MST'), by = "day", length.out = 2649))

# merge all days sequence with all data frames
Control_1_merged <-merge(data.frame(Control_1, 
                                  row.names = NULL), data.frame(alldays, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

Control_2_merged <-merge(data.frame(Control_2, 
                                  row.names = NULL), data.frame(alldays, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

Test_1_merged <-merge(data.frame(Test_1, 
                               row.names = NULL), data.frame(alldays, row.names=NULL),
                               by = 0, all = TRUE)[-1]

Test_2_merged <-merge(data.frame(Test_2, 
                               row.names = NULL), data.frame(alldays, row.names=NULL),
                               by = 0, all = TRUE)[-1]

Air_merged <-merge(data.frame(AirTemperature, 
                          row.names = NULL), data.frame(alldays, row.names=NULL),
                          by = 0, all = TRUE)[-1]

# sort chronologically 
Control_One_sorted <- Control_1_merged[order(Control_1_merged$alldays),]
Control_Two_sorted <- Control_2_merged[order(Control_2_merged$alldays),]
Test_One_sorted <- Test_1_merged[order(Test_1_merged$alldays),]
Test_Two_sorted <- Test_2_merged[order(Test_2_merged$alldays),]
Air_sorted <- Air_merged[order(Air_merged$alldays),]

# rename column in ambient air temperature data frame 
colnames(Air_sorted) <- c('DailyTemperature', 'alldays')

#### Create a time series ####

## Create time series objects, check class, and plot ## 

# example code
# C2_no3_xts = xts(C2_no3_filled_splineinterp$nitrate_uM_c_bc, order.by = 
# C2_no3_filled_splineinterp$date_timeAK)
# class(C2_no3_xts)
# plot(C2_no3_xts)
# summary(C2_no3_xts)

# Control One
Control_One_30in_xts <- xts(Control_One_sorted$X30.inches, order.by = 
                         Control_One_sorted$alldays)
# check class
class(Control_One_30in_xts)

# change column name
colnames(Control_One_30in_xts) <- c('X30.inches')

# create plot
plot(Control_One_30in_xts)

## repeat for all sensor depths ##

# 24 inches
Control_One_24in_xts <- xts(Control_One_sorted$X24.inches, order.by = 
                         Control_One_sorted$alldays)
colnames(Control_One_24in_xts) <- c('X24.inches')
plot(Control_One_24in_xts)

# 18 inches
Control_One_18in_xts <- xts(Control_One_sorted$X18.inches, order.by = 
                            Control_One_sorted$alldays)
colnames(Control_One_18in_xts) <- c('X18.inches')
plot(Control_One_18in_xts)

# 12 inches
Control_One_12in_xts <- xts(Control_One_sorted$X12.inches, order.by = 
                              Control_One_sorted$alldays)
colnames(Control_One_12in_xts) <- c('X12.inches')
plot(Control_One_12in_xts)

# 6 inches
Control_One_6in_xts <- xts(Control_One_sorted$X6.inches, order.by = 
                              Control_One_sorted$alldays)
colnames(Control_One_6in_xts) <- c('X6.inches')
plot(Control_One_6in_xts)


## Control Two ##

# 30 inches
Control_Two_30in_xts <- xts(Control_Two_sorted$X30.inches, order.by = 
                         Control_Two_sorted$alldays)
colnames(Control_Two_30in_xts) <- c('X30.inches')
plot(Control_Two_30in_xts)

# 24 inches
Control_Two_24in_xts <- xts(Control_Two_sorted$X24.inches, order.by = 
                              Control_Two_sorted$alldays)
colnames(Control_Two_24in_xts) <- c('X24.inches')
plot(Control_Two_24in_xts)

# 18 inches
Control_Two_18in_xts <- xts(Control_Two_sorted$X18.inches, order.by = 
                              Control_Two_sorted$alldays)
colnames(Control_Two_18in_xts) <- c('X18.inches')
plot(Control_Two_18in_xts)

# 12 inches
Control_Two_12in_xts <- xts(Control_Two_sorted$X12.inches, order.by = 
                              Control_Two_sorted$alldays)
colnames(Control_Two_12in_xts) <- c('X12.inches')
plot(Control_Two_12in_xts)

# 6 inches
Control_Two_6in_xts <- xts(Control_Two_sorted$X6.inches, order.by = 
                              Control_Two_sorted$alldays)
colnames(Control_Two_6in_xts) <- c('X6.inches')
plot(Control_Two_6in_xts)

## Test One ##

# 30 inches
Test_One_30in_xts <- xts(Test_One_sorted$X30.inches, order.by = 
                              Test_One_sorted$alldays)
colnames(Test_One_30in_xts) <- c('X30.inches')
plot(Test_One_30in_xts)

# 24 inches
Test_One_24in_xts <- xts(Test_One_sorted$X24.inches, order.by = 
                           Test_One_sorted$alldays)
colnames(Test_One_24in_xts) <- c('X24.inches')
plot(Test_One_24in_xts)

# 18 inches
Test_One_18in_xts <- xts(Test_One_sorted$X18.inches, order.by = 
                           Test_One_sorted$alldays)
colnames(Test_One_18in_xts) <- c('X18.inches')
plot(Test_One_18in_xts)


# 12 inches
Test_One_12in_xts <- xts(Test_One_sorted$X12.inches, order.by = 
                           Test_One_sorted$alldays)
colnames(Test_One_12in_xts) <- c('X12.inches')
plot(Test_One_12in_xts)


# 6 inches
Test_One_6in_xts <- xts(Test_One_sorted$X6.inches, order.by = 
                           Test_One_sorted$alldays)
colnames(Test_One_6in_xts) <- c('X6.inches')
plot(Test_One_6in_xts)


## Test Two ##

# 30 inches
Test_Two_30in_xts <- xts(Test_Two_sorted$X30.inches, order.by = 
                           Test_Two_sorted$alldays)
colnames(Test_Two_30in_xts) <- c('X30.inches')
plot(Test_Two_30in_xts)


# 24 inches
Test_Two_24in_xts <- xts(Test_Two_sorted$X24.inches, order.by = 
                           Test_Two_sorted$alldays)
colnames(Test_Two_24in_xts) <- c('X24.inches')
plot(Test_Two_24in_xts)

# 18 inches
Test_Two_18in_xts <- xts(Test_Two_sorted$X18.inches, order.by = 
                           Test_Two_sorted$alldays)
colnames(Test_Two_18in_xts) <- c('X18.inches')
plot(Test_Two_18in_xts)

# 12 inches
Test_Two_12in_xts <- xts(Test_Two_sorted$X12.inches, order.by = 
                           Test_Two_sorted$alldays)
colnames(Test_Two_12in_xts) <- c('X12.inches')
plot(Test_Two_12in_xts)


# 6 inches
Test_Two_6in_xts <- xts(Test_Two_sorted$X6.inches, order.by = 
                          Test_Two_sorted$alldays)
colnames(Test_Two_6in_xts) <- c('X6.inches')
plot(Test_Two_6in_xts)


## Daily Average Temperature from USGS ##
AirTemperature_xts <- xts(Air_sorted$DailyTemperature, order.by = 
                          Air_sorted$alldays)
colnames(AirTemperature_xts) <- c('DailyTemperature')
plot(AirTemperature_xts)


# check for gaps/NA's
summary(Control_One_30in_xts)
summary(Control_One_24in_xts)
summary(Control_One_18in_xts)
summary(Control_One_12in_xts)
summary(Control_One_6in_xts)
summary(Control_Two_30in_xts)
summary(Control_Two_24in_xts)
summary(Control_Two_18in_xts)
summary(Control_Two_12in_xts)
summary(Control_Two_6in_xts)
summary(Test_One_30in_xts)
summary(Test_One_24in_xts)
summary(Test_One_18in_xts)
summary(Test_One_12in_xts)
summary(Test_One_6in_xts)
summary(Test_Two_30in_xts)
summary(Test_Two_24in_xts)
summary(Test_Two_18in_xts)
summary(Test_Two_12in_xts)
summary(Test_Two_6in_xts)
summary(Ambient_Temperature_xts)
## SUCCESS ##


#### Check for autocorrelation ####

# examine acf and pacf for Control site #1 at 30 inches of depth
forecast::Acf(Control_One_30in_xts, na.action = na.pass, lag.max = 10)
forecast::Pacf(Control_One_30in_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Control_One_6in_xts, na.action = na.pass, lag.max = 10)
forecast::Pacf(Control_One_6in_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Ambient_Temperature_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Test_Two_6in_xts, na.action = na.pass, lag.max = 10)

#### Classic decomposition ####

# prepare data

# Control One
Control_One_30in_ts <- ts(Control_One_sorted$X30.inches,
                          frequency = 90,
                          start = day(min(Control_One_sorted$alldays)))
plot(Control_One_30in_ts)

Control_One_24in_ts <- ts(Control_One_sorted$X24.inches,
                          frequency = 90,
                          start = day(min(Control_One_sorted$alldays)))
plot(Control_One_24in_ts)

Control_One_18in_ts <- ts(Control_One_sorted$X18.inches,
                          frequency = 90,
                          start = day(min(Control_One_sorted$alldays)))
plot(Control_One_18in_ts)

Control_One_12in_ts <- ts(Control_One_sorted$X12.inches,
                          frequency = 90,
                          start = day(min(Control_One_sorted$alldays)))
plot(Control_One_12in_ts)

Control_One_6in_ts <- ts(Control_One_sorted$X6.inches,
                          frequency = 90,
                          start = day(min(Control_One_sorted$alldays)))
plot(Control_One_6in_ts)

# Control Two

# Test One
Test_One_30in_ts <- ts(Test_One_sorted$X30.inches,
                         frequency = 90,
                         start = day(min(Test_One_sorted$alldays)))
plot(Test_One_30in_ts)

Test_One_6in_ts <- ts(Test_One_sorted$X6.inches,
                         frequency = 90,
                         start = day(min(Test_One_sorted$alldays)))
plot(Test_One_6in_ts)

# Test Two

# Air Temperature
Air_Temp_ts <- ts(Air_sorted$AverageDailyTemperature,
                          frequency = 90,
                          start = day(min(Air_sorted$alldays)))
plot(Air_Temp_ts)


# decompose into additive components 
plot(decompose(Control_One_30in_ts))
plot(decompose(Control_One_6in_ts))
plot(decompose(Test_One_30in_ts))
plot(decompose(Test_One_6in_ts))
plot(decompose(Air_Temp_ts))


#### Wavelet ####

library(WaveletComp)
library(nlme)
library(MARSS)
library(beepr)
library(visreg)
library(biwavelet)

## Hypothesis: Soil temperature at the control sites are more closely correlated 
## with ambient air temperature than at the test sites.
## I will compare the wavelet plots of two sites: Control Site #1 and Test Site #2.

#combine Control_One_sorted with Air_sorted for comparison using 6 inch depth column
Control_One_6in_compare = as.data.frame(Control_One_sorted$alldays,
                                        Control_One_sorted$X6.inches,
                                        Air_sorted$DailyTemperature)

Control_One_6in_compare = as.data.frame(Control_One_6in_xts, AirTemperature_xts)

class(Control_One_sorted$alldays)

Test_Two_6in_compare = as.data.frame(Test_Two_sorted$alldays,
                                     Test_Two_sorted$X6.inches,
                                     Air_sorted$AverageDailyTemperature)
class(Test_Two_sorted$alldays)

#run wavelet analysis
#10 simulations for the sake of time
WaveletOne = analyze.coherency(Control_One_6in_compare, c(2,3),
                                           method = "AR",
                                           params = list(AR = list(p = 1)),
                                           dj = 1/20,
                                           lowerPeriod = 1/4,
                                           make.pval = T,
                                           n.sim = 10)

par(mfrow=c(1,1))
wc.image(WaveletOne, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="wavelet coherence"), 
         main="Control Site #1 at 6 inch depth and Ambient Temperature", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3))
