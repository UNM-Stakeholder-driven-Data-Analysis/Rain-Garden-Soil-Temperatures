#### read me ####

# The purpose of the first half of this project is to clean up the data sets from the 
# stakeholder and the temperature information downloaded from NOAA. Data frame structure
# and size will be calculated and the content analyzed. Data will be corrected by
# averaging duplicates and interpolating missing values to form a continuous
# time series at which point it will be readu for processing.

#### libraries ####

library(tidyverse)
library(lubridate)
library(dplyr)
library(psych)
library(car)
library(tsibble)
library(reshape2)
library(ggplot2)



#### retrieve data ####

#importing data from stakeholder
C1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C1_SoilTempData.csv", header = TRUE)
C2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/C2_SoilTempData.csv", header = TRUE)
T1 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T1_SoilTempData.csv", header = TRUE)
T2 = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/T2_SoilTempData.csv", header = TRUE)

#importing data from NOAA
AirTemp = read.csv("~/Documents/Data Analysis/R/Southwest Urban Hydrology/Rain-Garden-Soil-Temperatures/inside data/NOAA_AirportGauge_AirTemp.csv", header = TRUE)

# Hourly temperature data from NOAA station Santa Fe 2 (SF Airport) is used for 
# comparison to soil temperature data. NOAA station is approximately 3.37 miles 
# away from study site.



#### examine date/time ####

#view time formats
?strptime

#examine date/time for soil depth data frame
class(C1$Measurement.Time)
head(C1$Measurement.Time)
tail(C1$Measurement.Time)
View(C1)

class(C2$Measurement.Time)
head(C2$Measurement.Time)
tail(C2$Measurement.Time)
View(C2)

class(T1$Measurement.Time)
head(T1$Measurement.Time)
tail(T1$Measurement.Time)
View(T1)

class(T2$Measurement.Time)
head(T2$Measurement.Time)
tail(T2$Measurement.Time)
View(T2)

# Date/time columns for all four sites are currently in "%m/%d/%Y %I:%M %p" 
# format, need to reformat

#examine date/time for air temperature data frame
class(AirTemp$DATE)
head(AirTemp$DATE)
tail(AirTemp$DATE)
View(AirTemp)

# Date/time column is currently in "%m/%d/%Y %H:%M" format, need to reformat



#### reformat date/time ####

#view time zones
OlsonNames()

#create new date/time format for all data frames
C1$date_time=as.POSIXct(C1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
C2$date_time=as.POSIXct(C2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T1$date_time=as.POSIXct(T1$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
T2$date_time=as.POSIXct(T2$Measurement.Time, format="%m/%d/%Y %I:%M %p", tz="MST")
AirTemp$date_time=as.POSIXct(AirTemp$DATE, format="%m/%d/%y %H:%M", tz="MST")



#### explore all data set size and structure ####

#analyze C1
head(C1)
str(C1)
# Measurement time is a chr, values are all num, date_time is POSIXct

#analyze C2
head(C2)
str(C2)
# Measurement time is a chr, values are all num, date_time is POSIXct

#analyze T1
head(T1)
str(T1)
# X6.inches is inaccurately classed as a character instead of a number, 
# we need to convert it to a numeric form
T1$X6.inches = as.numeric(T1$X6.inches)

#check
class(T1$X6.inches)
## SUCCESS 

#analyze T2
head(T2)
str(T2)
# Measurement time is a chr, values are all num, date_time is POSIXct

#analyze air temperature
head(AirTemp)
str(AirTemp)
# Station is num (will delete this column later), date is chr, temperature is 
# chr, temperature is in Fahrenheit (will convert), and date_time is POSIXct

#convert temperature column to numeric values
AirTemp$HourlyDryBulbTemperature = as.numeric(AirTemp$HourlyDryBulbTemperature)
#check
class(AirTemp$HourlyDryBulbTemperature)
## SUCCESS 

#remove additional hours at the end of AirTemp data
AirTemp <- head(AirTemp, -13)

#remove STATION column from AirTemp
AirTemp <- AirTemp[,-1]

#convert Fahrenheit to Celsius
AirTemp_C <- AirTemp %>% mutate(Celsius = (HourlyDryBulbTemperature-32)*(5/9))

#apply round_date to round hours to nearest whole number 
AirTemp_C$date_time_rounded = round_date(AirTemp_C$date_time, unit = "hours")
# The first date/time is 12:53AM and rounds up to 1:00AM, need to delete the
# first hour from the soil temperature data sets to match

#remove first hour from all soil temperature data frames
C1 <- tail(C1,-1)
C2 <- tail(C2,-1)
T1 <- tail(T1,-1)
T2 <- tail(T2,-1)



#### analyze data values ####

factor(AirTemp_C$Celsius)
# Total number of temperature values identified is 107

factor(C1$X6.inches)
factor(C1$X12.inches)
factor(C1$X18.inches)
factor(C1$X24.inches)
factor(C1$X30.inches)
# Total number of temperature values identified are 366, 289, 260, 244, and 
# 228 (respectively), indicating that the range of temperatures decreases as 
# one moves deeper beneath the surface of the soil.

factor(T1$X6.inches)
factor(T1$X12.inches)
factor(T1$X18.inches)
factor(T1$X24.inches)
factor(T1$X30.inches)
# Total temperature values identified are 289, 257, 249, 240, and 220 
# (respectively). This makes two things clear and prior research on these sites: 
# 1) given the excavation to create the rain garden,
# there is a 6 inch difference in depths of a similar consistency/composition
# so when comparing wavelets (much later) I should use the 12 inch control depth 
# for the 6 inch experiment site depth and so forth.

factor(C2$X6.inches)
factor(C2$X12.inches)
factor(C2$X18.inches)
factor(C2$X24.inches)
factor(C2$X30.inches)
# Total number of temperature values identified are 325, 277, 254, 229, and 
# 223 (respectively). 

factor(T2$X6.inches)
factor(T2$X12.inches)
factor(T2$X18.inches)
factor(T2$X24.inches)
factor(T2$X30.inches)
# Total number of temperature values identified are 263, 254, 239, 231, and 
# 214 (respectively).



#### calculate correct number of hours ####

#get first and last days
summary(C1)

#calculating total number of days in study period
startDate <-as.Date("2014-09-01 01:00:00", tz = "MST")
endDate <-as.Date("2021-12-01 12:00:00", tz = "MST")
Noofdays <- endDate - startDate
view(Noofdays)
# Total days is 2648

#calculating total hours
difftime(startDate, endDate, units = "hours")
# Number of hours is 63552, so we know this is the number of observations we 
# can expect once the data frames are ready for processing

# The above is incorrect as evidenced when creating data frame from sequence of
# datetime in next section...

#### create sequence with correct number of rows ####

#create data frame from sequence 
allhours.df <- data.frame(allhours=seq(ISOdatetime(2014,9,01, 01, 00, 00, 'MST'), 
                                       by = "hour", length.out = 63564))
# The number 63564 was determined after running the code with the original value
# of 63552 and falling short of end value (2021-12-01 12:00:00)

#use the join function
C1_allhours <- data.frame(left_join(allhours.df, C1, by = c("allhours" = "date_time")))
C2_allhours <- data.frame(left_join(allhours.df, C2, by = c("allhours" = "date_time")))
T1_allhours <- data.frame(left_join(allhours.df, T1, by = c("allhours" = "date_time")))
T2_allhours <- data.frame(left_join(allhours.df, T2, by = c("allhours" = "date_time")))
AirTemp_allhours <- data.frame(left_join(allhours.df, AirTemp_C, 
                                         by = c("allhours" = "date_time_rounded")))

          

#### identify identical/missing values in data sets ####

#C1
subset(C1_allhours, duplicated(allhours))
# Data frame contains 7 duplicates

#C2
subset(C2_allhours, duplicated(allhours))
# Data frame contains 7 duplicates (same duplicated hours as C1)

#T1
subset(T1_allhours, duplicated(allhours))
# Data frame contains 5 duplicates (same duplicated hours as C1 but no duplicates
# for 2014-11-14 11:00:00 or 2016-01-14 15:00:00)

#T2
subset(T2_allhours, duplicated(allhours))
# Data frame contains 7 duplicates (same duplicated hours as C1)

#AirTemp
subset(AirTemp_allhours, duplicated(allhours))
AirTemp_duplicates <- as.data.frame(subset(AirTemp_allhours, duplicated(allhours)))
# Data frame contains 11,133 duplicates (also NA's)


#C1
C1_NAs <- as.data.frame(which(is.na(C1_allhours), arr.ind = TRUE))
# C1 has 42 NA's representing 7 missing hours, two of which are consecutive 

#C2
C2_NAs <- as.data.frame(which(is.na(C2_allhours), arr.ind = TRUE))
# C2 has 36 NA's representing 6 missing hours (all same as C1 except 
# excluding row 234)

#T1
T1_NAs <- as.data.frame(which(is.na(T1_allhours), arr.ind = TRUE))
# T1 has 124 NA's', many are the same as T2 but there are multiple 
# instances in which multiple consecutive hours are missing such as the 6 early 
# morning hours from 2016-01-15 then noon from that same day to 9:00AM on 
# 2016-01-18 for the 6 inch depth sensor.

#T2
T2_NAs <- as.data.frame(which(is.na(T2_allhours), arr.ind = TRUE))
# T2 has 36 NA's (all same as C2)

#AirTemp
AirTemp_NAs <- as.data.frame(which(is.na(AirTemp_allhours), arr.ind = TRUE))
# AirTemp has 5,836 NA's, too many to identify consecutive missing hours at this
# time.



#### calculate averages for duplicate variables in data sets ####

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
  summarize(across(c(Celsius), mean))

#check class of new date/time column
class(AirTemp_nodupes$allhours)

#now check
subset(C1_nodupes, duplicated(allhours))

subset(C2_nodupes, duplicated(allhours))

subset(T1_nodupes, duplicated(allhours))

subset(T2_nodupes, duplicated(allhours))

subset(AirTemp_nodupes, duplicated(allhours))



#### read me ####

# Now that our data has been cleaned up, it is ready for more processing. We 
# can create a time series to fill gaps using interpolation and begin analyzing
# the time structure more in preparation for the generation of wavelets.



#### more libraries ####

library(xts)
library(imputeTS)
library(tseries)
library(astsa)
library(WaveletComp)
library(beepr)



#### create time series & fill with spline interpolation ####

#create time series
Temporary_TS_C1 <- read.zoo(C1_nodupes, index.column = 1, format ="%Y-%m-%d %H:%M:%S", tz="MST")

Temporary_TS_T2 <- read.zoo(T2_nodupes, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="MST")
view(Temporary_TS_T2 <- read.zoo(AirTemp_nodupes, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="MST"))

Temporary_TS_Air <- read.zoo(AirTemp_nodupes, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="MST")
view(Temporary_TS_Air <- read.zoo(AirTemp_nodupes, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="MST"))
# Hourly dry bulb column name changed to "x", will alter later

#fill with spline interpolation 
C1_filled <- na.spline(Temporary_TS_C1, na.rm = T, maxgap = 24*1)
C2_filled <- na.spline(Temporary_TS_C2, na.rm = T, maxgap = 24*1)
T1_filled <- na.spline(Temporary_TS_T1, na.rm = T, maxgap = 24*1)
T2_filled <- na.spline(Temporary_TS_T2, na.rm = T, maxgap = 24*1)
AirTemp_filled <- na.spline(Temporary_TS_Air, na.rm = T, maxgap = 24*2)

#revert back to data frame 
C1_spline <- as.data.frame(C1_filled)
C2_spline <- as.data.frame(C2_filled)
T1_spline <- as.data.frame(T1_filled)
T2_spline <- as.data.frame(T2_filled)
AirTemp_spline <- as.data.frame(AirTemp_filled)

#re-add date sequence as a column by merging with allhours data frame
C1_merged <-merge(data.frame(C1_spline, 
                                  row.names = NULL), data.frame(allhours.df, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

C2_merged <-merge(data.frame(C2_spline, 
                                  row.names = NULL), data.frame(allhours.df, row.names=NULL),
                                  by = 0, all = TRUE)[-1]

T1_merged <-merge(data.frame(T1_spline, 
                               row.names = NULL), data.frame(allhours.df, row.names=NULL),
                               by = 0, all = TRUE)[-1]

T2_merged <-merge(data.frame(T2_spline, 
                               row.names = NULL), data.frame(allhours.df, row.names=NULL),
                               by = 0, all = TRUE)[-1]

AirTemp_merged <-merge(data.frame(AirTemp_spline, 
                          row.names = NULL), data.frame(allhours.df, row.names=NULL),
                          by = 0, all = TRUE)[-1]

#sort chronologically 
C1_sorted <- C1_merged[order(C1_merged$allhours),]
C2_sorted <- C2_merged[order(C2_merged$allhours),]
T1_sorted <- T1_merged[order(T1_merged$allhours),]
T2_sorted <- T2_merged[order(T2_merged$allhours),]
AirTemp_sorted <- AirTemp_merged[order(AirTemp_merged$allhours),]

#rename columns in all data frames
colnames(C1_sorted) <- c('X30.inches', 'X24.inches', 'X18.inches', 'X12.inches',
                           'X6.inches', 'Date_Time')
colnames(C2_sorted) <- c('X30.inches', 'X24.inches', 'X18.inches', 'X12.inches',
                           'X6.inches', 'Date_Time')
colnames(T1_sorted) <- c('X30.inches', 'X24.inches', 'X18.inches', 'X12.inches',
                        'X6.inches', 'Date_Time')
colnames(T2_sorted) <- c('X30.inches', 'X24.inches', 'X18.inches', 'X12.inches',
                        'X6.inches', 'Date_Time')
colnames(AirTemp_sorted) <- c('Temperature', 'Date_Time')

#### create time series objects  ####

## Control Site #2 

#30 inches
Control_Site_Two_30in_TimeSeries <- xts(C2_sorted$X30.inches, order.by = 
                         C2_sorted$Date_Time)
colnames(Control_Site_Two_30in_TimeSeries) <- c('X30.inches')
plot(Control_Site_Two_30in_TimeSeries)

#24 inches
Control_Site_Two_24in_TimeSeries <- xts(C2_sorted$X24.inches, order.by = 
                              C2_sorted$Date_Time)
colnames(Control_Site_Two_24in_TimeSeries) <- c('X24.inches')
plot(Control_Site_Two_24in_TimeSeries)

#18 inches
Control_Site_Two_18in_TimeSeries <- xts(C2_sorted$X18.inches, order.by = 
                              C2_sorted$Date_Time)
colnames(Control_Site_Two_18in_TimeSeries) <- c('X18.inches')
plot(Control_Site_Two_18in_TimeSeries)

#12 inches
Control_Site_Two_12in_TimeSeries <- xts(C2_sorted$X12.inches, order.by = 
                              C2_sorted$Date_Time)
colnames(Control_Site_Two_12in_TimeSeries) <- c('X12.inches')
plot(Control_Site_Two_12in_TimeSeries)

#6 inches
Control_Site_Two_6in_TimeSeries <- xts(C2_sorted$X6.inches, order.by = 
                              C2_sorted$Date_Time)
colnames(Control_Site_Two_6in_TimeSeries) <- c('X6.inches')
plot(Control_Site_Two_6in_TimeSeries)


## Test Site #2

#30 inches
Test_Two_30in_TimeSeries <- xts(T2_sorted$X30.inches, order.by = 
                           T2_sorted$Date_Time)
colnames(Test_Two_30in_TimeSeries) <- c('X30.inches')
plot(Test_Two_30in_TimeSeries)


#24 inches
Test_Two_24in_TimeSeries <- xts(T2_sorted$X24.inches, order.by = 
                           T2_sorted$Date_Time)
colnames(Test_Two_24in_TimeSeries) <- c('X24.inches')
plot(Test_Two_24in_TimeSeries)

#18 inches
Test_Two_18in_TimeSeries <- xts(T2_sorted$X18.inches, order.by = 
                           T2_sorted$Date_Time)
colnames(Test_Two_18in_TimeSeries) <- c('X18.inches')
plot(Test_Two_18in_TimeSeries)

#12 inches
Test_Two_12in_TimeSeries <- xts(T2_sorted$X12.inches, order.by = 
                           T2_sorted$Date_Time)
colnames(Test_Two_12in_TimeSeries) <- c('X12.inches')
plot(Test_Two_12in_TimeSeries)


#6 inches
Test_Two_6in_TimeSeries <- xts(T2_sorted$X6.inches, order.by = 
                          T2_sorted$Date_Time)
colnames(Test_Two_6in_TimeSeries) <- c('X6.inches')
plot(Test_Two_6in_TimeSeries)


## Air Temperature

AirTemperature_xts <- xts(Air_sorted$Temperature, order.by = 
                          Air_sorted$allhours)
colnames(AirTemperature_xts) <- c('Temperature')
plot(AirTemperature_xts)


# check for gaps/NA's
summary(Control_Two_30in_xts)
summary(Control_Two_24in_xts)
summary(Control_Two_18in_xts)
summary(Control_Two_12in_xts)
summary(Control_Two_6in_xts)
summary(Test_Two_30in_xts)
summary(Test_Two_24in_xts)
summary(Test_Two_18in_xts)
summary(Test_Two_12in_xts)
summary(Test_Two_6in_xts)
summary(AirTemperature_xts)
## SUCCESS 


#### check for autocorrelation ####

#examine both auto and partial correlation
forecast::Acf(Control_One_30in_xts, na.action = na.pass, lag.max = 10)
forecast::Pacf(Control_One_30in_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Control_One_6in_xts, na.action = na.pass, lag.max = 10)
forecast::Pacf(Control_One_6in_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Ambient_Temperature_xts, na.action = na.pass, lag.max = 10)
forecast::Acf(Test_Two_6in_xts, na.action = na.pass, lag.max = 10)


#### classic decomposition ####

#prepare data

#Control One
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

#Control Two


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


#### wavelet analysis ####

## Hypothesis: Soil temperature at the control sites are more closely correlated 
## with ambient air temperature than at the test sites.
## I will compare the wavelet plots of two sites: Control Site #1 and Test Site #2.

#combine data frame of sites with air temperature using left join 
Control_Two <- data.frame(left_join(C2_sorted, AirTemp_sorted,  
                                    by = c("Date_Time")))
Test_Two <- data.frame(left_join(T2_sorted, AirTemp_sorted,  
                                    by = c("Date_Time")))


#run Wavelet analysis on all five sensor depths at two selected sites

## Control Two

Control_Two_30in_wavelet = analyze.coherency(Control_Two, c(1,7),
                           method = "AR",
                           params = list(AR = list(p = 1)),
                           dt=1/24,
                           dj = 1/20,
                           lowerPeriod = 1/4,
                           upperPeriod = 365,
                           make.pval = T,
                           n.sim = 100)

Control_Two_24in_wavelet = analyze.coherency(Control_Two, c(2,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100); beep("fanfare")

Control_Two_18in_wavelet = analyze.coherency(Control_Two, c(3,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100)


Control_Two_12in_wavelet = analyze.coherency(Control_Two, c(4,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100);  beep("fanfare")


Control_Two_6in_wavelet = analyze.coherency(Control_Two, c(5,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100); beep("fanfare")


#generate wavelet plots

plot(Control_Two_30in_TimeSeries)
par(mfrow=c(1,1))
wc.image(Control_Two_30in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Control Site #2 at 30 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("fanfare")

par(mfrow=c(1,1))
wc.image(Control_Two_24in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Control Site #2 at 24 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3))

par(mfrow=c(1,1))
wc.image(Control_Two_18in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Control Site #2 at 18 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3))

par(mfrow=c(1,1))
wc.image(Control_Two_12in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Control Site #2 at 12 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3))

par(mfrow=c(1,1))
wc.image(Control_Two_6in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Control Site #2 at 6 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("fanfare")



## Test Two

Test_Two_30in_wavelet = analyze.coherency(Test_Two, c(1,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100)

Test_Two_24in_wavelet = analyze.coherency(Test_Two, c(2,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100); beep("fanfare")

Test_Two_18in_wavelet = analyze.coherency(Test_Two, c(3,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100)


Test_Two_12in_wavelet = analyze.coherency(Test_Two, c(4,7),
                                             method = "AR",
                                             params = list(AR = list(p = 1)),
                                             dt=1/24,
                                             dj = 1/20,
                                             lowerPeriod = 1/4,
                                             upperPeriod = 365,
                                             make.pval = T,
                                             n.sim = 100);  beep("fanfare")


Test_Two_6in_wavelet = analyze.coherency(Test_Two, c(5,7),
                                            method = "AR",
                                            params = list(AR = list(p = 1)),
                                            dt=1/24,
                                            dj = 1/20,
                                            lowerPeriod = 1/4,
                                            upperPeriod = 365,
                                            make.pval = T,
                                            n.sim = 100); beep("fanfare")


#generate wavelet plots

par(mfrow=c(1,1))
wc.image(Test_Two_30in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Test Site #2 at 30 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("shotgun")

par(mfrow=c(1,1))
wc.image(Test_Two_24in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Test Site #2 at 24 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("wilhelm") 

par(mfrow=c(1,1))
wc.image(Test_Two_18in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Test Site #2 at 18 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("facebook")

par(mfrow=c(1,1))
wc.image(Test_Two_12in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Test Site #2 at 12 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("treasure")

par(mfrow=c(1,1))
wc.image(Test_Two_6in_wavelet, 
         which.image="wc", 
         color.key="quantile", 
         plot.ridge=FALSE, 
         legend.params=list(lab="Significance intensity color scale"),
         timelab = "", periodlab = "period(days)",
         main="Test Site #2 at 6 inch depth", 
         clear.area=F, exponent = 3, siglvl.contour = (0.05/3)); beep("fanfare")



