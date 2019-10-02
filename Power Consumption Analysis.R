install.packages("RMySQL")
library(RMySQL)
library(dplyr)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)

## Iris Example 
## Lists attributes contained in a table  
dbListFields(con,'iris')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## Submeter Project 
## Using the dbListFields function learn the attributes associated with the yr_2006 table
dbListFields(con, 'yr_2006')
## Use the dbGetQuery function to download tables 2006 through 2010 with
##Date, Time and the 3 sub-meter attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                      Sub_metering_3 FROM yr_2010")

## Investigate each new data frame
## Use str(), summary(), head() and tail() with each data frame
str(yr_2010)
summary(yr_2006)
head(yr_2010)
tail(yr_2010)

## Combine tables into one dataframe using dplyr
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)
str(newDF)
summary(newDF)
head(newDF)
tail(newDF)

## Preprocessing
## DataTime
## Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF, paste(newDF$Date,newDF$Time), 
                      stringsAsFactors=FALSE)
head(newDF)
## Give the new attribute in the 6th column a header name 
colnames(newDF)[6] <-"DateTime"
## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)
## Convert DateTime from character to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"
## Inspect the data types
str(newDF)

## Lubridate
install.packages("lubridate")
library(lubridate)
## Create "year" attribute with lubridate
newDF$year <- year(newDF$DateTime)
head(newDF)
## quarter, month, week, weekday, day, hour and minute
newDF$quarter <- quarter(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- weekdays(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)

## initial exploration 
summary(newDF)
sd(newDF$Sub_metering_1)
sd(newDF$Sub_metering_2)
sd(newDF$Sub_metering_3)
mode(newDF$Sub_metering_1)
sum(newDF$Sub_metering_1)
sum(newDF$Sub_metering_2)
sum(newDF$Sub_metering_3)

######################################################################################
######################################################################################

## Task 2
#####################################################################################
## Viualization

## Testing
library(tibbletime)
testing_data <- as_tbl_time(sample_frac(newDF, 0.05 ), index = DateTime)
testing_data
as_period(testing_data, '30 minute')

## Granularity
## Plot all of sub-meter 1
newDF <- as_tbl_time(newDF, index = DateTime)
newDF
newDF_30min <- as_period(newDF, '30 minute')
newDF_30min
plot(newDF_30min$Sub_metering_1)

## Subsetting and Meaningful Time Periods
## Subset the second week of 2008 - All Observations
houseWeek <- filter(newDF, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Visualize a Single Day with Plotly
library(plotly)
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(newDF, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, 
        y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', 
                  mode = 'lines') %>%
        add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', 
                  mode = 'lines') %>%
        layout(title = "Power Consumption January 9th, 2008",
               xaxis = list(title = "Time"),
               yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(newDF, year == 2008 & month == 1 & day == 9 
                     & (minute == 0 | minute == 10 | minute == 20 | 
                                minute == 30 | minute == 40 | minute == 50))
houseDay10$minute
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', 
                  mode = 'lines') %>%
        add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', 
                  mode = 'lines') %>%
        layout(title = "Power Consumption January 9th, 2008",
               xaxis = list(title = "Time"),
               yaxis = list (title = "Power (watt-hours)"))

## Create a visualization with plotly for 2nd Week of 2008
newDF_40min <- as_period(newDF, '40 minute')
houseWeek40 <- filter(newDF_40min, year == 2008 & week == 2)
houseWeek40
plot_ly(houseWeek40, x = ~houseWeek40$DateTime, y = ~houseWeek40$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~houseWeek40$Sub_metering_2, name = 'Laundry Room',
                  mode = 'lines') %>%
        add_trace(y = ~houseWeek40$Sub_metering_3, name = 'Water Heater & AC',
                  mode = 'lines') %>%
        layout(title = "Power Consumption Week 2, 2008",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Power (watt-hours)"))

## choose monthly time period and use 60 mins interval
newDF_1h <- as_period(newDF, "1 h")
newDF_1h
houseMonth1h <- filter(newDF_1h, year == 2008, month == 1 )
houseMonth1h
plot_ly(houseMonth1h, x = ~houseMonth1h$DateTime, y = ~houseMonth1h$Sub_metering_1,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~houseMonth1h$Sub_metering_2, name = 'Laundry Room',
                  mode = 'lines') %>%
        add_trace(y = ~houseMonth1h$Sub_metering_3, name = 'Water Heater & AC',
                  mode = 'lines') %>%
        layout(title = "Power Consumption Jan,2008",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Power (watt-hours)"))

##############################################################################
## Prepare to analyze the data

## Subset to one observation per week on Mondays at 8:00pm 
## for 2007, 2008 and 2009
house070809weekly <- filter(newDF, weekday == 'Monday' & hour == 20 & minute == 0)
house070809weekly
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, 
                         frequency=52, start=c(2007,1))
tsSM3_070809weekly
## Produce time series plots
## Plot sub-meter 3 with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 3, Mondays, 20:00 ")

## Create a TS plot for Sub-meter 3 on Saturdays at 20:00
house070809weeklySat <- filter(newDF, weekday == 'Saturday' & hour == 20 & minute == 0)
house070809weeklySat
tsSM3_070809weeklySat <- ts(house070809weeklySat$Sub_metering_3, 
                         frequency=52, start=c(2007,1))
tsSM3_070809weeklySat
plot.ts(tsSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 3, Saturdays, 20:00 ")

## Create a TS plot for Sub-meter 1 on Saturdays at 18:00
house070809weeklySat18 <- filter(newDF, weekday == 'Saturday' & hour == 18 &
                                       minute == 0)
house070809weeklySat18
tsSM1_070809weeklySat18 <- ts(house070809weeklySat18$Sub_metering_1,
                            frequency = 52, start = c(2007,1))
plot.ts(tsSM1_070809weeklySat18, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 1, Saturdays, 20:00 ")

## Create a TS plot for Sub-meter 2 in May, 2007, 2008, 2009
house07Monthly <- filter(newDF_1h, year == 2007 & month == 5 )
tsSM2_07Monthly <- ts(house07Monthly$Sub_metering_2,
                            frequency = 24)
plot.ts(tsSM2_07Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2007")
# May, 2008
house08Monthly <- filter(newDF_1h, year == 2008 & month == 5 )
tsSM2_08Monthly <- ts(house08Monthly$Sub_metering_2,
                      frequency = 24)
plot.ts(tsSM2_08Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2008 ")
# May, 2009
house09Monthly <- filter(newDF_1h, year == 2009 & month == 5 )
tsSM2_09Monthly <- ts(house09Monthly$Sub_metering_2,
                      frequency = 24)
plot.ts(tsSM2_09Monthly, xlab = "Time", ylab = "Watt Hours", 
        main = "Sub-meter 2, May, 2009 ")

######################################################################################
## Forecasting a time serie

## Apply time series linear regression to the sub-meter 3 ts object.
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)
checkresiduals(fitSM3)
## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
summary(forecastfitSM3c)
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time",
     main = "Sub-meter 3 Forecast, Mondays, 20:00 ")

## Forecast of Sub-meter 1 for the first 20 weeks at 18:00 of 2010.
fitSM1 <- tslm(tsSM1_070809weeklySat18 ~ trend + season)
summary(fitSM1)
checkresiduals(fitSM1)
forecastfitSM1 <- forecast(fitSM1, h = 20, level = c(80,90))
plot(forecastfitSM1, ylim = c(0, 70), ylab = "Watt-Hours", xlab = "Time",
     main = "Sub-meter 1 Forecast, Saturdays, 18:00 ")

## Forecast of Sub-meter 2.
fitSM2 <- tslm(tsSM2_07Monthly ~ trend + season)
summary(fitSM2)
checkresiduals(fitSM2)
forecastfitSM2 <- forecast(fitSM2, h = 168, level = c(80, 90))
plot(forecastfitSM2, ylim = c(0, 40), ylab = "Watt-Hours", xlab = "Time",
     main = "Sub-meter 2 Forecast, First Week of Jun, 2007")

#####################################################################################
## Decomposing a seasonal time serie

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
components070809SM3weekly$seasonal
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly$seasonal)
summary(components070809SM3weekly$trend)
summary(components070809SM3weekly$random)

## Sub-meter 1 decomposed plot with your choice of frequency and time period
components070809SM1weekly <- decompose(tsSM1_070809weeklySat18)
plot(components070809SM1weekly)
summary(components070809SM1weekly$seasonal)
summary(components070809SM1weekly$trend)
summary(components070809SM1weekly$random)

## Sub-meter 2 decomposed plot with your choice of frequency and time
components07SM2monthly <- decompose(tsSM2_07Monthly)
plot(components07SM2monthly)
summary(components07SM2monthly$seasonal)
summary(components07SM2monthly$trend)
summary(components07SM2monthly$random)

##################################################################################
## Holt-Winters Forecasting

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809weekly)
tsSM3_HW070809
plot(tsSM3_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(85,95))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 30), ylab= "Watt-Hours", xlab="Time - Sub-meter 3",
     start(2010), main = "Sub-meter 3 HWForecast, Mondays, 20:00")

## Sub_meter 1
tsSM1_070809Adjusted <- tsSM1_070809weeklySat18 - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
plot(decompose(tsSM1_070809Adjusted))
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted)
tsSM1_HW070809
plot(tsSM1_HW070809, ylim = c(0, 50))
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(85,95))
plot(tsSM1_HW070809forC, ylim = c(0, 60), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", 
     start(2010), main = "Sub-meter 1 HWForecast, Saturdays, 20:00")

## Sub_meter 2
tsSM2_07Adjusted <- tsSM2_07Monthly - components07SM2monthly$seasonal
autoplot(tsSM2_07Adjusted)
plot(decompose(tsSM2_07Adjusted))
tsSM2_HW07 <- HoltWinters(tsSM2_07Adjusted)
tsSM2_HW07
tsSM2_HW07for <- forecast(tsSM2_HW07, h=24)
plot(tsSM2_HW07for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
tsSM2_HW07forC <- forecast(tsSM2_HW07, h=24, level=c(85,95))
plot(tsSM2_HW07forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2",
     start(30), main = "Sub-meter 2 HWForecast, May, 2007")

















