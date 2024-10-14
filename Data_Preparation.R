library(readxl)
library(openxlsx)
library(ggplot2)
library(imputeTS)
library(mltools)
library(data.table)
library(forecast)
library(dplyr)
library(rstudioapi)

#Get current working directory
currentPath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(currentPath))

###--- IMPORT THE DATA ---###
#Set paths and filename
projectPath = paste(getwd(), "/", sep="")
dataFile = "2023-11-Elec-train.xlsx"
#Import data (the first date is imported incorrectly)
colTypes = c("list", "numeric", "numeric") #list format works best for fixing the first date
data <- read_xlsx(paste(projectPath, dataFile, sep = ""), col_names=TRUE, col_types = colTypes)
#Fix the dates with a conditional formatting according to datatype in the Timestamp column
dateConvert <- function(x) ifelse(is.character(x[1]),
                                  as.POSIXct(strptime(x[1], tz = "UTC", format = "%m/%d/%Y %H:%M")),
                                  as.POSIXct(x[1])
)
data$Timestamp <- lapply(data$Timestamp, dateConvert)
data$Timestamp = as.POSIXct(unlist(data$Timestamp), tz = "UTC") #The dates were all converted to numeric, so reset the format to a date format
#Fix the column names
colnames(data) <- c("Timestamp", "Power", "Temperature")

###--- FIRST LOOK ---###
#Look at the first 5 days
startpt = 4*4
endpt = 4*24*5 + startpt
plot(x= data$Timestamp[startpt:endpt], y = data$Power[startpt:endpt], type="l")
#Get basic statistical data
summary(data)

###--- TREATMENT OF MISSING DATA ---###
#Examine the missing data (not last day)... these are zeros
plot(x=data$Timestamp[4500:4750], y = data$Power[4500:4750], type="b")
#Convert the 0 values to NA
replace_zeros <- function(x) ifelse(x == 0, NA, x)
data$Power = unlist(lapply(data$Power, replace_zeros))
sum(is.na(data$Power)) - 96 #Count the missing values (other than the final 96 prediction period NAs)
#Check the results on a plot
plot(x=data$Timestamp[4500:4750], y = data$Power[4500:4750], type="b")
#Now interpolate (simple linear function should be reasonable)
fix <- na_interpolation(data$Power[4500:4750], option = "linear", maxgap = 12)
#Check on the plots
plot(x = data$Timestamp[4500:4750], y = fix, type="b", col="red", ylab="Power (kW)", xlab="Date", main="Interpolation of Missing Power Values")
lines(x=data$Timestamp[4500:4750], y = data$Power[4500:4750], type="b", col="black")
#Copy results into the main dataframe
data$Power[4500:4750] = fix

###---ADD FEATURES---###
#Evening (11:15PM to 7AM)
#Weekdays
#Holidays: January 1st or President's day (February 15th)

#Create night feature
calc_Night <- function(x){
  if (x < "07:00:00" | x >= "23:15:00"){
    x = 1
  } else {
    x = 0
  }
}
time <- format(data$Timestamp, format = "%H:%M:%S")
night <- as.data.frame(sapply(time, FUN = calc_Night))
names(night) = "Night" #correct the column name

#Create weekday feature and apply onehot encoding
weekdays <- as.factor(strftime(data$Timestamp,"%A"))
weekdays <- as.data.frame(one_hot(as.data.table(weekdays)))

#Create Holiday feature
date <- format(data$Timestamp, format = "%Y-%m-%d")
calc_Holiday <- function(x){
  holiday_list <- c("2010-01-01","2010-02-15")
  if (x %in% holiday_list){
    x = 1
  } else {
    x = 0
  }
}
holidays <- as.data.frame(sapply(date, FUN = calc_Holiday))
names(holidays) = "Holiday" #correct the column name

#Push the new features into the dataset
data$Night <- night$Night
data$Holiday <- holidays$Holiday
data$Monday <- weekdays$weekdays_Monday
data$Tuesday <- weekdays$weekdays_Tuesday
data$Wednesday <- weekdays$weekdays_Wednesday
data$Thursday <- weekdays$weekdays_Thursday
data$Friday <- weekdays$weekdays_Friday
data$Saturday <- weekdays$weekdays_Saturday
data$Sunday <- weekdays$weekdays_Sunday

#Check the importance of each feature
nbComplete <- dim(data)[1] - sum(is.na(data)) #find where the last row of complete data (no NAs)
x <- data[1:nbComplete,] #Take only the data having values (the NAs are at the end)
x <- subset(x, select = -Timestamp) #Drop Timestamp column
x <- as.data.frame(x) #Make compatible with aov function
data_aov <- aov(x[,1]~x[,2]+x[,3]+x[,4]+x[,11]+x[,10]+x[,9]+x[,8]+x[,7]+x[,6]+x[,5]) 
summary(data_aov) #Temperature, Night and Sunday seem to have high importance. Wednesday and Saturday are somewhat important.
#Check the feature importance in a reduced model
data_aov <- aov(x[,1]~x[,2]+x[,3]+x[,7]+x[,11]+x[,10])
summary(data_aov)
#Saturday is no longer statistically important (though it's close)
data_aov <- aov(x[,1]~x[,2]+x[,3]+x[,7]+x[,11])
summary(data_aov)
#Drop the features that are unneeded
data = subset(data, select = c("Timestamp", "Power", "Temperature", "Night", "Wednesday", "Sunday"))

###--- TRANSFORM INTO A TIME SERIES ---###
#Consider DAILY time series, with first step of the day is at 00:15 and last at 24:00
startStep = 1*4 + 1 # 1:15 AM
endStep = 24*4 - 1 # 11:45 PM
stepsPerDay = 4*24
nbDays = floor(length(data$Timestamp) / stepsPerDay)
#Time series setup in days with increments of 15 minutes
powerTS <- ts(data[,-1], start = c(0,startStep), end = c(nbDays, endStep), frequency = stepsPerDay)
plot(powerTS)
#Verify that all the data has been transferred
length(powerTS[,"Power"]) == length(data$Power)

#---------------------------------------------------------------------------
###----------------------------BASE FUNCTIONS----------------------------###
#---------------------------------------------------------------------------

#Test for normality
normTest <- function(ts) {
  par(mfrow=c(2,1))
  qqnorm(ts[,"Power"], main="Normal Q-Q Plot for Power")
  qqline(ts[,"Power"], col = "purple")
  qqnorm(ts[,"Temperature"], main="Normal Q-Q Plot for Temperature")
  qqline(ts[,"Temperature"], col = "orange")
  par(mfrow=c(1,1))
}

#Box-Cox Transforamtion
#If no lambda is supplied for a Box-Cox transformation, then the base data is used
BC_Transform <- function(ts, lambdaP = -1, lambdaT = -1) {
  if (lambdaP >= 0) {
    ts[,"Power"] <- BoxCox(ts[,"Power"], lambdaP)
  }
  if (lambdaT >= 0) {
    ts[,"Temperature"] <- BoxCox(ts[,"Temperature"], lambdaT)
  }
  return(ts)
}

#Train-test-validate split
#train_end is the number of days and is an INTEGER 
TTP_split <- function(ts, train_end=44) {
  #Split parameters
  validate_days <- 1
  test_end <- nbDays - validate_days
  if (train_end >= test_end){
    train_end = test_end - 1
    print("WARNING: incorrect value supplied for train_end. It was automatically set to test_end minus 1 day")
  }
  #Splits
  train = window(ts, start=c(0, startStep), end=c(train_end, 24*4))
  test = window(ts, start=c(train_end+1, 1), end=c(test_end, 24*4 - 1))
  validate = window(ts, start=c(test_end, 24*4), end=c(nbDays, endStep))
  #Combined versions
  test_validate <- window(powerTS, start=c(train_end+1, 1), end=c(nbDays, endStep))
  train_test <- window(ts, start=c(1, startStep), end=c(test_end, 24*4 - 1))
  return(list("train" = train, "test" = test, "validate" = validate, "test_validate" = test_validate, "train_test" = train_test))
}
#Isolate the validate part of a predicted time series
#MUST BE of class time series, NOT forecast 
isolate_validate <- function(ts){
  validate_days <- 1
  test_end <- nbDays - validate_days
  validate = window(ts, start=c(test_end, 24*4), end=c(nbDays, endStep))
  return(validate)
}

#Mean Average Percentage Error (MAPE) for use in evaluating models
MAPE <- function(Predicted, Actual){
  #Exit early checks
  stopifnot(length(Actual) == length(Predicted))
  stopifnot(0 %in% Actual == FALSE)
  #Calculations
  ratio <- abs((Actual - Predicted)/Actual)
  return( 100 / length(ratio) * sum(ratio))
}

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#Check the normality of the distribution with a QQ plot
normTest(powerTS)

#Check the distributions and calculate the best lambdas for a Box-Cox transformation
#-> the data is Power data is bimodal (Box-Cox cannot correct this), both modes mostly gaussian
#-> the Temperature data is mostly gaussian
par(mfrow=c(2,1))
hist(powerTS[,"Power"], main="Histogram of Power", xlab="kW")
hist(powerTS[,"Temperature"], main="Histogram of Temperature", xlab="°C")
par(mfrow=c(1,1))
#Calculate the lambdas and transform data (store in a temporary variable)
lambdaPower <- BoxCox.lambda(powerTS[,"Power"])
lambdaTemperature <- BoxCox.lambda(powerTS[,"Temperature"])
tempP <- BoxCox(powerTS[,"Power"], lambdaPower)
tempT <- BoxCox(powerTS[,"Temperature"], lambdaTemperature)
#Plot the transformed data... are they more gaussian?
par(mfrow=c(2,1))
hist(tempP, main='BoxCox Transform on Power', xlab=paste('Values transformed with lambda =', lambdaPower))
hist(tempT, main='BoxCox Transform on Temperature', xlab=paste('Values transformed with lambda =', lambdaTemperature))
par(mfrow=c(1,1))
#->The transformations do not appear to help in any way

#Train-Test Split
split_TS <- TTP_split(powerTS)
power_train <- split_TS$train
power_test <- split_TS$test
power_validate <- split_TS$validate
power_test_validate <- split_TS$test_validate
power_train_test <- split_TS$train_test
#Examine the cutoffs in detail.. NA values should only appear in power_validate
head(power_train)
tail(power_train)
head(power_test)
tail(power_test)
head(power_validate)
tail(power_validate)
tail(powerTS) #The end of the full time series should be the same as the validate time series
#Plot train/test data
par(mfrow=c(2,1))
plot(power_train[,1], xlim=c(1,nbDays+2), ylab = "Electricity Demand (kW)")
lines(power_test[,1], type="l", col="red")
plot(power_train[,2], xlim=c(1,nbDays+2), ylab = "Outside Temperature (°C)")
lines(power_test[,2], type="l", col="red")
lines(power_validate[,2], type="l", col="blue")
par(mfrow=c(1,1))