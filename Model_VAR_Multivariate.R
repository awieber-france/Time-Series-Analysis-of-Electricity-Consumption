library(tseries)

#Get directory for current ile
fileDir = rstudioapi::getActiveDocumentContext()$path
if (getwd() != fileDir){ setwd(dirname(fileDir)) }
source("Data_Preparation.R")

#----------------------------------------------------------------------------
#---------------------------------MULTIVARIATE---------------------------------
#----------------------------------------------------------------------------
#Check the covariances between the two variables "power" and "temperature"
ccf(power_train[,1], power_train[,2])

#Remove the "Night" variable (the inverse cannot be calculated since it is a subset of the time variable)
if ("Night" %in% colnames(power_train)){
  pos <- match("Night", colnames(power_train))
  train <- power_train[,-pos] }
if ("Night" %in% colnames(power_test)){
  pos <- match("Night", colnames(power_test))
  test <- power_test[,-pos] }
if ("Night" %in% colnames(power_validate)){
  pos <- match("Night", colnames(power_validate))
  validate <- power_validate[,-pos] }
if ("Night" %in% colnames(power_test_validate)){
  pos <- match("Night", colnames(power_train_test))
  train_test <- power_train_test[,-pos] }
if ("Night" %in% colnames(power_test_validate)){
  pos <- match("Night", colnames(power_test_validate))
  test_validate <- power_test_validate[,-pos] }

###Vectorial Auto-regressive model - Constant
res_const_lag_search <- VARselect(train, lag.max = 4*8, type="const", season=96) #run the search for best lag order
n_const <- res_const_lag_search$selection["AIC(n)"] #retrieve best value for the lag order 
var_const <- VAR(train, p=n_const, type = "const", season = 96)
summary(var_const)
#Serial correlation test
serial.test(var_const, lags.pt = 24, type="PT.asymptotic")
serial.test(var_const, lags.pt = 96, type="PT.asymptotic")
#Heteroscedasticity (volatility)
arch.test(var_const, lags.multi = 24, multivariate.only = TRUE) #suffers from volatility!
#Normality test
normality.test(var_const, multivariate.only = TRUE) #residuals are not normally distributed and fail skewness + kurtosis
#Structural breaks test on residuals (stability)
var_const_stability <- stability(var_const, type="OLS-CUSUM")
plot(var_const_stability) #FAILED: the chart of "Power" exceeds the significance limits (red lines)
#Granger causality (not correlation)
causality(var_const, cause="Temperature") #no Granger causality

###Vectorial Auto-regressive model - Trend
res_trend_lag_search <- VARselect(train, lag.max = 4*8, type="trend", season=96) #run the search for best lag order
n_trend <- res_trend_lag_search$selection["AIC(n)"] #retrieve best value for the lag order 
var_trend<- VAR(train, p=n_trend, type = "trend", season = 96)
summary(var_trend)
#Serial correlation test
serial.test(var_trend, lags.pt = 24, type="PT.asymptotic")
serial.test(var_trend, lags.pt = 96, type="PT.asymptotic")
#Heteroscedasticity (volatility)
arch.test(var_trend, lags.multi = 24, multivariate.only = TRUE) #suffers from volatility!
#Normality test
normality.test(var_trend, multivariate.only = TRUE) #residuals are not normally distributed and fail skewness + kurtosis
#Structural breaks test on residuals (stability)
var_trend_stability <- stability(var_trend, type="OLS-CUSUM")
plot(var_trend_stability) #PASSED: the chart of "Power" is inside the significance limits (red lines)
#Granger causality (not correlation)
causality(var_trend, cause="Temperature") #no Granger causality

###Vectorial Auto-regressive model - Both Constant and Trend 
res_both_lag_search <- VARselect(train, lag.max = 4*8, type="both", season=96) #run the search for best lag order
n_both <- res_both_lag_search$selection["AIC(n)"] #retrieve best value for the lag order 
var_both <- VAR(train, p=n_both, type = "both", season = 96)
summary(var_both)
#Serial correlation test
serial.test(var_both, lags.pt = 24, type="PT.asymptotic")
serial.test(var_both, lags.pt = 96, type="PT.asymptotic")
#Heteroscedasticity (volatility)
arch.test(var_both, lags.multi = 24, multivariate.only = TRUE) #suffers from volatility!
#Normality test
normality.test(var_both, multivariate.only = TRUE) #residuals are not normally distributed and fail skewness + kurtosis
#Structural breaks test on residuals (stability)
var_both_stability <- stability(var_both, type="OLS-CUSUM")
plot(var_both_stability) #PASSED: the chart of "Power" is inside the significance limits (red lines)
#Granger causality (not correlation)
causality(var_both, cause="Temperature") #no Granger causality

#Forecast
fcast_var_const <- predict(var_const, n.ahead=length(test[,1]))
fcast_var_trend <- predict(var_trend, n.ahead=length(test[,1]))
fcast_var_both <- predict(var_both, n.ahead=length(test[,1]))

#Plot POWER predictions (test period)
#Convert predicted power to timeseries for proper plotting
temp1 <- ts(fcast_var_const$fcst$Power[,"fcst"], start = start(test), frequency = frequency(test))
temp2 <- ts(fcast_var_trend$fcst$Power[,"fcst"], start = start(test), frequency = frequency(test))
temp3 <- ts(fcast_var_both$fcst$Power[,"fcst"], start = start(test), frequency = frequency(test))
#Plot
plot(test[,1], type="l", col="black", main="VAR Model Predictions", xlab="Time (days)", ylab = "Electricity Demand (kW)", ylim=c(0, 400))
lines(temp1, type="l", col="magenta")
lines(temp2, type="l", col="darkturquoise")
lines(temp3, type="l", col="coral")
legend("bottomright", inset=.01, title="Model",
       c("Data","VAR Constant","VAR Trend","VAR Both"),
       fill=c("black", "magenta","darkturquoise","coral"), horiz=FALSE)

#Plot TEMPERATURE predictions (test period)
#Convert predicted power to timeseries for proper plotting
temp1 <- ts(fcast_var_const$fcst$Temperature[,"fcst"], start = start(test), frequency = frequency(test))
temp2 <- ts(fcast_var_trend$fcst$Temperature[,"fcst"], start = start(test), frequency = frequency(test))
temp3 <- ts(fcast_var_both$fcst$Temperature[,"fcst"], start = start(test), frequency = frequency(test))
#Plot
plot(test[,2], type="l", col="black", main="VAR Model Predictions", xlab="Time (days)", ylab = "Temperature (°C)", ylim=c(5, 25))
lines(temp1, type="l", col="magenta")
lines(temp2, type="l", col="darkturquoise")
lines(temp3, type="l", col="coral")
legend("bottomright", inset=.01, title="Model",
       c("Data","VAR Constant","VAR Trend","VAR Both"),
       fill=c("black", "magenta","darkturquoise","coral"), horiz=FALSE)

#Test Scores
MAPE(fcast_var_const$fcst$Power[,"fcst"], test[,"Power"])
MAPE(fcast_var_trend$fcst$Power[,"fcst"], test[,"Power"])
MAPE(fcast_var_both$fcst$Power[,"fcst"], test[,"Power"])

rmse(fcast_var_const$fcst$Power[,"fcst"], test[,"Power"])
rmse(fcast_var_trend$fcst$Power[,"fcst"], test[,"Power"])
rmse(fcast_var_both$fcst$Power[,"fcst"], test[,"Power"])
#-> best choice is VAR "both" since the scores are best in MAPE and almost best in RMSE but has the advantage
#   of being stable (see the the stability test of type OLS-CUSUM) 

#Validation
validate_res_const = predict(var_const, n.ahead=length(test_validate[,1]))
validate_res_trend = predict(var_trend, n.ahead=length(test_validate[,1]))
validate_res_both = predict(var_both, n.ahead=length(test_validate[,1]))

#Write results to file
write.csv2(validate_res_const$fcst$Power, file="validate_res_const.csv")
write.csv2(validate_res_trend$fcst$Power, file="validate_res_trend.csv")
write.csv2(validate_res_both$fcst$Power, file="validate_res_both.csv")
#Write results to Excel
temp <- ts(validate_res_both$fcst$Power[,"fcst"], start=start(test_validate), frequency=frequency(test_validate))
write.xlsx(isolate_validate(temp), file='VAR_Mutivariate_results.xlsx', colNames = FALSE)

#CROSS VALIDATION
#The time series must have the predict variable in the first column
#cv_start and cv_end makes the range of days to use for the training end point 
cross_val <- function(ts, cv_start=5, cv_end=48, type="both"){
  MAPE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  RMSE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  i <- 1 #counter
  for (day in cv_start:cv_end){
    split_TS <- TTP_split(ts, train_end=day)
    cv_train <- split_TS$train
    cv_test <- split_TS$test
    #Set prediction steps
    res_lag_search <- VARselect(cv_train, lag.max = 4*8, type=type, season=96) #run the search for best lag order
    n <- res_lag_search$selection["AIC(n)"] #retrieve best value for the lag order 
    model <- VAR(cv_train, p=n, type = type, season = 96)
    pred <- predict(model, n.ahead = length(cv_test[,1]))
    mean_pred <- pred$fcst$Power
    #Calculate scores
    MAPE_results[i] = MAPE(mean_pred[,1], cv_test[, 1])
    RMSE_results[i] = rmse(mean_pred[,1], cv_test[, 1])
    #Print results during run
    print(sprintf("Day %s completed.", day))
    print(sprintf("    MAPE = %s", MAPE_results[i]))
    print(sprintf("    RMSE = %s", RMSE_results[i]))
    i = i + 1
  }
  results = as.data.frame(list(cv_start:cv_end, unlist(MAPE_results), unlist(RMSE_results)))
  results = setNames(results, c('Day Train End','MAPE','RMSE'))
  return(results)
}
#Do cross val for VAR (predict will take a very long time if the start is low)
pos <- match("Night", colnames(power_train))
CV_results_add <- cross_val(powerTS[,-pos], 35, 48, type="both")
#Look at scores
mean(CV_results_add$MAPE)
mean(CV_results_add$RMSE)
max(CV_results_add$MAPE)
max(CV_results_add$RMSE)


#EXTRA - applied only to VAR "both"
#Impulse Response Function (give a shock to Temperature to see the behavior in Power
#var_both_impulse <- irf(var_both, impulse="Temperature", response="Power", n.ahead=96*2, boot=TRUE)
#plot(var_both_impulse)
#Variance decomposition
#var_both_fevd <- fevd(var_both, n.ahead=96*2)
#plot(var_both_fevd) #As time advances, the influence decreases