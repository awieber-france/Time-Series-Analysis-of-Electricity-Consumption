#Get directory for current ile
fileDir = rstudioapi::getActiveDocumentContext()$path
if (getwd() != fileDir){ setwd(dirname(fileDir)) }
source("Data_Preparation.R")

#----------------------------------------------------------------------------
#--------------------------------MULTIVARIATE--------------------------------
#----------------------------------------------------------------------------
#Set prediction steps
n_ahead_test <- length(power_test[,1])
n_ahead_validate <- n_ahead_test + length(power_validate[,1])

#Dynamic Sarima (with other variables)
fit_dynam_SARIMA1 <- Arima(power_train[,"Power"], xreg=power_train[,c('Temperature', 'Wednesday', 'Sunday')], order=c(1,0,0), season=c(0,1,0))
summary(fit_dynam_SARIMA1)
ggtsdisplay(fit_dynam_SARIMA1$residuals, main="SARIMA Power~Temp+Wed+Sun(1,0,0)(0,1,0)")
checkresiduals(fit_dynam_SARIMA1)
fcast_dynam_SARIMA1 = forecast(fit_dynam_SARIMA1, xreg=power_test[,c('Temperature', 'Wednesday', 'Sunday')])

fit_dynam_SARIMA1b <- Arima(power_train[,"Power"], xreg=power_train[,'Temperature'], order=c(1,0,0), season=c(0,1,0))
summary(fit_dynam_SARIMA1b)
ggtsdisplay(fit_dynam_SARIMA1b$residuals, main="SARIMA Power~Temp(1,0,0)(0,1,0)")
checkresiduals(fit_dynam_SARIMA1b)
fcast_dynam_SARIMA1b = forecast(fit_dynam_SARIMA1b, xreg=power_test[,'Temperature'])


fit_dynam_SARIMA2 <- Arima(power_train[,"Power"], xreg=power_train[,c('Temperature', 'Wednesday', 'Sunday')], order=c(1,0,0), seasonal=c(1,1,2))
summary(fit_dynam_SARIMA2)
ggtsdisplay(fit_dynam_SARIMA2$residuals, main="SARIMA Power~Temp+Wed+Sun(1,0,0)(1,1,2)")
checkresiduals(fit_dynam_SARIMA2)
fcast_dynam_SARIMA2 = forecast(fit_dynam_SARIMA2, xreg=power_test[,c('Temperature', 'Wednesday', 'Sunday')])

fit_dynam_SARIMA2b <- Arima(power_train[,"Power"], xreg=power_train[,'Temperature'], order=c(1,0,0), seasonal=c(1,1,2))
summary(fit_dynam_SARIMA2b)
ggtsdisplay(fit_dynam_SARIMA2b$residuals, main="SARIMA Power~Temp(1,0,0)(1,1,2)")
checkresiduals(fit_dynam_SARIMA2b)
fcast_dynam_SARIMA2b = forecast(fit_dynam_SARIMA2b, xreg=power_test[,'Temperature'])


#Plot
plot(power_test[,1], type="l", col="black", main="SARIMA Predictions", xlab="Time (days)", ylab = "Electricity Demand (kW)", ylim=c(0, 400))
lines(fcast_dynam_SARIMA1$mean, type="l", col="red")
lines(fcast_dynam_SARIMA2$mean, type="l", col="turquoise")
legend("bottomright", inset=.01, title="Model",
       c("Data","Power~Temp+Sat+Sun (1,0,0)(0,1,0)","Power~Temp+Sat+Sun (1,0,0)(1,1,2)"),
       fill=c("black", "red","turquoise"), horiz=FALSE)

#Model fitting scores
fit_dynam_SARIMA1$bic
fit_dynam_SARIMA1b$bic
fit_dynam_SARIMA2$bic
fit_dynam_SARIMA2b$bic

#Test Scores
MAPE(fcast_dynam_SARIMA1$mean, power_test[,1])
MAPE(fcast_dynam_SARIMA1b$mean, power_test[,1])
MAPE(fcast_dynam_SARIMA2$mean, power_test[,1])
MAPE(fcast_dynam_SARIMA2b$mean, power_test[,1])

rmse(fcast_dynam_SARIMA1$mean, power_test[,1])
rmse(fcast_dynam_SARIMA1b$mean, power_test[,1])
rmse(fcast_dynam_SARIMA2$mean, power_test[,1])
rmse(fcast_dynam_SARIMA2b$mean, power_test[,1])

#Validation
validate_dynam_SARIMA1 = forecast(fit_dynam_SARIMA1, xreg=power_test_validate[,c('Temperature', 'Wednesday', 'Sunday')])
validate_dynam_SARIMA1b = forecast(fit_dynam_SARIMA1b, xreg=power_test_validate[,'Temperature'])
validate_dynam_SARIMA2 = forecast(fit_dynam_SARIMA2, xreg=power_test_validate[,c('Temperature', 'Wednesday', 'Sunday')])
validate_dynam_SARIMA2b = forecast(fit_dynam_SARIMA2b, xreg=power_test_validate[,'Temperature'])

#Output (convert to time series then isolate the validation part of it)
write.csv2(isolate_validate(as.ts(validate_dynam_SARIMA1)), file="validate_dynam_SARIMA1.csv")
write.csv2(isolate_validate(as.ts(validate_dynam_SARIMA2)), file="validate_dynam_SARIMA2.csv")
#Output to Excel
write.xlsx(isolate_validate(as.ts(validate_dynam_SARIMA1)[,1]), file='Sarima_multivariate_results.xlsx', colNames = FALSE)

#Cross-validation
#The time series must have the predict variable in the first column
#cv_start and cv_end makes the range of days to use for the training end point
cross_val_func <- function(ts, cv_start=5, cv_end=48, order=c(1,0,0), seasonal=c(0,1,0), xreg_name_vec=NULL){
  MAPE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  RMSE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  i <- 1 #counter
  for (day in cv_start:cv_end){
    #Train-Test split
    split_TS <- TTP_split(ts, train_end=day)
    cv_train <- split_TS$train
    cv_test <- split_TS$test
    #Set prediction steps
    model <- Arima(cv_train[,1], order=order, seasonal=seasonal, xreg=cv_train[,xreg_name_vec])
    pred = forecast(model, xreg=cv_test[,xreg_name_vec])
    pred_mean = pred$mean
    #Calculate scores
    MAPE_results[i] = MAPE(pred_mean, cv_test[, 1])
    RMSE_results[i] = rmse(pred_mean, cv_test[, 1])
    #Print results during run
    print(sprintf("Day %s completed.", day))
    print(sprintf("    MAPE = %s", MAPE_results[i]))
    print(sprintf("    RMSE = %s", RMSE_results[i]))
    #Increment the counter
    i = i + 1
  }
  results = as.data.frame(list(cv_start:cv_end, unlist(MAPE_results), unlist(RMSE_results)))
  results = setNames(results, c('Day Train End','MAPE','RMSE'))
  return(results)
}
#Do cross val for additive
xreg_name_vec <- c('Temperature', 'Wednesday', 'Sunday')
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=45, order=c(1,0,0), seasonal=c(0,1,0), xreg_name_vec=xreg_name_vec)
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
#Write results to file
write.csv2(CV_results, file="cross_val_Sarima-100-110.csv")
