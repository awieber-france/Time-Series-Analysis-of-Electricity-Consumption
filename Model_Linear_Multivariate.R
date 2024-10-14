#Get directory for current ile
fileDir = rstudioapi::getActiveDocumentContext()$path
if (getwd() != fileDir){ setwd(dirname(fileDir)) }
source("Data_Preparation.R")

#----------------------------------------------------------------------------
#--------------------------------MULTIVARIATE--------------------------------
#----------------------------------------------------------------------------
#Time Series Regression Models - simple (only temperature)
fit_linear_temp <- tslm(Power~Temperature+trend+season, data=power_train)
summary(fit_linear_temp)
fcast_linear_temp = forecast(fit_linear_temp, newdata = data.frame(power_test))
#Time Series Regression Models - Temperature + Wednesday + Sunday
fit_linear_all <- tslm(Power~Temperature+Wednesday+Sunday+trend+season, data=power_train)
summary(fit_linear_all)
fcast_linear_all = forecast(fit_linear_all, newdata = data.frame(power_test))
#Time Series Regression Models - Temperature + Wednesday
fit_linear_temp_wed <- tslm(Power~Temperature+Wednesday+trend+season, data=power_train)
summary(fit_linear_temp_wed)
fcast_linear_temp_wed = forecast(fit_linear_temp_wed, newdata = data.frame(power_test))
#Time Series Regression Models - Temperature + Sunday
fit_linear_all_temp_sun <- tslm(Power~Temperature+Sunday+trend+season, data=power_train)
summary(fit_linear_all_temp_sun)
fcast_linear_temp_sun = forecast(fit_linear_all_temp_sun, newdata = data.frame(power_test))

#Plot
plot(power_test[,1], type="l", col="black", main="Linear Model Predictions", xlab="Time (days)", ylab = "Electricity Demand (kW)", ylim=c(0, 400))
lines(fcast_linear_temp$mean, type="l", col="brown2")
lines(fcast_linear_all$mean, type="l", col="lightgreen")
lines(fcast_linear_temp_wed$mean, type="l", col="blue")
lines(fcast_linear_temp_sun$mean, type="l", col="orange")
legend("bottomright", inset=.01, title="Model",
       c("Data","Power~Temp+Trend+Season","Power~Temp+Wed+Sun+Trend+Season","Power~Temp+Wed+Trend+Season","Power~Temp+Sun+Trend+Season"),
       fill=c("black", "brown2","lightgreen","blue","orange"), horiz=FALSE)

#Test Scores
MAPE(fcast_linear_temp$mean, power_test[,1])
MAPE(fcast_linear_all$mean, power_test[,1])
MAPE(fcast_linear_temp_wed$mean, power_test[,1])
MAPE(fcast_linear_temp_sun$mean, power_test[,1])

rmse(fcast_linear_temp$mean, power_test[,1])
rmse(fcast_linear_all$mean, power_test[,1])
rmse(fcast_linear_temp_wed$mean, power_test[,1])
rmse(fcast_linear_temp_sun$mean, power_test[,1])

#Validation
validate_linear_temp = forecast(fit_linear_temp, newdata = data.frame(power_test_validate))
validate_linear_all = forecast(fit_linear_all, newdata = data.frame(power_test_validate))
validate_linear_all = forecast(fcast_linear_temp_wed, newdata = data.frame(power_test_validate))
validate_linear_all = forecast(fcast_linear_temp_sun, newdata = data.frame(power_test_validate))

#Output (convert to time series then isolate the validation part of it)
write.csv2(validate_linear_temp, file="validate_linear_temp.csv")
write.csv2(validate_linear_all, file="validate_linear_all.csv")
#Output to Excel
write.xlsx(isolate_validate(as.ts(validate_linear_all)[,1]), file='Linear_Mutivariate_results.xlsx', colNames = FALSE)

#Cross-validation
#The time series must have the predict variable in the first column
#cv_start and cv_end makes the range of days to use for the training end point 
cross_val_func <- function(ts, cv_start=5, cv_end=48, formula = "Power~trend+season"){
  MAPE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  RMSE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  i <- 1 #counter
  for (day in cv_start:cv_end){
    #Train-Test split
    split_TS <- TTP_split(ts, train_end=day)
    cv_train <- split_TS$train
    cv_test <- split_TS$test
    #Set prediction steps
    model <- tslm(eval(formula), data=cv_train)
    pred <- forecast(model, newdata = data.frame(cv_test))
    mean_pred <- pred$mean
    #Calculate scores
    MAPE_results[i] = MAPE(mean_pred, cv_test[, 1])
    RMSE_results[i] = rmse(mean_pred, cv_test[, 1])
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
#Do cross val for additive Temperature only
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, formula = "Power~Temperature+trend+season")
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)

#Do cross val for additive with Temperature, Wednesday, Sunday
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, formula = "Power~Temperature+Wednesday+Sunday+trend+season")
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)

#Do cross val for additive with Temperature, Wednesday
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, formula = "Power~Temperature+Wednesday+trend+season")
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)

#Do cross val for additive with Temperature, Sunday
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, formula = "Power~Temperature+Sunday+trend+season")
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)
