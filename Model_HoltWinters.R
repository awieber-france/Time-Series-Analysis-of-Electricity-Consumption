#Get directory for current ile
fileDir = rstudioapi::getActiveDocumentContext()$path
if (getwd() != fileDir){ setwd(dirname(fileDir)) }
source("Data_Preparation.R")

#----------------------------------------------------------------------------
#---------------------------------UNIVARIATE---------------------------------
#----------------------------------------------------------------------------
#Retrieve training data (power values only)
train <- power_train[,1]
test <- power_test[,1]
validate <- power_validate[,1]
#Set prediction steps
n_ahead_test <- length(test)
n_ahead_validate <- length(validate)
n_ahead_test_validate <- n_ahead_test + n_ahead_validate

###--- HOLT-WINTERS SMOOTHING ---#
#Predict with ADDITIVE
add_Smoothing_HW <- HoltWinters(train, alpha=NULL, beta=NULL, gamma=NULL, seasonal="additive")
p_add <- predict(add_Smoothing_HW, n.ahead = n_ahead_test)
#Predict with MULTIPLICATIVE
mult_Smoothing_HW <- HoltWinters(train, alpha=NULL, beta=NULL, gamma=NULL, seasonal="multiplicative")
p_mult <- predict(mult_Smoothing_HW, n.ahead = n_ahead_test)
#Plot
plot(test, type="l", col="black", main="Holt-Winters Predictions", xlab="Time (days)", ylab = "Electricity Demand (kW)", ylim=c(0, 400))
lines(p_add[,1], type="l", col="blue")
lines(p_mult[,1], type="l", col="red")
legend("bottomright", inset=.01, title="Model",
       c("Data","HW Additive","HW Multiplicative"),
       fill=c("black", "blue","red"), horiz=FALSE)

#Test Scores
MAPE(p_add[,1], test)
MAPE(p_mult[,1], test)
rmse(p_add[,1], test)
rmse(p_mult[,1], test)

#Validation (includes test portion)
validate_HW_Additive = predict(add_Smoothing_HW, n.ahead = n_ahead_test_validate)
validate_HW_Multiplicative = predict(mult_Smoothing_HW, n.ahead = n_ahead_test_validate)

#Output (isolate the validation part of the time series)
write.csv2(isolate_validate(validate_HW_Additive), file="validate_HW_Additive.csv")
write.csv2(isolate_validate(validate_HW_Multiplicative), file="validate_HW_Multiplicative.csv")
#Output to Excel
write.xlsx(isolate_validate(validate_HW_Multiplicative[,1]), file='HW_results.xlsx', colNames = FALSE)

#CROSS VALIDATION
#The time series must have the predict variable in the first column
#cv_start and cv_end makes the range of days to use for the training end point 
cross_val <- function(ts, cv_start=5, cv_end=48, season_type="multiplicative"){
  MAPE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  RMSE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  i <- 1 #counter
  for (day in cv_start:cv_end){
    split_TS <- TTP_split(ts, train_end=day)
    cv_train <- split_TS$train
    cv_test <- split_TS$test
    #Set prediction steps
    model <- HoltWinters(cv_train[,1], alpha=NULL, beta=NULL, gamma=NULL, seasonal=season_type)
    pred <- predict(model, n.ahead = length(cv_test[,1]))
    #Calculate scores
    MAPE_results[i] = MAPE(pred[,1], cv_test[, 1])
    RMSE_results[i] = rmse(pred[,1], cv_test[, 1])
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
#Do cross val for additive
CV_results_add <- cross_val(powerTS, 35, 48, "additive")
#Look at scores
mean(CV_results_add$MAPE)
mean(CV_results_add$RMSE)
max(CV_results_add$MAPE)
max(CV_results_add$RMSE)
plot(CV_results_add$MAPE)
plot(CV_results_add$RMSE)

#Do cross val for multiplicative
CV_results_mult <- cross_val(powerTS, 35, 48, "multiplicative")
mean(CV_results_mult$MAPE)
mean(CV_results_mult$RMSE)
max(CV_results_mult$MAPE)
max(CV_results_mult$RMSE)
plot(CV_results_mult$MAPE)
plot(CV_results_mult$RMSE)

#Write results to file
write.csv2(CV_results_mult, file="cross_val_HW_add.csv")
write.csv2(CV_results_mult, file="cross_val_HW_mult.csv")
