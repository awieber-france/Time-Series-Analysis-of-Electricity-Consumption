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

###--- SARIMA ---###
#Predict with SARIMA (auto) using default hyperparameter limits
auto_SARIMA_simple <- auto.arima(train, xreg=NULL)
ggtsdisplay(auto_SARIMA_simple$residuals, main="SARIMA Univariate (1,0,0)(0,1,0)")
summary(auto_SARIMA_simple) #BIC=29096.07
checkresiduals(auto_SARIMA_simple)
p_auto_sarima_simple <- predict(auto_SARIMA_simple, n.ahead = n_ahead_test)
#Predict with SARIMA (auto) using higher hyperparameter limits... GIVES SAME RESULT! COMMENTED OUT
#auto_SARIMA_complex <- auto.arima(train, xreg=NULL, max.p=96, max.q=10, max.d=96, max.P=7, max.D=7)
#p_auto_sarima_complex <- predict(auto_SARIMA_complex, n.ahead = n_ahead_test)
#summary(auto_SARIMA_complex)
#Predict with SARIMA (manual), starting from the auto version
SARIMA_1 <- Arima(train, order=c(1,0,0), seasonal=c(1,1,0))
ggtsdisplay(SARIMA_1$residuals, main="SARIMA Univariate (1,0,0)(1,1,0)")
summary(SARIMA_1) #BIC=28046.77
checkresiduals(SARIMA_1)
p_sarima_1<- predict(SARIMA_1, n.ahead=n_ahead_test)

SARIMA_2 <- Arima(train, order=c(1,0,0), seasonal=c(1,1,2))
ggtsdisplay(SARIMA_2$residuals, main="SARIMA Univariate (1,0,0)(1,1,2)")
summary(SARIMA_2) #BIC=27019.74
checkresiduals(SARIMA_2)
p_sarima_2<- predict(SARIMA_2, n.ahead=n_ahead_test)

SARIMA_3 <- Arima(train, order=c(1,0,4), seasonal=c(1,1,2))
ggtsdisplay(SARIMA_3$residuals, main="SARIMA Univariate (1,0,4)(1,1,2)")
summary(SARIMA_3)
checkresiduals(SARIMA_3)
p_sarima_3<- predict(SARIMA_3, n.ahead=n_ahead_test)

#Plot
plot(test, type="l", col="black", main = "SARIMA Predictions", xlab="Time (days)", ylab = "Electricity Demand (kW)", ylim=c(0, 400))
lines(p_auto_sarima_simple$pred, type="l", col="purple")
lines(p_sarima_1$pred, type="l", col="orange")
lines(p_sarima_2$pred, type="l", col="green")
lines(p_sarima_3$pred, type="l", col="cyan2")
legend("bottomright", inset=.01, title="Model",
       c("Data","(1,0,0)(0,1,0)","(1,0,0)(1,1,0)","(1,0,0)(1,1,2)","(1,0,4)(1,1,2)"),
       fill=c("black", "purple","orange","green","cyan"), horiz=FALSE)

#Model fitting scores
auto_SARIMA_simple$bic
SARIMA_1$bic
SARIMA_2$bic
SARIMA_3$bic

#Test Scores
MAPE(p_auto_sarima_simple$pred, test)
MAPE(p_sarima_1$pred, test)
MAPE(p_sarima_2$pred, test)
MAPE(p_sarima_3$pred, test)

rmse(p_auto_sarima_simple$pred, test)
rmse(p_sarima_1$pred, test)
rmse(p_sarima_2$pred, test)
rmse(p_sarima_3$pred, test)

#Validation (includes test portion)
validate_auto_sarima_simple = forecast(auto_SARIMA_simple, h=n_ahead_test_validate)
validate_sarima_1 = forecast(SARIMA_1, h=n_ahead_test_validate)
validate_sarima_2 = forecast(SARIMA_2, h=n_ahead_test_validate)
validate_sarima_3 = forecast(SARIMA_3, h=n_ahead_test_validate)
#Output (convert to time series then isolate the validation part of it)
write.csv2(isolate_validate(as.ts(validate_auto_sarima_simple)), file="validate_auto_sarima_simple.csv")
write.csv2(isolate_validate(as.ts(validate_sarima_1)), file="validate_sarima_100_110.csv")
write.csv2(isolate_validate(as.ts(validate_sarima_2)), file="validate_sarima_100_112.csv")
write.csv2(isolate_validate(as.ts(validate_sarima_3)), file="validate_sarima_104_112.csv")
#Output to Excel
write.xlsx(isolate_validate(as.ts(validate_sarima_1)[,1]), file='Sarima_results.xlsx', colNames = FALSE)

#Cross-validation
#The time series must have the predict variable in the first column
#cv_start and cv_end makes the range of days to use for the training end point 
cross_val_func <- function(ts, cv_start=5, cv_end=48, order=c(1,0,0), seasonal=c(1,1,0)){
  MAPE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  RMSE_results <- vector(mode='list', length = cv_end - cv_start + 1)
  i <- 1 #counter
  for (day in cv_start:cv_end){
    #Train-Test split
    split_TS <- TTP_split(ts, train_end=day)
    cv_train <- split_TS$train
    cv_test <- split_TS$test
    #Set prediction steps
    model <- Arima(cv_train[,1], order=order, seasonal=seasonal)
    pred <- predict(model, n.ahead = length(cv_test[,1]))
    #Calculate scores
    MAPE_results[i] = MAPE(pred$pred, cv_test[, 1])
    RMSE_results[i] = rmse(pred$pred, cv_test[, 1])
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
#Do cross val for additive 100-110
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, order=c(1,0,0), seasonal=c(0,1,0))
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)
#Write results to file
write.csv2(CV_results, file="cross_val_Sarima-100-010.csv")

#Do cross val for additive 100-110
CV_results <- cross_val_func(powerTS, cv_start=35, cv_end=48, order=c(1,0,0), seasonal=c(1,1,0))
#Look at scores
mean(CV_results$MAPE)
mean(CV_results$RMSE)
max(CV_results$MAPE)
max(CV_results$RMSE)
#Write results to file
write.csv2(CV_results, file="cross_val_Sarima-100-110.csv")
