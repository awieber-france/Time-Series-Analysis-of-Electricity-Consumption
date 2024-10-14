# Time-Series-Analysis-of-Electricity-Consumption

## Introduction
Time series forecasting is typically performed using a variety of statistical approaches, but may also be approached with linear, random forest, and neural network models. This paper looks at several statistical approaches and a linear system for the modelling of a specific dataset. This dataset contains electricity consumption and outdoor air temperature for a single building from 1/1/2010 1:15 to 2/20/2010 23:45. Extra temperature data is available for 2/21/2010 and constitutes the validation period for the model. The time zone is unknown, but is irrelevant for this study.

Both univariate and multivariate models are examined. Holt-Winters seasonal smoothing and SARIMA are used for the univariate modelling. SARIMA and a linear model are used for the multivariate case, with temperature being the supplied supplementary variable. The data does not suggest that a VAR model is appropriate, but it is analyzed for comparative purposes.

## Files
These are the required files that should be stored in the same directory:
* 2023-11-Elec-train.xlsx : Contains all electricity and outside temperature data for the building.
* Data_Preparation.R : Imports and cleans the data. Extrapolation and feature creation is performed here. The model files automatically call this file and use the output data.
* Model_HoltWinters.R : The univariate Holt-Winters model.
* Model_Linear_Multivariate.R: The multivaraite linear model.
* Model_SARIMA.R : The univariate SARIMA model
* Model_SARIMA_Multivariate.R : The multivariate SARIMA model
* Model_VAR_Multivariate.R: The multivariate VAR model.

## Methods
An analysis of the data shows that the initial data contains 11 contiguous missing values on 18/02/2010 (day 49). A linear interpolation is used since it appears to be a good approximation (Figure 1a) . These values are used for both the modelling and the scoring. When used in the scoring it ensures that there is a penalization for a high deviation from the expected values.

Extra features are generated from the time data: Night, Holiday, and Weekdays. These are evaluated with analysis of variance to determine their relevance.
The data is transformed into a time series in the following manner:
* Day 0 is the first day (1/1/2010)
* Each day is composed of 15 minute blocks, giving 64 total points per day
* Each day starts at 00:15, with midnight as the last point of the day
* The series starts at point 5 of day 0 (1:15am)
* The complete data ends on day 50 at 11:45pm
* The validation set begins at midnight of day 50 and continues to the end of the temperature
data (all data from day 51)
* The season length is 1 day (64 points)

Once transformed into a time series, the distribution of the data is analyzed. The temperature data is essentially gaussian, however the power data is bimodal (Figure 1b). This is due to the lack of sufficient transitional values between high and low periods of the day. A Box-Cox transformation is not required for the temperature and is not adapted to the bimodal distribution of the power. As such, the data is left unmodified.

![Alt text](/imgs/Figures_1a_1b.jpg?raw=true)

&emsp;&emsp;&emsp;**Figure 1a: interpolation (in red) of missing data**&emsp;&emsp;&emsp;&emsp;&emsp;**Figure 1b: histogram of power & temperature**

Cross validation is used to rate the different models with the exception of SARIMA, which suffers from very long computation times. The final model is generated with the training data set to the period from day 0 to the end of day 44, leaving 6 days for the test set.

For the univariate approach, neither a linear model nor a VAR model are appropriate. A linear model does not perform well in general without extra correlated variables besides time, while a VAR model requires an extra variable by definition. Thus only the two typically best performing models are used: Holt-Winters and SARIMA, despite the bimodal power data violating the Gaussian assumptions of the latter. The auto-regression, moving average, and difference orders are quite high for SARIMA due to the 64-point period.

Holt-Winters is excluded from the multivariate approach due to its inability to handle additional variables. VAR is unsuitable to non-stationary data (temperature) and cannot take external temperature predictions to improve its performance. By design, it predicts temperature in parallel. Thus, VAR serves primarily as a comparative benchmark. A linear model and SARIMA are selected for the multivariate approach. Again, SARIMA is chosen despite the bimodal power data violating the Gaussian assumptions of SARIMA.

No SARIMA model (univariate or multivariate) manages to achieve stationarity. The p-values of the Box-Ljung test are all very small, meaning that the null hypothesis is rejected. The null hypothesis for this test states that the residuals show no autocorrelation over a certain number of lags. The models make acceptable predictions despite this fact. Testing of a heavier univariate SARIMA(1,0,4)(1,1,2) model shows better stationarity with little improvement in performance, but with a high computational cost. The multivariate version would have a very high computational cost. The search for stationarity is thus halted early and results are accepted as good enough.

The machine used to run the code was a MacBook Pro with a 4-core 2,9 GHz Intel Core i7 and 16GB of RAM running MacOS 13.6.3. It operated in a hot summer environment. The code execution times cited in this report are specific to this machine and operating conditions.

## Code
The models are coded in the R language and are supplied in a single R script in the appendix. It requires a number of libraries that must be installed prior to running the code (see the top of the code). Since the SARIMA models are computationally intensive, a toggle for fast modelling is included. This toggles simple SARIMA models that allow the code to be quickly tested and analyzed, at the cost of getting inferior results. The code is structured in the following manner:
* Selection of fast_mode (true/false)
* Automatic finding of code directory (note that all files must be in the same directory)
* Importation and correction of the data (interpolation of NAs)
* Feature creation (Night, Holiday, Weekdays)
* Transformation into a time series
* Base functions
* Data examination and splitting into train-test-validate
* General Model Parameters (n_ahead values)
* Holt-Winters
* SARIMA univariate
* Linear model (univariate)
* SARIMA multivariate
* Vectorial Auto-Regressive model (VAR)
* Comparison of models & exportation of results
* Cross validation, with default values for SARIMA set to (1,0,0)(1,1,0) to reduce computation

## Results
Analysis of variance reveals that Temperature, Night, Wednesday, and Sunday are statistically relevant variables. The VAR model shows that the Night variable, however, is already implicitly included in the time data, since a noninvertible matrix is created when it is included. Night is effectively a linear combination of other variables and is thus excluded from all models. In the end, Wednesday and Sunday do not often improve the models in any meaningful way. These variables are thus sometimes dropped from the final selection of model parameters. The best model parameters are:
* Holt-Winters multiplicative
  * Seasonal model = multiplicative (additive has worse performance)
  * ⍺ = 0.6543097, β = 0, and 𝛄 = 0.6293974 (selected automatically by the model)
  * runtime < 1 minute
* SARIMA(1,0,0)(1,1,2) univariate
  * pdq and PDQ values determined by manual, step-wise selection
  * runtime ≈ 50 minutes
* Linear model
  * Power ~ Temperature + Wednesday + Trend + Season (best combination selected)
  * runtime < 1 minute
* SARIMA(1,0,0)(1,1,2) multivariate
  * pdq and PDQ values determined by manual, step-wise selection
  * Temperature covariate (additional covariates make the model too complex)
  * runtime ≈ 90 minutes
* VAR
  * Lag order = 5 (determined via an algorithm)
  * Deterministic regressors: constant + trend (best performing)
  * Power, Temperature, Wednesday and Sunday selected (no optimization)

A plot of the predictions is shown in Figure 2. Holt-Winters over-predicts the peak values when they are lower than usual, while the other models under-predict the high values. All models show a decent performance overall.

![Alt text](/imgs/Figure2.jpg?raw=true)

&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;**Figure 2: predictions of electricity demand over the test period**

A look at the scores of the different models (Table 1) confirms a good overall performance, clearly with some slight difference between models. The scores for cross validation of SARIMA models is not available. This complicates the choice of best model. A cross validation of simpler SARIMA models is performed in the code, which confirms that it is at least stable.

### Table 1: Univariate and Multivariate Scores (MAPE & RMSE)


|                   | Holt-Winters | SARIMA (1,0,0)(1,1,2) | Linear                         | SARIMA (1,0,0)(1,1,2) | VAR (constant + trend) |
| :--               | :-:          | :-:                   | :-:                            | :-:                   | :-:                    |
|                   |      -       |       -               | Power~Temperature+Wednesday    |   Power~Temperature   | Power~Temperature+Wednesday+Sunday   |
|                   |              |                       |                                |                       |                                      |
|MAPE               |              |                       |                                |                       |                                      |
|  Test Set         |     4,92     |      4,76             |        4,64                    |       4,81            |        4,83                          |
|  Cross Validation |     5,05     |        -              |        4,90                    |         -             |        4,90                          |
|RMSE               |              |                       |                                |                       |                                      |
|  Test Set         |    15,02     |      14,06            |       14,20                    |       13,80           |        15,15                         |
|  Cross Validation |    15,59     |        -              |       15,63                    |         -             |        15,61                         |

## Conclusion
The best models are selected based on their prediction scores and their run times. In the univariate case, SARIMA(1,0,0)(1,1,2) is selected. In the Multivariate case, the linear model is selected. No particular model stands out as significantly better. As such, computation time is a significant factor in the selection of these models, with computations over 1 hour being penalized.

VAR performed better than expected. It could be improved even further by making the data stationary before modelling it. It will, however, always have the weakness of needing to predict the temperature along with power, instead of taking an outside temperature prediction and using it to improve the power prediction.

Validation results are supplied in a separate Excel file. The first column is the univariate SARIMA model. The second column is the multivariate linear model.
