# ForecastingCrudeOil
The following repository is a rudimentary forecast of oil prices. The objective of the project was to cultivate time-series techniques I've learned in my forecasting course while displaying my command in R. Topics covered in this project include ARIMA forecasting, Augmented-Dickey Fuller Test, Johannsen test, and VECM.

The project begins with the univariate case by predicting WTI with an ARIMA model. Our next forecast takes into consideration the bivariate case by incorporating Brent Crude prices. Cointegration factors are found using the Johannsen Procedure. VECM is utilized to forecast both Brent and WTI. 

Code alongside output can be found in ForecastingCrudeOil.ipynb. If the former fails to load please view ForecastingCrudeOil.R. Thank you. 
