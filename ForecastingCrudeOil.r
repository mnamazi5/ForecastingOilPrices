data <- read.csv('Oil_Prices.csv')
library(ggplot2)
WTI_ts <- ts(data$WTI, frequency=12, start=c(1999,9))
Brent_ts <- ts(data$Brent, frequency=12, start=c(1999,9))


library(forecast)
hist(WTI_ts, breaks=20, freq=FALSE, col='blue')
lines(density(WTI_ts) , col='red', lwd=3)


monthplot(WTI_ts,col.base='red')
seasonplot(WTI_ts,col=1:20, year.labels=TRUE)
# We can determine a slight seasonal pattern. We see demand increases in the end 
#of the year months,however these are not strong enough to determine seasonality.


t0 = c(1999,9)
t1 = c(2018,12)
t2 = c(2019,7) 
WTI_train <- window(WTI_ts, start=t0, end=t1)
WTI_test <- window(WTI_ts, start=t2)

#Univariate Case
library(urca)
WTI_ADF_none <- ur.df(WTI_train, type="none", lags=12, selectlags = "AIC")
summary(WTI_ADF_none)

WTI_ADF_drift <- ur.df(WTI_train, type="drift", lags=12, selectlags = "AIC")
summary(WTI_ADF_drift)

WTI_ADF_trend <- ur.df(WTI_train, type="trend", lags=12, selectlags = "AIC")
summary(WTI_ADF_trend)
# We fail to reject null for all test. This means WTI is not a stationary process
# for all test.
# You cannot reject the no drift null. You do not need a drift coefficient.
# You do not need to include a drift nor trend in our data



DWTI <-diff(WTI_train)
DWTI_ADF_none <- ur.df(DWTI, type="none", lags=12, selectlags = "AIC")
summary(DWTI_ADF_none)
DWTI_ADF_drift <- ur.df(DWTI, type="drift", lags=12, selectlags = "AIC")
summary(DWTI_ADF_drift)
DWTI_ADF_trend <- ur.df(DWTI, type="trend", lags=12, selectlags = "AIC")
summary(DWTI_ADF_trend)
# The best version is the no drift and no trend. Based on this we say it
# is stationary integrated order 1.



WTIarimaA <- auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10,
seasonal=FALSE, stepwise=FALSE, ic='aic')
WTIarimaB <- auto.arima(DWTI, d=0, max.p=5, max.q=5, max.order=10,
seasonal=FALSE, stepwise=FALSE, ic='bic')

WTIarimaA
WTIarimaB


future= forecast(WTIarimaA, h = 20)
plot(future)


# Bivariate Case

library(vars)
Brent_train <- window(Brent_ts, start=t0, end=t1)
Brent_test <- window(Brent_ts, start=t2)
df_level <-data.frame(WTI_train, Brent_train) 


vecm <- ca.jo(df_level, type = 'eigen', ecdet = 'const', K=2,spec='transitory')
summary(vecm)
# The null r=0 is rejected so there is atleast 1 cointegration factor. The 
# second test is not rejected therefore there is only one cointegration factor. 



vecm.rls <- cajorls(vecm, r = 1)
error <- vecm.rls$rlm$model['ect1']
tsdisplay(error$ect1)
error_ADF <- ur.df(error$ect1, type="none", lags=12, selectlags = "AIC")
summary(error_ADF)
# The error correction term is stationary and is not a white noise process.



cajorls(vecm)
coef(summary(cajorls(vecm)$rlm))
# At 5% significance WTI responds most to our error correction term


var.model = vec2var(vecm)
H = 12
fc <- predict(var.model, n.ahead=H)


WTI_forecast <- ts(fc$fcst$WTI_train[1:H,1], frequency=12, start=t2)
Brent_forecast <- ts(fc$fcst$Brent_train[1:H,1], frequency=12, start=t2)


print('WTI error')
x = accuracy(WTI_forecast,WTI_test)
cat('RMSE:',x[1,'RMSE'], '\n')
cat('MAE:',x[1,'MAE'], '\n')

print('Brent error')
y = accuracy(Brent_forecast,Brent_test)
cat('RMSE:',y[1,'RMSE'], '\n')
cat('MAE:',y[1,'MAE'], '\n')

WTI_forecast
Brent_forecast

plot(fc)
