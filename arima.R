library(plotly)
library(tfplot)
library(forecast)
library(tseries)
library(dynlm)
library(vars)


year=c(1995:2017)
# GDP constant price 2010
Y = c(158777,	163322,	170645,	177292,	182738,	189901,	197747,	205504,	217412,	228415,	229782,	242771,	250718,	249878,	239132,	226031,	205389,	190395,	184223,	185586,	185047,	184595,	187089)

unemployment = c(10.000,	10.300,	10.300,	11.200,	12.125,	11.350,	10.775,	10.350,	9.775,	10.600,	10.000,	9.000,	8.400,	7.750,	9.600,	12.725,	17.850,	24.425,	27.475,	26.500,	24.900,	23.550,	21.450)
investment = c(22.472,	23.354,	22.441,	25.176,	24.149,	25.826,	25.692,	24.750,	27.375,	25.307,	22.099,	26.152,	27.131,	24.511,	18.338,	17.048,	15.105,	12.803,	11.601,	11.911,	9.819,	10.610,	11.728)


acf(Y)
#pacf(Y)

#ARIMA
fit = auto.arima(Y, trace = TRUE)
summary(fit)
tsdisplay(residuals(fit), main='Model Residuals')
fcast <- forecast(fit, h=14)
plot(fcast)
percentChange(fcast[['mean']])
checkresiduals(fit)


#AR
aar=ar(Y, aic = TRUE, method="burg")
summary(aar)
fcastaar <- forecast(aar, h=14)
plot(fcastaar)
percentChange(fcastaar[['mean']])


#VAR
varmat <- as.ts(as.matrix(cbind(Y, investment)))
#VARselect(varmat, lag.max=9, type="const")
varfit <- VAR(varmat, p = 2,type="const") 
summary(varfit)
fcast3 <- forecast(varfit, h=14)
plot(fcast3)
percentChange(fcast3[["forecast"]][["Y"]][["mean"]])
