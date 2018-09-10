library(plotly)
library(tfplot)
library(forecast)
library(tseries)


year=c(1995:2017)
Y = c(93064, 103037, 114712, 125263, 133789,	141247, 152194,	163461, 178905, 193716, 199242, 217862, 232695,	241990, 237534, 226031, 207029, 191204, 180654, 178656, 176312, 174199, 177735)


acf(Y)
#pacf(Y)


fit = auto.arima(Y,seasonal = FALSE, trace = TRUE)

summary(fit)

tsdisplay(residuals(fit), main='Model Residuals')


fcast <- forecast(fit, h=14)
plot(fcast)

ts=fcast[['mean']]
percentChange(ts)

checkresiduals(fit)
