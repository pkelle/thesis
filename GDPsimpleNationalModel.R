library(MonteCarlo)
library(plotly)
library(scales)
library(tfplot)
library(MASS)
library(fitdistrplus)

year=c(1995:2017)
years = c(2017:2030)
# GDP constant price 2010
Y = c(158777,	163322,	170645,	177292,	182738,	189901,	197747,	205504,	217412,	228415,	229782,	242771,	250718,	249878,	239132,	226031,	205389,	190395,	184223,	185586,	185047,	184595,	187089)

Y = ts(Y, start=1995, frequency = 1)
perc = percentChange(Y)

fit = fitdistr(as.numeric(perc), "normal")

hist(perc, prob = TRUE)
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col = 2, add = TRUE)
shapiro.test(as.numeric(perc))

datalist = list()
for(i in seq(1000)) {
  Y_vector = c(187089)
  
  for(growth in rnorm(13, fit$estimate[1], fit$estimate[2])) {
    Y_vector=c(Y_vector, Y_vector[length(Y_vector)] * (1 + growth/100))
  }
  
  datalist[[i]] = Y_vector
}

gdppredicts = do.call(rbind, datalist)
YgrowthFinal = colMeans(gdppredicts)


plow = apply(gdppredicts, 2, function(x) quantile(x, 0.25))
phigh = apply(gdppredicts, 2, function(x) quantile(x, 0.75))


p = plot_ly(x = ~years, y = ~YgrowthFinal, type = 'scatter', mode = 'lines+markers', name = "GDP growth", line = list(width=0.5) )
p = add_trace(p, y = ~plow, name = 'plow',mode = 'lines')
p = add_trace(p, y = ~phigh, name = 'phigh',mode = 'lines')
p




