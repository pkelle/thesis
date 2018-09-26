library(MonteCarlo)
library(plotly)
library(scales)
library(tfplot)
library(MASS)
library(fitdistrplus)

year = c(1995:2017)
years = c(2018:2030)
# GDP deflator based on 2010
GDP_deflator = c(58.613, 63.088, 67.223, 70.654, 73.214, 74.379, 76.964, 79.542, 82.288, 84.809, 86.709, 89.740, 92.811, 96.843, 99.332, 100.000, 100.798, 100.425, 98.063, 96.266, 95.279, 94.368, 95.000)

GDP_deflator = ts(GDP_deflator, start=1995, frequency = 1)
perc = percentChange(GDP_deflator)

fit = fitdistr(as.numeric(perc), "normal")

hist(perc, prob = TRUE)
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col = 2, add = TRUE)
shapiro.test(as.numeric(perc))

datalist = list()
for(i in seq(1000)) {
  GDP_deflator_vector = c(95.000) # GDP deflator 2017
  
  for(growth in rnorm(13, fit$estimate[1], fit$estimate[2])) {
    GDP_deflator_vector=c(GDP_deflator_vector, GDP_deflator_vector[length(GDP_deflator_vector)] * (1 + growth/100))
  }
  
  datalist[[i]] = GDP_deflator_vector[-1] # remove GDP deflator value for 2017
}

predictions = do.call(rbind, datalist)
Forecast.GDP_deflator = colMeans(predictions)
percentChange(ts(Forecast.GDP_deflator, start=2017, frequency = 1))

plow = apply(predictions, 2, function(x) quantile(x, 0.25))
phigh = apply(predictions, 2, function(x) quantile(x, 0.75))

p = plot_ly(x = ~years, y = ~Forecast.GDP_deflator, type = 'scatter', mode = 'lines+markers', name = "GDP growth", line = list(width=0.5))
p = add_trace(p, y = ~plow, name = 'plow', mode = 'lines')
p = add_trace(p, y = ~phigh, name = 'phigh',mode = 'lines')
p = add_trace(p,x = time(GDP_deflator), y = ~GDP_deflator, name = 'GDP_deflator',mode = 'lines+markers', line = list(dash="dot", width=0.5))
p



