library(plotly)
library(tfplot)
library(forecast)
library(tseries)

years = c(2018:2030)
Forecast.nominalGDP = Forecast.ConstGDP.Predictions[,] * (Forecast.GDP_deflator.Predictions[,]/100)


p1 <- plot_ly()

for(i in seq(nrow(Forecast.nominalGDP))) {
  p1 = add_trace(p1, y = Forecast.nominalGDP[i,], name = i, mode = 'lines',  type = 'scatter')
}

p1





p2  <- plot_ly()
p2 = add_ribbons(p2, x=years,
            ymin = apply(Forecast.nominalGDP, 2, function(x) quantile(x, 0.025)),
            ymax = apply(Forecast.nominalGDP, 2, function(x) quantile(x, 0.975)),
            line = list(color = 'rgba(7, 164, 181, 0.2)'),
            fillcolor = 'rgba(7, 164, 181, 0.2)',
            name = "95%")
p2 = add_ribbons(p2, x=years,
                 ymin = apply(Forecast.nominalGDP, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(Forecast.nominalGDP, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "90%")
p2