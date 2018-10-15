library(plotly)

# IMF, CPI, avg, % change
Greece.CPIinflation = ts(c(24.668, 24.353, 21.420, 19.859, 18.419, 19.531, 23.113, 16.361, 13.480, 13.706, 20.347, 19.498, 15.869, 14.352, 10.881, 8.818, 7.873, 5.441, 4.517, 2.141, 2.897, 3.642, 3.921, 3.450, 3.020, 3.486, 3.314, 2.989, 4.230, 1.347, 4.704, 3.118, 1.035, -0.854, -1.394, -1.094, 0.013, 1.138), start=1980)



plot = plot_ly( x=time(Greece.CPIinflation), y = Greece.CPIinflation, type = 'scatter', mode = 'lines', name = "overall", line = list(width=2))

plot = layout(plot, font=list(size=14), yaxis = list(title="Inflation YoY change", ticksuffix = "%"),  
              xaxis = list(autotick=FALSE, dtick=5, title="Year"))
plot


