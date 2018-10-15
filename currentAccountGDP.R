library(plotly)

# IMF
Greece.CurrentAccountGDP = ts(c(-3.845, -4.547, -3.442, -3.786, -4.411, -6.834, -2.971, -1.862, -1.253, -3.232, -3.611, -1.490, -1.840, -0.685, -0.125, -2.347, -3.492, -3.720, -2.624, -3.579, -5.927, -5.369, -6.832, -8.450, -7.710, -8.870, -11.486, -15.188, -15.111, -12.345, -11.384, -10.006, -3.831, -2.041, -1.630, -0.229, -1.075, -0.818), start=1980)

plot = plot_ly( x=time(Greece.CurrentAccountGDP), y = Greece.CurrentAccountGDP, type = 'scatter', mode = 'lines', name = "overall", line = list(width=2))
plot = layout(plot, font=list(size=14), yaxis = list(title="Current Account Balance to GDP ratio", ticksuffix = "%"),  
              xaxis = list(autotick=FALSE, dtick=5, title="Year"))
plot


