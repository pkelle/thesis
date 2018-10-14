library(plotly)

# IMF for before 1995 rest AMECO
Greece.GovDefictGDP = ts(c(-2.47, -6.98, -5.42, -6.09, -7.09, -9.43, -8.50, -7.98, -9.49, -11.73, -13.10, -9.49, -10.50, -11.29, -8.36, -9.73, -8.16, -6.06, -6.27, -5.79, -4.06, -5.47, -6.02, -7.83, -8.83, -6.19, -5.95, -6.71, -10.18, -15.14, -11.20, -10.28, -8.87, -13.15, -3.62, -5.67, 0.63, 0.82), start=1980)
Greece.PrimaryGovDefictGDP = ts(c(-4.32, -6.62, -6.03, -3.17, -2.56, -2.71, 1.14, 0.96, 2.10, 2.27, 1.43, 1.79, 2.79, 0.82, -0.46, -2.93, -4.04, -1.49, -1.53, -2.21, -5.36, -10.10, -5.34, -3.00, -3.77, -9.13, 0.36, -2.12, 3.85, 3.98), start=1988)

plot = plot_ly( x=time(Greece.GovDefictGDP), y = Greece.GovDefictGDP, type = 'scatter', mode = 'lines', name = "overall", line = list(width=2))
plot = add_trace(plot, x=time(Greece.PrimaryGovDefictGDP) ,y = Greece.PrimaryGovDefictGDP, name = 'primary',mode = 'lines', line = list(width=2))
plot = layout(plot, font=list(size=14), yaxis = list(title="Government Balance to GDP Ratio", ticksuffix = "%"),  
              xaxis = list(autotick=FALSE, dtick=5, title="Year"))
plot


