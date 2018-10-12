library(plotly)

# OECD, "Main Economic Indicators - complete database", Main Economic Indicators (database),http://dx.doi.org/10.1787/data-00052-en 
# (Accessed on date) Copyright, 2016, OECD. Reprinted with permission.
Greece.BundSpread = ts(c(3.91, 1.82, 0.85, 0.51, 0.34, 0.20, 0.22, 0.23, 0.31, 0.28, 0.82, 1.95, 6.35, 13.14, 21.00, 8.48, 5.77, 9.17, 8.27, 5.66), start=1998)
Italy.BundSpread = ts(c(0.31, 0.24, 0.31, 0.39, 0.25, 0.23, 0.22, 0.20, 0.28, 0.27, 0.70, 1.09, 1.29, 2.81, 4.00, 2.75, 1.73, 1.22, 1.40, 1.80), start=1998)
Ireland.BundSpread = ts(c(0.18, 0.28, 0.22, 0.22, 0.21, 0.05, 0.03, -0.03, 0.03, 0.11, 0.57, 2.01, 3.25, 6.97, 4.50, 2.26, 1.10, 0.62, 0.60, 0.48), start=1998)
Spain.BundSpread = ts(c(0.26, 0.24, 0.26, 0.32, 0.18, 0.05, 0.07, 0.03, 0.02, 0.09, 0.38, 0.75, 1.51, 2.83, 4.35, 2.99, 1.56, 1.24, 1.30, 1.24), start=1998)
Portugal.BundSpread = ts(c(0.31, 0.29, 0.33, 0.36, 0.22, 0.11, 0.11, 0.08, 0.15, 0.21, 0.54, 0.99, 2.65, 7.63, 9.05, 4.72, 2.59, 1.93, 3.08, 2.74), start=1998)

#basispoints
Greece.BundSpread = Greece.BundSpread * 100
Italy.BundSpread = Italy.BundSpread * 100
Ireland.BundSpread = Ireland.BundSpread * 100
Spain.BundSpread = Spain.BundSpread * 100
Portugal.BundSpread = Portugal.BundSpread * 100

plot = plot_ly( x=time(Greece.BundSpread), y = Greece.BundSpread, type = 'scatter', mode = 'lines', name = "Greece", line = list(width=2))
plot = add_trace(plot, x=time(Ireland.BundSpread) ,y = Ireland.BundSpread, name = 'Ireland',mode = 'lines', line = list(width=2))
plot = add_trace(plot, x=time(Italy.BundSpread) ,y = Italy.BundSpread, name = 'Italy',mode = 'lines', line = list(width=2))
plot = add_trace(plot, x=time(Portugal.BundSpread) ,y = Portugal.BundSpread, name = 'Portugal',mode = 'lines', line = list(width=2))
plot = add_trace(plot, x=time(Spain.BundSpread) ,y = Spain.BundSpread, name = 'Spain',mode = 'lines', line = list(width=2))
plot = layout(plot, font=list(size=14), yaxis = list(title="Spread over 10 Year Bund (Bps)"),  
              xaxis = list(autotick=FALSE, dtick=2, title="Year"))
plot
