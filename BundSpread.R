library(plotly)

Greece.BundSpread = ts(c(3.90, 1.82, 0.84, 0.51, 0.34, 0.20, 0.22, 0.23, 0.31, 0.28, 0.82, 1.95, 6.35, 13.14, 21.00, 8.48, 5.77, 9.17, 8.27, 5.66), start=1998)
Italy.BundSpread = ts(c(1.20, 0.30, 0.24, 0.31, 0.39, 0.25, 0.22, 0.22, 0.20, 0.28, 0.27, 0.70, 1.09, 1.29, 2.81, 4.00, 2.75, 1.73, 1.22, 1.40, 1.80), start=1998)
Ireland.BundSpread = ts(c(), start=1998)
Spain.BundSpread = ts(c(), start=1998)
Portugal.BundSpread = ts(c(), start=1998)

plot = plot_ly( x=time(Greece.DebtGDP), y = Greece.DebtGDP, type = 'scatter', mode = 'lines', name = "Greece", line = list(width=2))
plot = add_trace(plot, x=time(Cyprus.DebtGDP) ,y = Cyprus.DebtGDP, name = 'Cyprus',mode = 'lines', line = list(width=1))
plot = add_trace(plot, x=time(Ireland.DebtGDP) ,y = Ireland.DebtGDP, name = 'Ireland',mode = 'lines', line = list(width=1))
plot = add_trace(plot, x=time(Italy.DebtGDP) ,y = Italy.DebtGDP, name = 'Italy',mode = 'lines', line = list(width=1))
plot = add_trace(plot, x=time(Portugal.DebtGDP) ,y = Portugal.DebtGDP, name = 'Portugal',mode = 'lines', line = list(width=1))
plot = add_trace(plot, x=time(Spain.DebtGDP) ,y = Spain.DebtGDP, name = 'Spain',mode = 'lines', line = list(width=1))
plot = layout(plot, yaxis = list(ticksuffix = "%"))
plot
