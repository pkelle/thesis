library(plotly)

#IMF World Economic Outlook Database (April 2018)
Cyprus.DebtGDP = ts(c(46.748, 48.828, 53.480, 54.985, 55.663, 55.978, 57.525, 61.010, 63.006, 64.687, 64.029, 59.030, 53.149, 44.134, 52.826, 55.803, 65.224, 79.175, 102.085, 107.475, 107.497, 107.146), start=1995)
Greece.DebtGDP = ts(c(22.526, 26.681, 29.310, 33.591, 40.061, 46.621, 47.142, 52.413, 57.069, 59.821, 73.155, 74.683, 79.968, 100.288, 98.296, 98.990, 101.335, 99.452, 97.425, 98.906, 104.935, 107.081, 104.863, 101.456, 102.870, 107.392, 103.574, 103.103, 109.416, 126.745, 146.250, 172.096, 159.586, 177.946, 180.212, 178.779, 183.453), start=1980)
Ireland.DebtGDP = ts(c(78.622, 69.922, 61.657, 51.496, 46.663, 36.081, 33.235, 30.560, 29.932, 28.217, 26.079, 23.610, 23.901, 42.392, 61.534, 86.075, 110.391, 119.731, 119.599, 104.691, 77.056, 72.899), start=1995)
Italy.DebtGDP = ts(c(92.970, 95.571, 98.786, 102.281, 109.719, 120.539, 127.070, 116.909, 116.341, 113.764, 110.808, 109.656, 105.106, 104.727, 101.923, 100.485, 100.089, 101.941, 102.557, 99.792, 102.405, 112.547, 115.413, 116.520, 123.359, 129.018, 131.784, 131.509, 132.039), start=1988)
Portugal.DebtGDP = ts(c(56.429, 59.899, 54.445, 53.634, 56.624, 58.305, 59.511, 55.184, 51.827, 51.049, 50.317, 53.416, 56.183, 58.653, 61.989, 67.392, 69.175, 68.439, 71.666, 83.609, 96.183, 111.390, 126.222, 129.040, 130.593, 128.770, 129.901), start=1990)
Spain.DebtGDP = ts(c(16.577, 20.017, 25.141, 30.378, 37.075, 42.059, 43.301, 43.139, 39.626, 41.033, 42.510, 43.086, 45.421, 56.160, 58.683, 63.384, 67.526, 66.206, 64.212, 62.459, 57.959, 54.163, 51.268, 47.640, 45.261, 42.284, 38.907, 35.510, 39.398, 52.704, 60.065, 69.460, 85.737, 95.451, 100.367, 99.439, 98.988), start=1980)

plot1 = plot_ly( x=time(Greece.DebtGDP), y = Greece.DebtGDP, type = 'scatter', mode = 'lines', name = "Greece", line = list(width=2))
plot1 = add_trace(plot1, x=time(Ireland.DebtGDP) ,y = Ireland.DebtGDP, name = 'Ireland',mode = 'lines', line = list(width=2))
plot1 = add_trace(plot1, x=time(Italy.DebtGDP) ,y = Italy.DebtGDP, name = 'Italy',mode = 'lines', line = list(width=2))
plot1 = add_trace(plot1, x=time(Portugal.DebtGDP) ,y = Portugal.DebtGDP, name = 'Portugal',mode = 'lines', line = list(width=2))
plot1 = add_trace(plot1, x=time(Spain.DebtGDP) ,y = Spain.DebtGDP, name = 'Spain',mode = 'lines', line = list(width=2))
plot1 = layout(plot1, font=list(size=14), yaxis = list(ticksuffix = "%", title="Debt-GDP Ratio"), xaxis=list(title="Year"))
plot1

# average debt growth for the first phase
mean(percentChange(Greece.DebtGDP)[1:13])

percentChange(Greece.DebtGDP)