library(plotly)
library(tfplot)

#AMECO
Ireland.DebtGDP = ts(c(66.8722, 72.1983, 81.1186, 90.6492, 94.6735, 97.4295, 108.3093, 109.3878, 105.2564, 96.18, 90.2282, 91.5564, 88.6531, 91.1368, 85.8636, 78.5431, 69.8517, 61.6004, 51.481, 46.6499, 36.0732, 33.2395, 30.5521, 29.9297, 28.2149, 26.0766, 23.6183, 23.9084, 42.4037, 61.5433, 85.9938, 110.8616, 119.8647, 119.6837, 104.1284, 76.8191, 73.4443, 68.4404), start=1980)
Greece.DebtGDP = ts(c(22.7496, 27.0308, 30.5069, 35.034, 41.7999, 48.8537, 50.7326, 57.0162, 62.3094, 65.5431, 72.4769, 74.8493, 79.9676, 100.2891, 98.296, 98.9904, 101.3358, 99.4515, 97.425, 98.9067, 104.9344, 107.0812, 104.8631, 101.4561, 102.8703, 107.3918, 103.574, 103.103, 109.4155, 126.7447, 146.2496, 172.0702, 159.56, 177.4096, 178.9075, 175.8595, 178.4886, 176.1278), start=1980)
Spain.DebtGDP = ts(c(16.0308, 19.6295, 24.4635, 29.5582, 35.4534, 40.3521, 41.6703, 41.9851, 38.4652, 39.8338, 41.5655, 42.3242, 44.7004, 55.725, 58.3329, 61.71, 65.5699, 64.4007, 62.5254, 60.9479, 57.9585, 54.1627, 51.268, 47.6401, 45.2611, 42.2838, 38.903, 35.5903, 39.4742, 52.7811, 60.1404, 69.5337, 85.7366, 95.4507, 100.3665, 99.3312, 98.97, 98.1228), start=1980)
Italy.DebtGDP = ts(c(53.9922, 56.279, 60.787, 66.8144, 72.1069, 77.8836, 81.9426, 85.79, 87.4459, 89.8312, 91.6706, 94.8721, 101.4817, 111.2535, 117.208, 116.9095, 116.3409, 113.7639, 110.8079, 109.6555, 105.1064, 104.7267, 101.9234, 100.4854, 100.0893, 101.9409, 102.5573, 99.792, 102.4048, 112.5472, 115.4129, 116.522, 123.3604, 129.02, 131.7848, 131.5551, 131.3558, 131.2196), start=1980)
Portugal.DebtGDP = ts(c(29.0957, 36.8342, 40.2858, 43.7592, 48.0412, 55.6604, 56.0136, 53.6209, 53.24, 51.8768, 52.4986, 54.8337, 49.2507, 53.7832, 56.4454, 58.3049, 59.5107, 55.1842, 51.8273, 51.049, 50.3171, 53.4164, 56.1823, 58.6529, 61.9893, 67.3922, 69.1748, 68.4391, 71.6663, 83.6095, 96.1833, 111.3897, 126.2224, 129.0396, 130.5996, 128.7547, 129.216, 124.7624), start=1980)
                                                            
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





