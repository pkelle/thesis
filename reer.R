library(plotly)

# AMECO, reversing order, Real effective exchange rates, based on unit labour costs (total economy) Performance relative to the rest of the former EU-15
# 2010 = 100
Greece.reer15 = ts(rev(c(81.7, 81.1, 80.9, 83.3, 85.8, 93, 97.9, 100, 99.1, 95.7, 92.4, 91.4, 93.1, 86.3, 85.8, 84, 78.5, 79.2, 82.6, 80.5, 81.1, 76.6, 75.5, 71.2, 68.6, 66.2, 66.9, 71.7, 71, 66.1, 62, 63.7, 77, 77.7, 77.4, 78.2, 69.6, 61.1)), start=1980)



plot = plot_ly( x=time(Greece.reer15), y = Greece.reer15, type = 'scatter', mode = 'lines', name = "overall", line = list(width=2))

plot = layout(plot, font=list(size=14), yaxis = list(title="Reer15"),  
              xaxis = list(autotick=FALSE, dtick=5, title="Year"))
plot


