library(plotly)
library(tfplot)

# IMF, in bil. euro
Greece.InterstPaymentsAbsolut = ts(c(1.643, 1.929, 3.220, 3.559, 5.162, 6.285, 7.889, 9.953, 10.569, 9.555, 9.644, 10.144, 9.679, 9.566, 9.103, 8.761, 9.269, 9.353, 9.623, 10.469, 11.653, 11.972, 13.239, 15.076, 9.744, 7.275, 7.096, 6.322, 5.651), start = 1988)
Greece.DebtAbsolut = ts(c(18.118, 22.571, 33.314, 42.002, 51.992, 73.418, 81.606, 92.124, 104.413, 114.083, 122.037, 132.326, 148.217, 162.971, 171.410, 181.510, 199.276, 213.970, 225.648, 239.915, 264.775, 301.062, 330.570, 356.289, 305.135, 321.466, 321.959, 315.209, 319.573), start= 1988)

plot = plot_ly( x=time(Greece.DebtAbsolut), y = Greece.DebtAbsolut, type = 'scatter', mode = 'lines', name = "Gross Government Debt", line = list(width=2))
plot = add_trace(plot, x=time(Greece.InterstPaymentsAbsolut) ,y = Greece.InterstPaymentsAbsolut, name = 'Interst Payments',mode = 'lines', line = list(width=2), yaxis = "y2")
plot = layout(plot, font=list(size=14), yaxis = list(title="Gross Government Debt (in billion Euro)", range=c(0,400)),  
              xaxis = list(autotick=F, dtick=2, title="Year"), yaxis2 = list(overlaying = "y", side = "right", title = "Interst Payments (in billion Euro)", autotick=T, dtick=2, rangemode = "tozero", range=c(0,20)),
              legend = list(x = 1, y=0.98))
plot

percentChange(Greece.DebtAbsolut)