library(MonteCarlo)
library(plotly)
library(scales)

i = 1 # mean interest rate
Y = 194.6 #GDP in Bil. $
meanYgrowth = .035

years = c(2017:2030)


Ygrowth_vector = rnorm(14, mean=meanYgrowth, sd=.02)

#Ygrowth_vector = percent(Ygrowth_vector, accuracy=0.01 ,scale=1)
Ygrowth_vector

Y_vector= c(Y)

for(growth in Ygrowth_vector) {
 Y_vector=c(Y_vector, Y_vector[length(Y_vector)] * (1 + growth))
}

Y_vector=Y_vector[2:length(Y_vector)]

print(Y_vector)  


plotdata = data.frame(years, Ygrowth_vector)

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)

#p = plot_ly()
#p = add_lines(p,x = ~1:3, y = ~10*(1:3), name = "slope of 10") 
#p = add_lines(p,x = ~2:4, y = ~1:3, name = "slope of 1", yaxis = "y2")
#p = layout(p,   title = "Double Y Axis", yaxis2 = ay,xaxis = list(title="x") )


p =  plot_ly( x = ~years, y = ~Ygrowth_vector, type = 'scatter', mode = 'lines+markers')
p = add_trace(p, x=~years, y = ~Y_vector,  yaxis = "y2", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))
p = layout(p,title = "Double Y Axis", yaxis2 = ay, xaxis = list(title="x"))

p


