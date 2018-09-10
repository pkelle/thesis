library(MonteCarlo)
library(plotly)
library(scales)
library(tfplot)

i = 1 # mean interest rate
Y = 177735 #GDP in million € for 2017
Ygrowth2017 = .02
meanYgrowth = .035

years = c(2017:2030)

Ygrowth_vector = rnorm(13, mean=meanYgrowth, sd=.02)

#Ygrowth_vector = percent(Ygrowth_vector, accuracy=0.01 ,scale=1)

Y_vector= c(Y)

#calculate GDP up until to 2030
for(growth in Ygrowth_vector) {
 Y_vector=c(Y_vector, Y_vector[length(Y_vector)] * (1 + growth))
}

Ygrowth_vector=c(Ygrowth2017, Ygrowth_vector)


#plot GDP and growth
#ay <- list(
#  tickfont = list(color = "black"), overlaying = "y", side = "right", title = "GDP in million €"
#)
#p =  plot_ly(x = ~years, y = ~Ygrowth_vector, type = 'scatter', mode = 'lines+markers', name = "GDP growth %", line = list(width=0.5) )
#p = add_trace(p,  y = ~Y_vector,  yaxis = "y2", name ="GDP", line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash'))
#p = layout(p,title = "GPD prediction", yaxis2 = ay, xaxis = list(title="year"), yaxis=list(title="GDP growth in %"))
#p


calculateGrowth <- function(meanY) {

  Ygrowth_vector = rnorm(13, mean=meanY, sd=.02)
  Y_vector= c(Y)
  
  #calculate GDP up until to 2030
  for(growth in Ygrowth_vector) {
    Y_vector=c(Y_vector, Y_vector[length(Y_vector)] * (1 + growth))
  }
  
  Ygrowth_vector=c(Ygrowth2017, Ygrowth_vector)
  
  return(Y_vector)
}
  
datalist = list()
for(i in seq(10)) {
  
  datalist[[i]] = calculateGrowth(0.035)
}

dfYgrowth = do.call(rbind, datalist)

YgrowthFinal=colMeans(dfYgrowth)

min(dfYgrowth[,14])
max(dfYgrowth[,14])

p =  plot_ly(x = ~years, y = ~YgrowthFinal, type = 'scatter', mode = 'lines+markers', name = "GDP growth", line = list(width=0.5) )
p

Y_ts <- ts(YgrowthFinal, start = c(2017, 1), frequency = 1)
percentChange(Y_ts)


