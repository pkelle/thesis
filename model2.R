library(plotly)
library(scales)
library(tfplot)
library(MASS)
library(fitdistrplus)

year = c(1995:2017)
years = c(2018:2030)

SAMPLES = 2000

### REAL GDP ###
# GDP constant price, 2010 based, in million Euro
Y = c(158777,	163322,	170645,	177292,	182738,	189901,	197747,	205504,	217412,	228415,	229782,	242771,	250718,	249878,	239132,	226031,	205389,	190395,	184223,	185586,	185047,	184595,	187089)

Y = ts(Y, start=1995, frequency=1)
Y_perc = percentChange(Y)


datalist = list()

for(i in seq(SAMPLES)) {
  start = sample(seq(length(Y_perc)-5),1,replace=T)
  perc_vec = Y_perc[start:(start+4)]
  
  start = sample(seq(length(Y_perc)-5),1,replace=T)
  perc_vec = c(perc_vec, Y_perc[start:(start+4)])
  
  start = sample(seq(length(Y_perc)-3),1,replace=T)
  perc_vec = c(perc_vec, Y_perc[start:(start+2)])
  
  Y_vec = c(187089) # 2017 GDP
  
  for (perc_change in perc_vec) {
    Y_vec=c(Y_vec, Y_vec[length(Y_vec)] * (1 + perc_change/100))
  }
  
  datalist[[i]] = Y_vec[-1] # remove GDP value for 2017
}

Forecast.ConstGDP = do.call(rbind, datalist)

#Plotting
#p1 <- plot_ly()
#for(i in seq(nrow(Forecast.ConstGDP))) {
#  p1 = add_trace(p1, y = Forecast.ConstGDP[i,], name = i, mode = 'lines',  type = 'scatter')
#}
#p1 = layout(p1, showlegend=F)
#p1


### GDP DEFLATOR ###

# GDP deflator based on 2010
GDP_deflator = c(58.613, 63.088, 67.223, 70.654, 73.214, 74.379, 76.964, 79.542, 82.288, 84.809, 86.709, 89.740, 92.811, 96.843, 99.332, 100.000, 100.798, 100.425, 98.063, 96.266, 95.279, 94.368, 95.000)

GDP_deflator = ts(GDP_deflator, start=1995, frequency=1)
GDP_deflator_perc = percentChange(GDP_deflator)

datalist = list()

for(i in seq(SAMPLES)) {
  start = sample(seq(length(GDP_deflator_perc)-5),1,replace=T)
  perc_vec = GDP_deflator_perc[start:(start+4)]
  
  start = sample(seq(length(GDP_deflator_perc)-5),1,replace=T)
  perc_vec = c(perc_vec, GDP_deflator_perc[start:(start+4)])
  
  start = sample(seq(length(GDP_deflator_perc)-3),1,replace=T)
  perc_vec = c(perc_vec, GDP_deflator_perc[start:(start+2)])
  
  GDP_deflator_vec = c(95.000) # GDP deflator 2017
  
  for (perc_change in perc_vec) {
    GDP_deflator_vec=c(GDP_deflator_vec, GDP_deflator_vec[length(GDP_deflator_vec)] * (1 + perc_change/100))
  }
  
  datalist[[i]] = GDP_deflator_vec[-1] # remove GDP value for 2017
}

Forecast.GDP_deflator = do.call(rbind, datalist)


### FORECAST ###

#in million euro, source elstat "Greek Economy"
Govdebt2017 = 317407 # add gov surplus in 2017
interestpayments = 5626
avginterestrate = interestpayments/Govdebt2017

# in percent, see bruegel DSA for why constant high primary balance not possible
# EU assumption
yearly_primary_balance_of_GDP_percentage = ts(c(3.5, 3.5, 3.5, 3.5, 3.5, 3.0, 3.0, 3.0, 2.2, 2.2, 2.2, 2.2, 2.2), start=2018, frequency=1)
# IMF assumption
#yearly_primary_balance_of_GDP_percentage = ts(c(2.2, 3.5, 3.5, 3.5, 3.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5), start=2018, frequency=1)


Forecast.Debt = c()

Forecast.nominalGDP = Forecast.ConstGDP[,] * (Forecast.GDP_deflator[,]/100)

for(i in seq(nrow(Forecast.nominalGDP))) {
  #reset for each forecast calculation
  Forecast.primary_balance = Forecast.nominalGDP[i,] * yearly_primary_balance_of_GDP_percentage/100  
  debt_vec = c(Govdebt2017)
  Forecast.Balance = c()
  
  for (j in seq(ncol(Forecast.nominalGDP))) {
    #interest payments on last years total debt
    #fixed interest rate
    interestpayments = avginterestrate * debt_vec[j]
    
    #forecast overall balance
    Forecast.Balance = c(Forecast.Balance, Forecast.primary_balance[j] - interestpayments)      
    
    #repay debt when surplus, take up debt when deficit
    debt_vec = c(debt_vec, debt_vec[j] - Forecast.Balance[j])
  }
  
  #remove initial debt for 2017
  debt_vec = debt_vec[-1]
  
  Forecast.Debt = rbind(Forecast.Debt, debt_vec)
}

Final.GDPDebtRatio = Forecast.Debt/Forecast.nominalGDP

default_prob_vec = c()
for(i in seq(ncol(Final.GDPDebtRatio))) {
  default_prob_vec = c(default_prob_vec, sum(Final.GDPDebtRatio[,i]>0.9)/SAMPLES)
}
default_prob_vec

sum(Final.GDPDebtRatio[,13]<=.9)/SAMPLES


#Plotting
#p1 <- plot_ly()
#for(i in seq(nrow(Final.GDPDebtRatio))) {
#  p1 = add_trace(p1, y = Final.GDPDebtRatio[i,], name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
#}
#p1 = layout(p1, showlegend=F)
#p1



p2  <- plot_ly()
p2 = add_ribbons(p2, x = years,
                 ymin = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.025)),
                 ymax = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.975)),
                 line = list(color = 'rgba(7, 164, 181, 0.2)'),
                 fillcolor = 'rgba(7, 164, 181, 0.2)',
                 name = "95%")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "90%")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.25)), 
                 ymax = apply(Final.GDPDebtRatio, 2, function(x) quantile(x, 0.75)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.6)',
                 name = "50%")
p2
