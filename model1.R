library(plotly)
library(tfplot)
library(forecast)
library(tseries)


#forecasting period
years = c(2018:2030)

#in million euro, source elstat "Greek Economy"
Govdebt2017 = 317407 # add gov surplus in 2017
interestpayments = 5626
avginterestrate = interestpayments/Govdebt2017

# in percent, see bruegel DSA for why constant high primary balance not possible
yearly_primary_balance_of_GDP_percentage = ts(c(3.5, 3.5, 3.3, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1, 3.1), start=2018, frequency=1)


Forecast.Debt = c(Govdebt2017)
Forecast.Balance = c()
Final.Forecast.debt = c()

Forecast.nominalGDP = Forecast.ConstGDP.Predictions[,] * (Forecast.GDP_deflator.Predictions[,]/100)

for(i in seq(nrow(Forecast.nominalGDP))) {
  #reset for each forecast calculation
  Forecast.primary_balance = Forecast.nominalGDP[i,] * yearly_primary_balance_of_GDP_percentage/100  
  Forecast.Debt = c(Govdebt2017)
  Forecast.Balance = c()
  
  for (j in seq(ncol(Forecast.nominalGDP))) {
    #interest payments on last years total debt
    #fixed interest rate
    interestpayments = avginterestrate * Forecast.Debt[j]
    
    #forecast overall balance
    Forecast.Balance = c(Forecast.Balance, Forecast.primary_balance[j] - interestpayments)      
  
    #repay debt when surplus, take up debt when deficit
    Forecast.Debt = c(Forecast.Debt, Forecast.Debt[j] - Forecast.Balance[j])
  }
  
  #remove initial debt for 2017
  Forecast.Debt = Forecast.Debt[-1]
  
  Final.Forecast.debt = rbind(Final.Forecast.debt, Forecast.Debt)
}

Final.GDPDebtRatio = Final.Forecast.debt/Forecast.nominalGDP


#Plotting
p1 <- plot_ly()

for(i in seq(nrow(Final.GDPDebtRatio))) {
  p1 = add_trace(p1, y = Final.GDPDebtRatio[i,], name = i, mode = 'lines',  type = 'scatter')
}

p1


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


p3  <- plot_ly()
p3 = add_ribbons(p3, x = years,
                 ymin = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.025)),
                 ymax = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.975)),
                 line = list(color = 'rgba(7, 164, 181, 0.2)'),
                 fillcolor = 'rgba(7, 164, 181, 0.2)',
                 name = "95%")
p3 = add_ribbons(p3, x = years,
                 ymin = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "90%")
p3 = add_ribbons(p3, x = years,
                 ymin = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.25)), 
                 ymax = apply(Final.Forecast.debt, 2, function(x) quantile(x, 0.75)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.6)',
                 name = "50%")
p3

