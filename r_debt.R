library(plotly)
library(scales)
library(tfplot)
library(MASS)
library(fitdistrplus)

#graph axis caption
years = c(2018:2030)


#new debt interest rates
#forecasted Greek 10yr bond yields, interest rate for new debt, now 4.3%, forward rate calculated in 2030 5.53%
GreekNewDebtInterestRates=seq(0.043, 0.0529, length.out=13)

#old debt interest rates
#in million euro, source ELSTAT "The Greek Economy"
Govdebt2016 = 315009 
Govdebt2017 = 317407 
interestpayments2017 = 5626
avginterestrate_2017 = interestpayments2017/Govdebt2016
# EC prediction for 2030 for EFSF loans 3.1% 
GreekOldDebtInterestRates = seq(avginterestrate_2017, 0.031, length.out=14)
GreekOldDebtInterestRates = GreekOldDebtInterestRates[-1]

pdebt <- plot_ly()
pdebt = add_trace(pdebt, x=years, y = GreekOldDebtInterestRates*100, name = 'r_old', mode = 'lines',  type = 'scatter')
pdebt = add_trace(pdebt, x=years, y = GreekNewDebtInterestRates*100, name = 'r_new', mode = 'lines',  type = 'scatter')
pdebt = layout(pdebt, font=list(size=14), yaxis = list(title="Interest Rates on Greek Debt", ticksuffix="%"),  
               xaxis = list(autotick=FALSE, dtick=2, title="Year"))
pdebt