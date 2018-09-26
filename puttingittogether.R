library(plotly)
library(tfplot)
library(forecast)
library(tseries)

#sourcing
source('C:/thesis/data.R', echo=TRUE)

Forecast.nominalGDP = Forecast.ConstGDP * Forecast.GDP_deflator/100

Forecast.primary_balance = Forecast.nominalGDP * yearly_primary_balance_of_GDP_percentage/100

Forecast.Debt = c()
Forecast.Balance = c()

interest = interest_payments_2017
Forecast.Debt = c(total_debt) 

for (i in seq(13)) {
  
  Forecast.Balance = c(Forecast.Balance, Forecast.primary_balance[i] - interest)
  
  newdebt = yearly_total_debt_repayments[i] - Forecast.Balance[i] 
  
  #interest calculation for next year
  interest = interest - yearly_total_debt_repayments[i] * 0.025 
  interest = interest + newdebt*0.3*0.01 + newdebt*0.7*0.04 # 30% short term rest long term new debt
  
  if (i+1 <= length(yearly_total_debt_repayments)) {
  yearly_total_debt_repayments[i+1] = yearly_total_debt_repayments[i+1] + newdebt*0.3 # add short term debt maturing the following year
  }
  
  if (i+5 <= length(yearly_total_debt_repayments)) {
    yearly_total_debt_repayments[i+5] = yearly_total_debt_repayments[i+5] + newdebt*0.7 # add long term debt maturing in 5 year
  }
  
  Forecast.Debt = c(Forecast.Debt, Forecast.Debt[i] - yearly_total_debt_repayments[i] + newdebt)
  
}

Forecast.Debt = Forecast.Debt[-1] #remove initial value from 2017


Forecast.Debt/ Forecast.nominalGDP 


