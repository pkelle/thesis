library(plotly)
library(scales)
library(tfplot)
library(MASS)
library(fitdistrplus)

avggrowth = 0
avggrowthde = 0
#graph axis caption
years = c(2018:2030)

#amount of simulations
SAMPLES = 2000 

# GDP constant price, 2010 based, source IMF WEO (https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/index.aspx)a
# in million Euro
RealGDP_1980_2017 = c(139374, 137205, 135655, 134192, 136885, 140325, 141047, 137864, 143776, 149238, 149238, 153867, 154940, 152462, 155511, 158777,	163322,	170645,	177292,	182738,	189901,	197747,	205504,	217412,	228415,	229782,	242771,	250718,	249878,	239132,	226031,	205389,	190395,	184223,	185586,	185047,	184595,	187089)

# GDP deflator with base year 2010, source IMF WEO (https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/index.aspx) 
#GDP_deflator_1980_2017 = c(5.101, 6.203, 7.891, 9.518, 11.605, 13.812, 16.421, 18.925, 22.081, 25.283, 30.514, 36.552, 41.962, 8.016, 53.386, 58.613, 63.088, 67.223, 70.654, 73.214, 74.379, 76.964, 79.542, 82.288, 84.809, 86.709, 89.740, 92.811, 96.843, 99.332, 100.000, 100.798, 100.425, 98.063, 96.266, 95.279, 94.368, 95.000)
GDP_deflator_2001_2017 = c(76.964, 79.542, 82.288, 84.809, 86.709, 89.740, 92.811, 96.843, 99.332, 100.000, 100.798, 100.425, 98.063, 96.266, 95.279, 94.368, 95.000)
# http://www.pdma.gr/attachments/article/37/Maturity%20Profile%20Central%20Government%20Debt%20Table_30-06-2018.pdf
# http://www.pdma.gr/attachments/article/1935/Greece-2019%20Financing%20Strategy.pdf
# debt repayment schedule, excluding repos and TBills
# in million Euro
yearly_matured_debt= ts(c(3168, 11844, 5066, 5102, 9825, 12183, 7684, 9057, 6728, 6709, 12146, 5597, 6254), start=2018)

#forecasted Greek 10yr bond yields, interest rate for new debt, now 4.3%, forward rate calculated in 2030 5.53%
GreekNewDebtInterestRates=c(0, seq(0.043, 0.0553, length.out=13))

### REAL GDP ###
RealGDP_1980_2017 = ts(RealGDP_1980_2017, start=1980, frequency=1) # create time series
RealGDPpercent_1981_2017 = percentChange(RealGDP_1980_2017) # create time series of percentage change 
RealGDPpercent_1981_2017 = 1 + RealGDPpercent_1981_2017/100 # change to have a vector of proper growth multipicators

### GDP DEFLATOR ###
GDP_deflator_2001_2017 = ts(GDP_deflator_2001_2017, start=1980, frequency=1)
GDPdeflatorPercent_2002_2017 = percentChange(GDP_deflator_2001_2017)
GDPdeflatorPercent_2002_2017 = 1 + GDPdeflatorPercent_2002_2017/100

temp_datalist_realGDP = list()
temp_datalist_GDPdeflator = list()

for(i in seq(SAMPLES)) {
  start = sample(seq(length(RealGDPpercent_1981_2017)), 1, replace=T) # determine start year 
  ForecastRealGDPgrowth = RealGDPpercent_1981_2017[start] # first year real GDP
   # first year GDP deflator 
   
  start = sample(seq(length(RealGDPpercent_1981_2017)-3), 1, replace=T) # determine start year 
  ForecastRealGDPgrowth = c(ForecastRealGDPgrowth, RealGDPpercent_1981_2017[start:(start+3)]) # next 4 years real GDP
  
  start = sample(seq(length(RealGDPpercent_1981_2017)-3), 1, replace=T) # determine start year
  ForecastRealGDPgrowth = c(ForecastRealGDPgrowth, RealGDPpercent_1981_2017[start:(start+3)]) 
  
  start = sample(seq(length(RealGDPpercent_1981_2017)-3), 1, replace=T) # determine start year
  ForecastRealGDPgrowth = c(ForecastRealGDPgrowth, RealGDPpercent_1981_2017[start:(start+3)])
  
  #DEFLATOR
  start = sample(seq(length(GDPdeflatorPercent_2002_2017)), 1, replace=T)
  ForecastGDPdeflatorgrowth = GDPdeflatorPercent_2002_2017[start]
  
  start = sample(seq(length(GDPdeflatorPercent_2002_2017)-3), 1, replace=T) 
  ForecastGDPdeflatorgrowth = c(ForecastGDPdeflatorgrowth, GDPdeflatorPercent_2002_2017[start:(start+3)])
  
  start = sample(seq(length(GDPdeflatorPercent_2002_2017)-3), 1, replace=T) 
  ForecastGDPdeflatorgrowth = c(ForecastGDPdeflatorgrowth, GDPdeflatorPercent_2002_2017[start:(start+3)])
  
  start = sample(seq(length(GDPdeflatorPercent_2002_2017)-3), 1, replace=T) 
  ForecastGDPdeflatorgrowth = c(ForecastGDPdeflatorgrowth, GDPdeflatorPercent_2002_2017[start:(start+3)])
  
  ForecastRealGDP = c(187089) # 2017 real GDP
  
  # loop through all 13 elements of the ForecastRealGDPgrowth vector and multiply them 
  # with the real GDP of the previous year
  for (temp_perc_change in ForecastRealGDPgrowth) {  
    ForecastRealGDP = c(ForecastRealGDP, ForecastRealGDP[length(ForecastRealGDP)] * temp_perc_change)
  }
  
  #
  #
  #
  avggrowth = (avggrowth + sum(ForecastRealGDPgrowth)/length(ForecastRealGDPgrowth))/2
  avggrowthde = (avggrowthde + sum(ForecastGDPdeflatorgrowth)/length(ForecastGDPdeflatorgrowth))/2
  
  # remove GDP value for 2017 and insert vector in a temporary dataframe
  temp_datalist_realGDP[[i]] = ForecastRealGDP[-1] 
  
  ForecastGDPdeflator = c(95.00) # GDP deflator 2017
  
  for (temp_perc_change in ForecastGDPdeflatorgrowth) {
    ForecastGDPdeflator=c(ForecastGDPdeflator, ForecastGDPdeflator[length(ForecastGDPdeflator)] * temp_perc_change)
  }
  
  # remove GDP deflator value for 2017 and insert vector in a temporary dataframe
  temp_datalist_GDPdeflator[[i]] = ForecastGDPdeflator[-1] 
}

#bind rows to form matrix
ForecastRealGDPmatrix = do.call(rbind, temp_datalist_realGDP)
ForecastGDPdeflatormatrix = do.call(rbind, temp_datalist_GDPdeflator)

### FORECAST ###

#in million euro, source ELSTAT "The Greek Economy"
Govdebt2016 = 315009 
Govdebt2017 = 317407 
interestpayments2017 = 5626
avginterestrate = interestpayments2017/Govdebt2016
# EC prediction for 2030 for EFSF loans 3.1%
GreekOldDebtInterestRates = seq(avginterestrate, 0.031, length.out=13)

# EU assumption, in percent
PrimaryBalancePercGDP_2018_2030 = ts(c(3.5, 3.5, 3.5, 3.5, 3.5, 3.0, 2.5, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2), start=2018, frequency=1)
# IMF assumption, in percent
#PrimaryBalancePercGDP_2018_2030 = ts(c(2.2, 3.5, 3.5, 3.5, 3.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5), start=2018, frequency=1)

ForecastDebtmatrix = c()
ForecastGDPmatrix = ForecastRealGDPmatrix * (ForecastGDPdeflatormatrix/100)

for(i in seq(nrow(ForecastGDPmatrix))) {
  #reset for each forecast calculation
  temp_PrimaryBalance = ForecastGDPmatrix[i,] * PrimaryBalancePercGDP_2018_2030/100  
  temp_debt = c(Govdebt2017)
  temp_GovBalance = c()
  temp_yearly_newdebt = c(0)
  
  for (j in seq(ncol(ForecastGDPmatrix))) {
    #interest payments on last years total debt
    #fixed interest rate
    interestpayments = GreekOldDebtInterestRates[j] * (temp_debt[j]-sum(temp_yearly_newdebt)) + sum((GreekNewDebtInterestRates[1:j]*temp_yearly_newdebt[1:j]))
    
    #forecast overall balance
    temp_GovBalance = c(temp_GovBalance, temp_PrimaryBalance[j] - interestpayments)
    
    #repay debt when surplus, take up debt when deficit
    temp_debt = c(temp_debt, temp_debt[j] - temp_GovBalance[j])
    
    temp_yearly_newdebt = c(temp_yearly_newdebt, max((yearly_matured_debt[j] - temp_GovBalance[j]),0))
  }
  
  ForecastDebtmatrix = rbind(ForecastDebtmatrix, temp_debt[-1])
}

GDPDebtRatiomatrix = ForecastDebtmatrix/ForecastGDPmatrix

### probability to achieve a debt GDP ratio <= 90% ###
DebtGDP_90_prob = c()

# check for each year all simulated values and count the amount of which fall within <= 0.9
# divide by amount of simulations run to determine percentage
for(i in seq(ncol(GDPDebtRatiomatrix))) {
  DebtGDP_90_prob = c(DebtGDP_90_prob, sum(GDPDebtRatiomatrix[,i]<=0.9)/SAMPLES)
}

DebtGDP_90_prob


### Dispersion ###

# for each year calculate the difference between the 0.95 and 0.05 quantile of all simulations 
temp_GDPDebtdifference = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.95)) - apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.05))
temp_GDPDebtdifference

DebtGDP_dispersion_percentiles = c()

# now for each of the yearly differences determine the quantile that difference represents 
# of all simulations of that respective year 
for (y in seq(13)) {
  f <- ecdf(GDPDebtRatiomatrix[,y])
  DebtGDP_dispersion_percentiles = c(DebtGDP_dispersion_percentiles, f(temp_GDPDebtdifference[y]))
}

# see "Debt sustainability analysis for euro area sovereigns: a methodological framework"
# percentile < 33, green
# 33 < percentile >= 66th, yellow
# 66 < percentile), red
DebtGDP_dispersion_percentiles


### Plotting ###

#Plot all simulated GDP
# p1 <- plot_ly()
# for(i in seq(nrow(GDPDebtRatiomatrix))) {
# p1 = add_trace(p1, y = GDPDebtRatiomatrix[i,], name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
# }
# p1 = layout(p1, showlegend=F)
# p1

#Plot total debt
# p1 <- plot_ly()
# for(i in seq(nrow(ForecastDebtmatrix))) {
#  p1 = add_trace(p1, y = ForecastDebtmatrix[i,], name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
# }
# p1 = layout(p1, showlegend=F)
# p1

p1 <- plot_ly()
for(i in seq(nrow(ForecastRealGDPmatrix))) {
  p1 = add_trace(p1, y = ForecastRealGDPmatrix[i,], name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
}
p1 = layout(p1, showlegend=F)
p1


# plot prediction intervals
p2  <- plot_ly()
p2 = add_ribbons(p2, x = years,
                 ymin = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.025)),
                 ymax = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.975)),
                 line = list(color = 'rgba(7, 164, 181, 0.2)'),
                 fillcolor = 'rgba(7, 164, 181, 0.2)',
                 name = "95%")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "90%")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.25)), 
                 ymax = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.75)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.6)',
                 name = "50%")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.45)), 
                 ymax = apply(GDPDebtRatiomatrix, 2, function(x) quantile(x, 0.55)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.8)',
                 name = "10%")
p2 = add_trace(p2, x = years,y = temp_GDPDebtdifference, name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
p2

