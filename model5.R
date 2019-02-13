#library(plotly)
#library(scales)
#library(tfplot)
#library(MASS)
#library(fitdistrplus)

if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}

if(!require(scales)){
  install.packages("scales")
  library(scales)
}

if(!require(tfplot)){
  install.packages("tfplot")
  library(tfplot)
}

avggrowth = 0
avggrowthde = 0
#graph axis caption
years = c(2018:2030)

#amount of simulations
SAMPLES = 2000 

### Sample Data ###
# GDP constant price, 2010 based, source IMF WEO (https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/index.aspx)
# 1980-2017,in million Euro
realGDPsamples = c(139374, 137205, 135655, 134192, 136885, 140325, 141047, 137864, 143776, 149238, 149238, 153867, 154940, 152462, 155511, 158777,	163322,	170645,	177292,	182738,	189901,	197747,	205504,	217412,	228415,	229782,	242771,	250718,	249878,	239132,	226031,	205389,	190395,	184223,	185586,	185047,	184595,	187089)

# GDP deflator, 2001-2017, with base year 2010, source IMF WEO (https://www.imf.org/external/pubs/ft/weo/2018/01/weodata/index.aspx) 
GDPDeflatorSamples = c(76.964, 79.542, 82.288, 84.809, 86.709, 89.740, 92.811, 96.843, 99.332, 100.000, 100.798, 100.425, 98.063, 96.266, 95.279, 94.368, 95.000)

### Debt Maturity Profile ###

# http://www.pdma.gr/attachments/article/37/Maturity%20Profile%20Central%20Government%20Debt%20Table_30-06-2018.pdf
# http://www.pdma.gr/attachments/article/1935/Greece-2019%20Financing%20Strategy.pdf
# debt repayment schedule, excluding repos and TBills
# in million Euro
DebtMaturityProfile = ts(c(3168, 11844, 5066, 5102, 9825, 12183, 7684, 9057, 6728, 6709, 12146, 5597, 6254), start=2018)

### Interest Rates ###

#new debt interest rates
#forecasted Greek 10yr bond yields, interest rate for new debt, now 4.3%, forward rate calculated in 2030 5.53%
IRnew=seq(0.043, 0.0529, length.out=13)

#old debt interest rates
#in million euro, source ELSTAT "The Greek Economy"
Govdebt2016 = 315009 
Govdebt2017 = 317407 
interestpayments2017 = 5626
avginterestrate_2017 = interestpayments2017/Govdebt2016
# EC prediction for 2030 for EFSF loans 3.1% 
IRold = seq(avginterestrate_2017, 0.031, length.out=14)
IRold = IRold[-1]

### Primary Balance ###

# EU assumption, in percent, 2018-2030
PrimaryBalancePercGDP = ts(c(3.5, 3.5, 3.5, 3.5, 3.5, 3.0, 2.5, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2), start=2018, frequency=1)
# IMF assumption, in percent, 2018-2030
#PrimaryBalancePercGDP = ts(c(2.2, 3.5, 3.5, 3.5, 3.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5), start=2018, frequency=1)


### REAL GDP ###

realGDPsamples = ts(realGDPsamples, start=1980, frequency=1) # create time series
realGDPpercent = percentChange(realGDPsamples) # create time series of percentage change 
realGDPpercent = 1 + realGDPpercent/100 # change to have a vector of proper growth multipicators

### GDP DEFLATOR ###

GDPDeflatorSamples = ts(GDPDeflatorSamples, start=1980, frequency=1)
GDPdefPerc = percentChange(GDPDeflatorSamples)
GDPdefPerc = 1 + GDPdefPerc/100

temp_datalist_realGDP = list()
temp_datalist_GDPdeflator = list()

#Bootstrap real GDP and Deflator
for(i in seq(SAMPLES)) {
  #real GDP
  start = sample(seq(length(realGDPpercent)), 1, replace=T) # determine start year 
  FCrealGDPpercent = realGDPpercent[start] #  year 1 real GDP
   
  start = sample(seq(length(realGDPpercent)-3), 1, replace=T) # determine start year 
  FCrealGDPpercent = c(FCrealGDPpercent, realGDPpercent[start:(start+3)]) # years 2-5 real GDP
  
  start = sample(seq(length(realGDPpercent)-3), 1, replace=T) # determine start year
  FCrealGDPpercent = c(FCrealGDPpercent, realGDPpercent[start:(start+3)]) # years 5-9 real GDP
  
  start = sample(seq(length(realGDPpercent)-3), 1, replace=T) # determine start year
  FCrealGDPpercent = c(FCrealGDPpercent, realGDPpercent[start:(start+3)]) # years 9-13 real GDP
  
  #DEFLATOR
  start = sample(seq(length(GDPdefPerc)), 1, replace=T)
  FcGDPdefPerc = GDPdefPerc[start]
  
  start = sample(seq(length(GDPdefPerc)-3), 1, replace=T) 
  FcGDPdefPerc = c(FcGDPdefPerc, GDPdefPerc[start:(start+3)])
  
  start = sample(seq(length(GDPdefPerc)-3), 1, replace=T) 
  FcGDPdefPerc = c(FcGDPdefPerc, GDPdefPerc[start:(start+3)])
  
  start = sample(seq(length(GDPdefPerc)-3), 1, replace=T) 
  FcGDPdefPerc = c(FcGDPdefPerc, GDPdefPerc[start:(start+3)])
  
  FCrealGDP = c(187089) # 2017 real GDP
  
  # loop through all 13 elements of the FCrealGDPpercent vector and multiply them 
  # with the real GDP of the previous year
  for (temp_perc_change in FCrealGDPpercent) {  
    FCrealGDP = c(FCrealGDP, FCrealGDP[length(FCrealGDP)] * temp_perc_change)
  }
  
  #
  avggrowth = (avggrowth + sum(FCrealGDPpercent)/length(FCrealGDPpercent))/2
  avggrowthde = (avggrowthde + sum(FcGDPdefPerc)/length(FcGDPdefPerc))/2
  #
  
  # remove GDP value for 2017 and insert vector in a temporary dataframe
  temp_datalist_realGDP[[i]] = FCrealGDP[-1] 
  
  FcGDPdef = c(95.00) # GDP deflator 2017
  
  for (temp_perc_change in FcGDPdefPerc) {
    FcGDPdef=c(FcGDPdef, FcGDPdef[length(FcGDPdef)] * temp_perc_change)
  }
  
  # remove GDP deflator value for 2017 and insert vector in a temporary dataframe
  temp_datalist_GDPdeflator[[i]] = FcGDPdef[-1] 
}

#bind rows to form matrix
FcRealGDPmatrix = do.call(rbind, temp_datalist_realGDP)
FcGDPdeflatormatrix = do.call(rbind, temp_datalist_GDPdeflator)

### FORECAST ###

FctDebtmatrix = c()
FcGDPmatrix = FcRealGDPmatrix * (FcGDPdeflatormatrix/100)

for(i in seq(nrow(FcGDPmatrix))) {
  #reset for each forecast calculation
  temp_PrimaryBalance = FcGDPmatrix[i,] * PrimaryBalancePercGDP/100  
  TotalDebt = c(Govdebt2017)
  GovBalance = c()
  NewDebt = c()
  
  for (j in seq(ncol(FcGDPmatrix))) {
    #in the first year no interest payments yet on new debt 
    if (i==1) {
      #interest payments on last years total debt
      interestpayments = IRold[j] * TotalDebt[j]
    }
    else {
      interestpayments = IRold[j] * (TotalDebt[j]-sum(NewDebt)) + sum((IRnew[1:j-1]*NewDebt[1:j-1]))  
    }
    
    #forecast overall balance
    GovBalance = c(GovBalance, temp_PrimaryBalance[j] - interestpayments)
    
    #repay debt when surplus, take up debt when deficit
    TotalDebt = c(TotalDebt, TotalDebt[j] - GovBalance[j])
    
    NewDebt = c(NewDebt, max((DebtMaturityProfile[j] - GovBalance[j]),0))
  }
  
  FctDebtmatrix = rbind(FctDebtmatrix, TotalDebt[-1])
}

DebtGDPmatrix = FctDebtmatrix/FcGDPmatrix


### Evaluation ###

# probability for each year of the forecast to achieve a debt GDP ratio <= 90% 
DebtGDP_90_prob = c()

# check for each year all simulated values and count the amount of which fall within <= 0.9
# divide by amount of simulations run to determine percentage
for(i in seq(ncol(DebtGDPmatrix))) {
  DebtGDP_90_prob = c(DebtGDP_90_prob, sum(DebtGDPmatrix[,i]<=0.9)/SAMPLES)
}
DebtGDP_90_prob

# probability in 2030 of debt to GDP ratio >X 
DebtGDP_prob = c()
for(i in seq(from=90, to=300)) {
  DebtGDP_prob = c(DebtGDP_prob, sum(DebtGDPmatrix[, ncol(DebtGDPmatrix)]>(i/100))/SAMPLES)
}
DebtGDP_prob

plot = plot_ly(y=DebtGDP_prob, x0=90, type='scatter', mode='lines', line=list(width=2))
plot = layout(plot, yaxis=list(tickformat="%", title="Probability"), xaxis=list(title="Debt to GDP Ratio in 2030",  range=c(90,300), 
                                                                               tickprefix=">", ticksuffix="%", tick0=90, dtick=20, tickmode="linear"))
plot 

# debt stabilisation, defines as debt-GDP ratio will not grow, 2017 value 178%
DebtGDP_stable_prob = c()
for(i in seq(ncol(DebtGDPmatrix))) {
  DebtGDP_stable_prob = c(DebtGDP_stable_prob, sum(DebtGDPmatrix[,i]<=1.78)/SAMPLES)
}
DebtGDP_stable_prob

plot = plot_ly(y = DebtGDP_stable_prob, x = years, name = i, mode = 'lines',  type = 'scatter', line = list(width=2))
plot = layout(plot, yaxis=list(tickformat="%", title="Probability of Debt-GDP <=178%"), xaxis=list(title="Year"), margin=list(r=30))
plot


### Dispersion ###

# for each year calculate the difference between the 0.95 and 0.05 quantile of all simulations 
temp_DebGDPtdifference = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.95)) - apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.05))
temp_DebGDPtdifference

DebtGDP_dispersion_quantiles = c()

# now for each of the yearly differences determine the quantile that the difference represents 
# of all simulations of that respective year 
for (y in seq(13)) {
  f <- ecdf(DebtGDPmatrix[,y])
  DebtGDP_dispersion_quantiles = c(DebtGDP_dispersion_quantiles, f(temp_DebGDPtdifference[y]))
}

# see "Debt sustainability analysis for euro area sovereigns: a methodological framework"
# percentile < 33, green
# 33 < percentile >= 66th, yellow
# 66 < percentile), red
DebtGDP_dispersion_quantiles

p2  <- plot_ly()
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) min(x)),
                 ymax = apply(DebtGDPmatrix, 2, function(x) max(x)),
                 line = list(color = 'rgba(7, 164, 181, 0.2)'),
                 fillcolor = 'rgba(7, 164, 181, 0.2)',
                 name = "Min-Max band")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "5th-95th percentile")
p2 = add_trace(p2, x = years,y = temp_DebGDPtdifference, name = "Difference 95th-5th percentile", mode = 'lines',  type = 'scatter', line = list(width=0.5, color="black"))
p2 = add_trace(p2, x = years,y = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.33)), name = "33th percentile", mode = 'lines',  type = 'scatter', line = list(width=0.5, color="green"))
p2 = add_trace(p2, x = years,y = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.66)), name = "66th percentile", mode = 'lines',  type = 'scatter', line = list(width=0.5, color="red"))
p2 = layout(p2, yaxis =list(tickformat="%", title="Debt-GDP Ratio"), xaxis=list(title="Year"), legend = list(x = 0.04, y = 1),  margin=list(r=30))
p2

### Plotting ###

#Plot Debt
p1 <- plot_ly()
for(i in seq(nrow(FctDebtmatrix))) {
 p1 = add_trace(p1, y = FctDebtmatrix[i,], x = years, name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
}
p1 = layout(p1, showlegend=F, yaxis=list(title="Debt (in million Euro)"), xaxis=list(title="Year"), margin=list(r=30))
p1

#Plot GDP 
p1 <- plot_ly()
for(i in seq(nrow(FcGDPmatrix))) {
  p1 = add_trace(p1, y = FcGDPmatrix[i,], x = years, name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
}
p1 = layout(p1, showlegend=F, yaxis=list(title="GDP (in million Euro)"), xaxis=list(title="Year"), margin=list(r=30))
p1

#Plot Debt GDP ratio 
p1 <- plot_ly()
for(i in seq(nrow(DebtGDPmatrix))) {
  p1 = add_trace(p1, y = DebtGDPmatrix[i,], x = years, name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
}
p1 = layout(p1, showlegend=F, yaxis=list(tickformat="%", title="Debt-GDP Ratio"), xaxis=list(title="Year"), margin=list(r=30))
p1


#AVGs, add existing values for 2017 to get growth for 2018
debtgrowth_avg = 0
for(i in seq(nrow(FctDebtmatrix))) {
  debtgrowth_avg = debtgrowth_avg + percentChange(ts(c(Govdebt2017, FctDebtmatrix[i,])))
}
debtgrowth_avg = debtgrowth_avg/nrow(FctDebtmatrix)
debtgrowth_avg

gdpgrowth_avg = 0
for(i in seq(nrow(FcGDPmatrix))) {
  gdpgrowth_avg = gdpgrowth_avg + percentChange(ts(c(realGDPsamples[length(realGDPsamples)]*GDPDeflatorSamples[length(GDPDeflatorSamples)]/100 
                                                       ,FcGDPmatrix[i,])))
}
gdpgrowth_avg = gdpgrowth_avg/nrow(FcGDPmatrix)
gdpgrowth_avg

debtgdpgrowth_avg = 0
for(i in seq(nrow(DebtGDPmatrix))) {
  debtgdpgrowth_avg = debtgdpgrowth_avg + percentChange(ts(c(Govdebt2017/(realGDPsamples[length(realGDPsamples)]*GDPDeflatorSamples[length(GDPDeflatorSamples)]/100)  
                                                              ,DebtGDPmatrix[i,])))
}
debtgdpgrowth_avg = debtgdpgrowth_avg/nrow(DebtGDPmatrix)
debtgdpgrowth_avg


# plot prediction intervals
p2  <- plot_ly()
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.025)),
                 ymax = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.975)),
                 line = list(color = 'rgba(7, 164, 181, 0.2)'),
                 fillcolor = 'rgba(7, 164, 181, 0.2)',
                 name = "2.5th-97.5th percentile")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.05)), 
                 ymax = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.95)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.4)',
                 name = "5th-95th percentile")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.25)), 
                 ymax = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.75)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.6)',
                 name = "25th-75th percentile")
p2 = add_ribbons(p2, x = years,
                 ymin = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.45)), 
                 ymax = apply(DebtGDPmatrix, 2, function(x) quantile(x, 0.55)),
                 line = list(color = 'rgba(7, 164, 198, 0.4)'),
                 fillcolor = 'rgba(7, 164, 198, 0.8)',
                 name = "45th-55th percentile")
#p2 = add_trace(p2, x = years,y = temp_GDPDebtdifference, name = i, mode = 'lines',  type = 'scatter', line = list(width=0.5))
p2 = layout(p2, yaxis =list(tickformat="%", title="Debt-GDP Ratio"), xaxis=list(title="Year"), legend = list(x = 0.04, y = 1.02), margin=list(r=30))
p2



