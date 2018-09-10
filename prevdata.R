library(MonteCarlo)
library(plotly)
library(scales)
library(dplyr)
library(tfplot)


year=c(1995:2017)

#in million euro
Y = c(93064, 103037, 114712, 125263, 133789,	141247, 152194,	163461, 178905, 193716, 199242, 217862, 232695,	241990, 237534, 226031, 207029, 191204, 180654, 178656, 176312, 174199, 177735)
I = c(20913,	24063,	25742,	31535,	32310,	36478,	39101,	40456,	48975,	49024,	44031,	56975,	63133,	59315,	43559,	38534,	31271,	24480,	20958,	21280,	17312,	18482,	20844)
G = c(16559,	18143,	20284,	21918,	24055,	25792,	28483,	31429,	33918,	37106,	39889,	43911,	47750,	50143,	55367,	50188,	45104,	41564,	36924,	36230,	35940,	35239,	35508)
C = c(63277, 70024,	76520,	83929,	89285,	94485,	100686,	108123,	115871,	124017,	131813,	139856,	150862,	163039,	161838,	156803,	144678,	133668,	127853,	125441,	122969,	121737,	123296)
NX = c(-7685,	-9193,	-7834,	-12119,	-11861,	-15507,	-16075,	-16548,	-19860,	-16431,	-16490,	-22880,	-29049,	-30507,	-23230,	-19495,	-14024,	-8508,	-5081,	-4294,	91,	-1258,	-1913)


#starting with 96
#Y_change=Y[-1]/Y[-length(Y)]-1
#I_change=I[-1]/I[-length(I)]-1
#G_change=G[-1]/G[-length(G)]-1
#C_change=C[-1]/C[-length(C)]-1
#NX_change=NX[-1]/NX[-length(NX)]-1

Y_ts <- ts(Y, start = c(1995, 1), frequency = 1)
I_ts <- ts(I, start = c(1995, 1), frequency = 1)
G_ts <- ts(G, start = c(1995, 1), frequency = 1)
C_ts <- ts(C, start = c(1995, 1), frequency = 1)
NX_ts <- ts(NX, start = c(1995, 1), frequency = 1)

percentChange(Y_ts)

#gdpplot =  plot_ly( x = ~years, y = ~Y, type = 'scatter', mode = 'lines+markers', name='GDP')
#gdpplot

p = plot_ly(x = ~year[-1], y = ~percentChange(Y_ts), name = 'Y%', type = 'scatter', mode = 'lines+markers', color = '#F5FF8D')
p = add_trace(p, y = ~percentChange(I_ts), name = 'I%', color = '#blue')
p = add_trace(p, y = ~percentChange(G_ts), name = 'G%', color = '#red')
p = add_trace(p, y = ~percentChange(C_ts), name = 'C%', color = '#green')
p = add_trace(p, y = ~percentChange(NX_ts), name = 'NX%', color = '#pink')
p


#p = plot_ly(x = ~year, y = ~(NX+C+I+G), name = 'NX+C+G+I', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = '#F5FF8D')
#p = add_trace(p, y = ~(NX+C+G), name = 'NX+C+G', fillcolor = '#blue')
#p = add_trace(p, y = ~(NX+C), name = 'NX+C', fillcolor = '#red')
#p = add_trace(p, y = ~NX, name = 'NX', fillcolor = '#green')
#p



