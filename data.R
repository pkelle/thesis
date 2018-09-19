
# https://www.esm.europa.eu/assistance/greece/efsf-programme-greece-expired
# 1) disbursed 09/03/2012 34.6 billion euro, Amortisation from 2023 to 2042 
# 2) disbursed 17/12/2012 11.3 billion euro, Amortisation from 2023 to 2042
# total 45,9 in 20 years yields 2,295 per year
# the repayment of the loan tranche is equally distributed per year during the repayment period
# in million euro
#efsm_debt_repayment_installments = ts(c(0, 0, 0, 0, 2295, 2295, 2295, 2295, 2295, 2295, 2295, 2295), start=2019, frequency=1) #up until 2030


# Issued Benchmark Bonds outstanding, maturing up until 2030
# http://www.pdma.gr/en/debt-instruments-greek-government-bonds/benchmark-bonds-outstanding
#GR0114028534 	Hellenic Republic 	4.750% 	17-Apr-14 	17-Apr-19 	2,456,470,000 EUR
#GR0114029540 	Hellenic Republic 	4.375% 	1-Aug-17 	1-Aug-22 	3,000,000,000 EUR
#GR0114030555 	Hellenic Republic 	3.500% 	5-Dec-17 	30-Jan-23 	4,355,990,324 EUR
#GR0118017657 	Hellenic Republic 	3.375% 	15-Feb-18 	15-Feb-25 	3,000,000,000 EUR
#GR0124034688 	Hellenic Republic 	3.750% 	5-Dec-17 	30-Jan-28 	5,962,747,327 EUR
#bonds_maturing = ts(c(2456.5, 0, 0, 3000, 4356, 0, 3000, 0, 0, 5962.7, 0, 0), start=2019, frequency=1)


print (bonds_maturing+efsm_debt_repayment_installments)
# floating rate notes ESM?

# IMF repayment schedule?

#http://www.pdma.gr/attachments/article/37/Maturity%20Profile%20Central%20Government%20Debt%20Table_30-06-2018.pdf
yearly_total_debt_repayments = ts(c(39300,13861,5066,5102,9825,12183,7684,9057,6728,6709,12146,5597,6254), start=2018, frequency=1)

