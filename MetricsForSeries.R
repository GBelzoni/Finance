library("RODBC", lib.loc="/usr/lib/R/site-library")
library('zoo')
library('tseries')
library('xts')
library('PerformanceAnalytics')
library('ggplot2')
library('reshape2')

odbcDataSources()
con = odbcConnect('finance')
con
tbls = sqlTables(con)
tblsName = tbls$TABLE_NAME

toZoo = function(data) {
  #Turns data pulled from FinanceDB to zoo object
  index = as.Date(data[,1])
  zdata = zoo(data[,-1],order.by=index)
  return(zdata)
}

SP500 = sqlFetch(con, sqtable='SP500')
SP500 = toZoo(SP500)

series = sqlFetch(con, sqtable='AAPL')
series = toZoo(series)
dim = 'Adj_Close'
data = merge(SP500[,dim],series[,dim],all=F)
colnames(data) = c('SP500','Series')
head(data)
plot(data[,1])

range(index(data))
d1 = as.Date('2012-01-01')
d2 = as.Date('2013-01-01')
data_red = window(data,start = d1, end =d2)


rets = Return.calculate(data_red)
rets = rets[-1,]
head(rets)
rets_an = 252*rets
head(rets_an)
cumrets = merge(data_red[,1]/as.numeric(data_red[1,1]),
                data_red[,2]/as.numeric(data_red[1,2]))

Return.cumulative(rets)
colnames(cumrets) = colnames(rets)



func_beta = function(x){CAPM.beta(Ra = x, Rb= rets_an[,1])}
func_alpha = function(x){CAPM.alpha(Ra = x, Rb= rets_an[,1])}                        
func_alpha(rets_an)
func_beta(rets_an)

table.TrailingPeriods(rets,
                      FUNCS = c("func_beta","func_alpha"))#,
#                       func.names = c("beta","alpha"))
chart.TimeSeries(cumrets)


CAPM.beta(Ra=rets_an[,'Series'],Rb=rets_an[,'SP500'])
CAPM.alpha(Ra=rets_an[,'Series'],Rb=rets_an[,'SP500'])
CAPM.CML(Ra=rets_an[,'Series'],Rb=rets_an[,'SP500'])
plot(rets_an[,'SP500'],rets_an[,'Series'])

pdata = as.data.frame(rets)
pdata['date']=index(rets)
pdata = melt(pdata,id.vars=c('date'))

head(pdata)
ggplot(pdata, aes(y = value,x = date, color = variable)) + geom_line()
mod = lm('Series~SP500',data=rets_an)
abline(mod)
mod$coefficients

table.AnnualizedReturns(rets)



