library('zoo')
library('tseries')
library('xts')
library('PerformanceAnalytics')

#generate lognormal price series with drift = 0.05, stdev =0.2
s=c(1)
s[1]
for( i in seq(1:100)){
  ds = s[i]*(0.05 + 0.2*rnorm(n=1))
  thisS = s[i] +ds
  s = c(s,thisS)
}
plot(s,type='l')

Perf_rets = CalculateReturns(s)
Man_rets = s/lag(s,1)-1

head(Perf_rets)
head(Man_rets)

toZoo = function(data) {
  #Turns data pulled from FinanceDB to zoo object
  index = as.Date(data[,1])
  zdata = zoo(data[,-1],order.by=index)
  return(zdata)
}


SP500xts = xts(SP500)

SP500red = SP500xts['2011-01-01::2012-01-01','Adj_Close']
plot(SP500red)

ret = CalculateReturns(SP500red)#, method ='compound')


head(SP500red)
ret2 =SP500red/lag(SP500red)-1
head(ret2)
head(ret)
plot(ret)


meanRet = mean(ret2,na.rm=T)
stdev = StdDev(ret2)
meanRet/stdev
SharpeRatio(ret2)
SharpeRatio.annualized(SP500red)


