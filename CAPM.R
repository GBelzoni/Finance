library("RODBC", lib.loc="/usr/lib/R/site-library")
library('zoo')
library('tseries')
library('xts')
library('PerformanceAnalytics')
library('ggplot2')
library('reshape2')
library('vars')

odbcDataSources()
con = odbcConnect('finance')
con
tbls = sqlTables(con)
tblsName = tbls$TABLE_NAME
tblsName[order(tblsName)]

SeriesList = sqlFetch(con,sqtable='SeriesList')

sharpe_r = function(rets){
  #calc sharp from rets
  sr = mean(rets,na.rm=T)/apply(rets,2, sd, na.rm=T)
  sr = as.matrix(sr)
  return( sr)
}
toZoo = function(data) {
  #Turns data pulled from FinanceDB to zoo object
  index = as.Date(data[,1])
  zdata = zoo(data[,-1],order.by=index)
  return(zdata)
}
pl_data = function(data, idvars = 'date'){
  #formats data to use with ggplot
  pdata = as.data.frame(data)
  pdata[idvars] = index(data)
  pdata = melt(pdata, id.vars='date')
  return(pdata)  
}

#Read data
SP500 = sqlFetch(con, sqtable='SP500')
SP500 = toZoo(SP500)
DJIA = sqlFetch(con,sqtable = 'DJIA')
DJIA = toZoo(DJIA)
FTSE = sqlFetch(con,sqtable = 'FTSE_100')
FTSE = toZoo(FTSE)

#Merge data on dim
dim = 'Adj_Close'
data = merge(SP500[,dim],DJIA[,dim],FTSE[,dim])
colnames(data) = gsub( "(.*)\\[.*", "\\1", colnames(data))
range(index(data))
data = as.xts(data)

#plot data
pdata = na.omit(data["2012-01-01::2013-01-01"])
head(pdata)
ggplot(pl_data(pdata), aes(y = value,x = date, color = variable)) + geom_line()

#Calc rets
rets = Return.calculate(pdata)
ggplot(pl_data(rets), aes(y = value,x = date, color = variable)) + geom_line()
sr = SharpeRatio.annualized(rets)
R = cor(rets,use="pairwise.complete.obs")
V = var(rets,use="pairwise.complete.obs")
mean_rets = mean(rets,na.rm=T)
sr = sharpe_r(rets)

max_sr = sqrt(t(sr) %*% solve(R) %*% (sr))
max_sr*sqrt(252) #mult by annualising factor

#Create portfolio from weights
omega1 = 0.1
omega2 = 0.2
weight = as.matrix(c(omega1, omega2, 1-omega1-omega2))
weight = scaled_calc_weights_full
port_rets = as.matrix(rets) %*% weight
port_rets[1,]=0
head(port_rets)
sharpe_r(port_rets)*sqrt(252)

port_sr = function(weights, rets){
  #function to optim to find max weights
  #returns -sr as optim uses min by default
  sum_weights = sum(weights)
  weight_vec = c(weights, 1 - sum_weights)
  weight_vec = as.matrix(weight_vec)
  port_rets =  as.matrix(rets) %*% weight_vec
  return(sharpe_r(port_rets)[1,1])
}
port_sr(weights=c(0.1,0.2),rets)*sqrt(252)

res = optim(par= c(0.1,0.1),fn=port_sr,rets=rets)
weights_calc =res$par
calc_weights_full = c(weights_calc,1-sum(weights_calc))
scaled_calc_weights_full = calc_weights_full/ sqrt(t(matrix(calc_weights_full))%*%matrix(calc_weights_full))
sr_calc = - res$value*sqrt(252)
port_sr(calc_weights_full[1:2],rets)
#These should be very close
sr_calc
max_sr*sqrt(252)

#Solve theoretical weights
#Weights in P_M are scaled by weight in total pf
#therefore calc constant without scaling and then calc weight so
#sum to 1. This makes weights of P_M sum to 1 and weight of rf=0
#ie this is weights in P_M only portfolio
theo_weights =  (solve(V) %*% mean_rets) #Note I had to use V not R here
theo_weights = theo_weights/sum(theo_weights)
sum(theo_weights)

#Should be close on first n-1 indices
theo_weights
weights_calc 

#How does market cap go??
market_cap_w = pdata[1]/apply(pdata,1, sum)[1]
port_sr(as.numeric(market_cap_w[,c(1,2)]),rets)*sqrt(252)

v1 = VAR(na.omit(rets),p=10)
plot(v1)
pred1 = predict(v1,n.ahead=10)
ind = index(data["2013-01-01::2013-01-20"])[1:10]
pdata = lapply(pred1$fcst,zoo,order.by=ind)

data_test = na.omit(data["2013-01-01::2013-01-20"])
rets_test = Return.calculate(data_test)
rets_test = na.omit(rets_test)

a = rets_test[,'SP500']
fcst = as.xts(pdata$SP500$fcst)
dat=merge(a,fcst)
dat1 = pl_data(dat)
ggplot(dat1, aes(x=date,y=value,color=variable))+geom_line()
residuals(v1)
fanchart(pred1)

v1
v1 = VAR(na.omit(rets[,c('DJIA','FTSE')]),p=6)
fevd(v1)
summary(v1)
stability(v1)
serial.test(v1)
causality(v1,cause=c('FTSE'))
normality.test(v1)

qqnorm(residuals(v1)[,2])
qqnorm(residuals(v1)[,1])
