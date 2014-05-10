library('xts')
library('zoo')
library('tseries')
library('urca')

#fxSeries defined in FX.R
data = fxSeries
series = colnames(data)

#SP500 ticker defined in GetDataFromDb.R
#All SP500 that are I1
numNotInI1tickers = which((I1tickersList %in% colnames(FullSP500) ) == F)
data = FullSP500[, I1tickersList[-numNotInI1tickers]]
series = colnames(data)


##Do EG Coint Test

#Check if Integrated with adf-test
cdata = apply(coredata(data),2,adf.test)
tableI1test = sapply(cdata, function(x){return(x$p.value)})
tableI1test[tableI1test< 0.05]

#Create models for EG CI regression
combSeries = as.data.frame(combn(series,2))
head(combn(series,2))

models = list()
for( x in combSeries )
{
  models = c(models, paste(x[1],"~",x[2],sep=""))
}

head(fxSeries)
modtest = "AUD.USD~GBP.USD"
fn2DEGCItest(data,modtest)

results = data.frame(matrix(0,ncol(combSeries),2))
i=1
for(mod in models)
{
  thisResult = c(mod, fn2DEGCItest(data,mod))  
  print(thisResult)
  results[i,]=thisResult
  i=i+1
}
results

fnCItestwithPlots(data, modStr=results[9,1])

#Do ECM for coint SP500 tickers.
CointTickers2 #sorted tickers most cointegrated with SP500

series1tick ='KIM'
series1 = sqlFetch(con, sqtable=series1tick)
series1 = toZoo(series1)
dim='Adj Close'
dataTest = merge(SP500[,dim],series1[,dim],all=F)
colnames(dataTest)=c('SP500',series1tick)
head(dataTest)
modTest = lm(paste('SP500',series1tick,sep="~") , data=dataTest)
adf.test(coredata(modTest$residuals))
plot(modTest$residuals,type='l')
range(index(dataTest))


ecmData = dataTest
head(ecmData)
modStr = paste('SP500','~',series1tick,sep="")
mod = lm(modStr, data=dataTest)
summary(mod)
resids = zoo(mod$residuals, order.by = as.Date(names(mod$residuals)))
adf.test(coredata(resids))
dataTest[,1] - mod$coefficients[2]*dataTest[,2]


decmData = diff(ecmData)
lags = 2
ecmData2 = lag(decmData,0:lags)
disequilTerm = lag(resids,lags-1)
head(disequilTerm)
ecmData2 = merge(disequilTerm,ecmData2,all=F)
head(ecmData2)
explainedVecs = colnames(ecmData2)[(2*lags+2):(2*lags+3)]
explanatoryVecs = setdiff(colnames(ecmData2),explainedVecs)
ecm1mod = paste(explainedVecs[1], paste(explanatoryVecs, collapse = '+'),sep='~')
ecm2mod = paste(explainedVecs[2], paste(explanatoryVecs, collapse = '+'),sep='~')
ecm1modRslt = lm(ecm1mod, data = ecmData2)
summary(ecm1modRslt)
ecm2modRslt = lm(ecm2mod, data = ecmData2)
summary(ecm2modRslt)


head(diff(BA))
head(diff(BA))
data(denmark)

vecmData = coredata(ecmData)
vecm = ca.jo(vecmData, ecdet = "const", type="trace", K=2, spec="longrun")
summary(vecm)
vecmSum = summary(vecm)
weights = vecmSum@V[1:2,1]
const = vecmSum@V[3,1]

plot(ecmData %*% weights + const,type='l')
lines(mod$residuals, col=2)

plot((ecmData %*% weights + const) - mod$residuals)


sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- ca.jo(sjd, ecdet = "const", type="eigen", K=2, spec="longrun",
season=4)
summary(sjd.vecm)

data(finland)
sjf <- finland
sjf.vecm <- ca.jo(sjf, ecdet = "none", type="eigen", K=2,
spec="longrun", season=4)
summary(sjf.vecm)
plotres(sjd.vecm)
cajools(sjd.vecm)


