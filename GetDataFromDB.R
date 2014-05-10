library("RODBC", lib.loc="/usr/lib/R/site-library")
library('zoo')
library('tseries')
library('xts')
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
head(SP500)
plot(SP500[,'Close'], type = 'l')

SP500xts = xts(SP500)
SP500red = SP500xts['2011-01-01::2012-01-01','Adj_Close']
plot(SP500red)

ret = CalculateReturns(SP500red)#, method ='compound')
head(ret)
ret = sqrt(252) * ret
  
head(SP500red)
ret2 =SP500red/lag(SP500red)-1
head(ret2)
head(ret)
plot(ret)


meanRet = mean(ret2,na.rm=T)
stdev = StdDev(ret2)
meanRet/stdev
sr=SharpeRatio(ret)
sr[1]*sqrt(250)
SharpeRatio(ret2)
SharpeRatio.annualized(ret2)



library('PerformanceAnalytics')

library('quantmod')
getSymbols('INDEXDJX',src='google')


#Get list of SP500 constituent tickers
SP500tickers = read.table('sp500.csv')
colnames(SP500tickers)='ticker'

#Check which tickers are in db
dbTickers = SP500tickers[as.character(SP500tickers[,'ticker']) %in% tblsName,]
dbTickers = as.character(dbTickers)

#Check if dbTickers all read from db
missingTicker = list()
for(ticker in dbTickers)
{
#  print(ticker)
  #browser()
  data = tryCatch(sqlFetch(con, sqtable=ticker),error = function(e){return(e)})
  if(class(data)!='data.frame'){
    missingTickers[ticker] = ticker
    print(paste('Problem with ','ticker'))
    next
  }
}

#Read into SP500 into one big set
dim = 'Adj Close'
FullSP500 = SP500[,dim]
colnames(FullSP500)
i=2
#problem with ticker = NSM = dbTicker[308]
dbTickers[308]
head(sqlFetch(con, dbTickers[308]))

##ERROR: RANGE OF COMBINED DATA IS REALL SMALL. DON'T USE
for(ticker in dbTickers[-308]){
  #browser()
  thisRead = sqlFetch(con, ticker)
  thisRead = toZoo(thisRead)
  thisRead = thisRead[,dim]
  FullSP500 = merge(FullSP500,thisRead,all=F)
  colnames(FullSP500)[i]=ticker
  i = i +1 
}
colnames(FullSP500)[1]="SP500"
write(FullSP500,'FullSP500AdjClose',sep=",")
head(FullSP500)

range(index(FullSP500))

head(FullSP500[,65])
BA[,]
which(colnames(FullSP500)=='BA')

#Check which are I(1)
I1Check = dbTickers
I1Check =cbind(I1Check,0,0)
colnames(I1Check) = c('ticker','pval','diff_pval')
for(ticker in dbTickers){
  print(ticker)
  data = tryCatch(sqlFetch(con, sqtable=ticker),error = function(e){return(e)})
  if(class(data)!='data.frame'){
    missingTickers[ticker] = ticker
    print(paste('Problem with ',ticker))
    next
  }
  data = toZoo(data)
  #plot(data[,dim])
  cdata = coredata(data[,dim])
  #Augmented dickey fuller test
  adft1 = adf.test(cdata)
  pval = adft1$p.value
  #browser()
  I1Check[I1Check[,1] == ticker,2] = pval
  #Check if diff is stationary
  diffData = diff(data[,dim],1)
  #plot(diffData)
  cdiffData = coredata(diffData)
  adft2 = adf.test(cdiffData)
  pvalDiff = adft2$p.value
  I1Check[I1Check[,1] == ticker,3] = pvalDiff   
}
#write.table(I1Check,'I1CheckSP500.csv',sep=',',row.names=F)
I1Check = read.table('I1CheckSP500.csv',sep=',',header=T,stringsAsFactors=F)
critVal = 0.05
I1Check =(I1Check)
class(I1Check)
ProblemTickers = I1Check[I1Check$pval == 0 ,]
StationaryTickers = I1Check[( I1Check$pval != 0) & (I1Check$pval < critVal) ,]
I1tickers = I1Check[I1Check$pval > critVal ,]
#List of I1 tickers
I1tickersList = I1tickers$ticker


#Check if ticker is Co-integrated with sp500
CointTickers = cbind(dbTickers,0)
colnames(CointTickers)= c('ticker','pval')
compData = SP500

for( ticker in as.character(I1tickersList)){
  #print(ticker)
  data=tryCatch(sqlFetch(con, sqtable=ticker),error = function(e){})
  if(class(data)!='data.frame')
  {
    print('error with ticker')
    next
  }
  data=toZoo(data)
  cointData = merge(compData[,dim],data[,dim])
  colnames(cointData) = c('x','y')
  CItest = lm("y~x",data=cointData)
  ciresids = CItest$residuals
  ciTest = adf.test(ciresids)
  cipval = ciTest$p.value
  CointTickers[CointTickers[,1] == ticker,'pval']=cipval 
}
head(CointTickers)
write.table(CointTickers, "SP500TickersADFtest.csv",sep=",",row.names=F)
CointTickers = read.table("SP500TickersADFtest.csv",sep=",",stringsAsFactors=F,header=T)
#Retrieve tickers that have coint less than critical value
cointCritValue = 0.05
CointTickers2 = CointTickers[0<CointTickers$pval & CointTickers$pval <= cointCritValue,]
CointTickers2 = CointTickers2[order(CointTickers2$pval),]

head(CointTickers[order(CointTickers$pval),],20)

analCI(1)
#Display residuals and series for tickers cointegrated to SP500
analCI = function(tn){
  ticker = as.character(CointTickers2$ticker[tn])
  data = sqlFetch(con,sqtable=ticker)
  data = toZoo(data)
  dimData = merge(SP500[,dim],data[,dim],all=F)
  #rets = (diff(dimData,lag=1)/dimData)+1
  #gIndex = cumprod(rets)
  #plot(dimData)
  #plot(gIndex , screens=c(1,1),col=c(2,3))
  colnames(dimData)=c('SP500',ticker)
  model = paste(ticker,"~SP500",sep="")
  CItest = lm(model,data=dimData)
  print(CItest)  
  ciresids = CItest$residuals
  print(adf.test(ciresids))
  xaxis = as.Date(names(ciresids))
  ciresids = zoo(ciresids,order.by=xaxis)
  plot.zoo(ciresids,type='l')
  #plot(as.xts(ciresids))
  abline(h=0)
  sd = 0.5*sd(ciresids)
  abline(h =c(-sd, sd),col=2)
  sd = 1.5*sd(ciresids)
  abline(h =c(-sd, sd),col=4)
  
  fittedVals = CItest$fitted.values
  plot(fittedVals,type='l')
  lines(coredata(data[,dim]),col=2)
  print(ticker)
}
analCI(1)