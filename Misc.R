
#Calculate signal
getCIresids = function(data1,data2,colnames=c('data1','data2'))
{
  dimData = merge(data1,data2,all=F)
  colnames(dimData)=colnames
  model = paste(colnames[1],"~",colnames[2],sep="")
  CItest = lm(model,data=dimData)
  print(CItest)  
  xaxis = as.Date(names(ciresids))
  ciresids = CItest$residuals
  ciresids = zoo(ciresids,order.by=xaxis)
  plot.zoo(ciresids,type='l')
  return(ciresids)
}  

ticker = as.character(CointTickers2[1,1])
tickerData = sqlFetch(con,sqtable=ticker)
tickerData = toZoo(tickerData)
tickerData = tickerData[,dim]
colnames = c(ticker, "SP500")

resids = getCIresids(tickerData,SP500[,dim],colnames)

EntryLevel = 1.5
ExitLevel = 0

ir = index(resids)

Notional = 100
sd= sd(resids)
for( i in 1:length(index(resids))){
  
  if()
  
  print(resids[i])
}

analCI(1)
#Do AR1 model - manually
y=data[,dim]
y1 = lag(y,-1)
d2 = merge(y,y1)
plot(d2)
AR1 = lm('y~y1',data=d2)
plot(AR1)
resids = AR1$residuals
acfticker = acf(resids,plot=T)

#Compare acf of SP500 to ticker. 
#IF AR(1) coeff is approx=1 and ticker correlated to SP500 then will look similar
plot(acf1$acf)
points(acfticker$acf,col=2)
#Test normality of residuals
plot(AR1$residuals)
jarque.bera.test(AR1$residuals)

#ARIMA gives funny results for intercept - returns mean rather than intercept??
ARMA = arma(coredata(data[,dim]),c(9,0,0))
summary(ARMA)
AR1

#Check if ticker is Co-integrated with sp500
cointData = merge(SP500[,dim],data[,dim])
colnames(cointData) = c('x','y')
CItest = lm("y~x",data=cointData)
ciresids = CItest$residuals
ciTest = adf.test(ciresids)
cipval = ciTest$p.value

yhoo = sqlFetch(con, sqtable="BA", as.is=T)
yhoo = toZoo(yhoo)
head(yhoo)
plot(yhoo)
dim = 'Close'

imData = merge(SP500[,dim],yhoo[,dim],all=F)
rets = (diff(dimData,lag=1)/dimData)+1
gIndex = cumprod(rets)
plot(dimData)
plot(gIndex , screens=c(1,1),col=c(2,3))
colnames(dimData)=c('SP500','YHOO')
head(dimData)
diffdimData = diff(dimData)
plot(diffdimData)

testD = diffdimData
dftest = apply(coredata(dimData),2,adf.test)
dftest
modData = dimData
head(modData[,1])
x = SP500[,'Adj Close']; y= yhoo[,'Adj Close']
mod1 = lm("y ~ x")
summary(mod1)
resid = mod1$residuals
plot(resid)
adf.test(coredata(resid))



for( tn in tblsName) {
  
  data = sqlFetch(con, sqtable=tn, as.is=T)
  index = as.Date(data[,1])
  dataz = zoo(data[,'Close'],order.by=index)
  Cldata = merge(Cldata,dataz)
  
}



class(data$Date)

sqlTxt = "SELECT DATE FROM YHOO"
qry =sqlQuery(con,sqlTxt,as.is=T)
qry
qry[1,]
sqlColumns(con,"YHOO",as.is=T)




data = data[0:(nrow(data)/2),]
sqlSave(con,data,tablename='YHOO')

df3 = data.frame(df)
df3 = data.frame(cbind(df, c('a','b','c')))
colnames(df3) = c('n1', 'n2', 'n3', 'n4')
df2 = sqlFetch(con,'df')

if( is.data.frame(df3)) { 
sqlSave(con,df3,rownames=F,safer=F)
}

if( is.data.frame(df3)) { 
sqlUpdate(con,df3,index=)
}