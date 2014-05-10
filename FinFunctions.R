library('tseries')
library('zoo')
library('xts')


#acf1 = acf(coredata(data), plot=T)
#acf1$acf
#plot(acf1, ci = 0.99)


toZoo = function(data) {
  #Turns data pulled from FinanceDB to zoo object
  index = as.Date(data[,1])
  zdata = zoo(data[,-1],order.by=index)
  return(zdata)
}


fn2DEGCItest = function(data,modStr)
{
  #Given p-value for ADF test for series in data
  #you have to specify the model string
  
  CItest =lm(modStr,data=data)
  ciresids = CItest$residuals
  ciTest = adf.test(ciresids)
  cipval = ciTest$p.value
  return(cipval)
}


fnCItestwithPlots = function(data, modStr){
  #Does CI-test but gives some plots too
  #rets = (diff(dimData,lag=1)/dimData)+1
  #gIndex = cumprod(rets)
  #plot(dimData)
  #plot(gIndex , screens=c(1,1),col=c(2,3))
  CItest = lm(modStr,data=data)
  print(CItest)  
  ciresids = CItest$residuals
  print(adf.test(ciresids))
  xaxis = as.Date(names(ciresids))
  ciresids = zoo(ciresids,order.by=xaxis)
  plot.zoo(ciresids,type='l')
  plot(as.xts(ciresids))
  abline(h=0)
  sd = 0.5*sd(ciresids)
  abline(h =c(-sd, sd),col=2)
  sd = 1.5*sd(ciresids)
  abline(h =c(-sd, sd),col=4)
  
  #fittedVals = CItest$fitted.values
  #fittedVals = xts(fittedVals,order.by=index(data))
  #plot(fittedVals,type='l',ylim=c(1.2,1.4))
  #lines(data[,2],col=2)
  
}
