#AUDUSD defined read in FX.R
library('tseries')
library('xts')
head(AUDUSD)

plot(AUDUSD)
data = SP500[,'Adj Close']
ret = diff(log(data),1)
mean(ret[-1])
sqrt(var(ret[-1])*252)
rm = rollmean(ret[-1]^2,8,align="right")
plot(as.zoo(merge(ret,rm, all=F)))



alpha = 0.012
beta = 0.89
omega = 0.000

garch_gen <- function (ret, omega, alpha, beta, sig0)
{
  
  lret = length(ret)
  gVol = rep(0,lret)
  gVol[1]=sig0
  #print(lret)
  for( i in 2:lret){
    
    gVol[i]=omega+alpha*ret[i-1]^2+beta*gVol[i-1]
 
  }
  return(gVol)
}
eret = ret[-(1:3)] - mean(ret[-(1:3)])
erm = rollmean(eret^2,2,align="right")
gVol = garch_gen(eret,omega,alpha,beta,0)
length(gVol)
gVol = as.zoo(gVol, order.by = index(eret))
sbst = 100:length(eret)
plot( as.zoo(merge(eret^2,erm,gVol,all=F)))

LLF = function( alpha1,beta1,omega1){

  sbst = 100:length(eret)
  gvol1 = garch_gen(ret=eret[sbst,],omega1,alpha1,beta1,sig0=0)
  #result = log(gvol1) + (ret[-3]^2/gvol1)
  #print(length(gvol1))
  #print(length(ret[-(1:3)]))
  result = sum(log(gvol1) + (eret[sbst,]^2/gvol1),na.rm=T)
  return(result)
}
LLF(0.1,0.6,0)

TwoDLLF = function(alpha2, beta2){return(LLF(alpha2,beta2,0))}
TwoDLLF(0.75,0.68)
x=c(0.0275,0.97)
TVDLLF = function(x){LLF(alpha = x[1], beta = x[2], 0)}
TVDLLF(x)/2



TVDLLF(c(0.0001,0.1))
s3
optGC = optim(c(0.5,0.5),TVDLLF)
optGC
s3 = optGC$par
s2
s1

dataG = coredata(eret)
plot(dataG,type='l')
fittedGarch = garch(dataG,grad=c('numeric'),start=c(0.01,0.1,0.6))
fittedGarch$n.likeli*2
coef(fittedGarch)
summary(fittedGarch)
x11()
plot(fittedGarch,type='l')
plot(fittedGarch$fitted.values[,1],type='l')
fGval=fittedGarch$fitted.values[,1]
length(fGval)
fGval = zoo(fGval,order.by = index(eret[100:length(eret),]))

fGfit = garchFit(data = coredata(eret), cond.dist='std',include.mean=T)
plot(fGfit@sigma.t, type="l")
coef(fGfit)
#plot(fGfit)

pd = predict(fGfit)
plot(sqrt(252)*pd[,3],type='l')

##Make Grid of results
#Takes a little while to run, so comment out in interactive
library('lattice')
library('reshape2')
library('stats')
alphaSeq = seq(0,1,0.05)
betaSeq = seq(0,1,0.05)
#Gen grid of inputs
plotData = data.frame(expand.grid(alphaSeq,betaSeq))
#Eval function on inputs
#Next line takes a little while to run, so comment out by default
z = apply(plotData,1, function(x){TwoDLLF(alpha=x[1],beta=x[2])})
plotData['LLF']=z
colnames(plotData)=c('alpha','beta','LLF')
#Fremove infs
infs = which(plotData[,1]==0)
plotData = plotData[-infs,]
#Do 3d plot
wireframe(LLF~alpha*beta,data=plotData,drape = TRUE, colorkey = TRUE)
#Make HeatMap of results
meltdePD = melt(plotData,id.vars=c('alpha','beta'))
gridVals = acast(meltdePD,alpha~beta)
which(gridVals[4,18] == min(gridVals))
(344/21-16)*21

gridVals[2:6,15:20]
data3d= gridVals[0:6,12:21]
melt3D = melt(data3d)
wireframe(value~Var1*Var2,data=melt3D,drape = TRUE, colorkey = TRUE)
