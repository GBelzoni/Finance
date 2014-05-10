library(dlm)
library(ggplot2)
library(xts)
library(reshape2)


mod1 = dlm(FF = 1, V = 0.8, GG = 1, W = 0.1, m0 = 0, C0 = 100)
summary(mod1)
  plot(mod1)

write.table(as.data.frame(as.xts(Nile)),'Nile.csv',sep=',')

togdata = function(data){
  #converst to format good for ggplot2
  result =data.frame(coredata(data))
  if(dimcolnames(result)=colnames(data)
  result['time']=index(data)
  result = melt(result, id='time')
  return(result)
}

data = as.zoo(Nile)
qplot(index(data),coredata(data),geom='line')
#x is state var, y is obseved var


#observations
z = coredata(data)

#KF by hand - univariate
  
  #F - state predict
  #B - control matrix
  #Q - var of from linear predict
  #H - state to observation matrix
  #R - state to observation error
  
  #Initial parameters - take state =0, but with large uncertainty = 100 in e.g.
  #This way Kalman gain starts off near I as so biased towards measurement, then adjusts accordingly
  


#vars for state
F=0.99688 #ar1
B=0
Q = exp(-67.8661434) #var(Nile) #var of the error in the state
fit$par

#vars of observation
H=107
R =  exp(10.00298) #observation measurement error. observation has no error

# #loop generating filtered sig
# x = xts(rep(0,length(z)),order.by=index(z)) #state var - updated
# xp = xts(rep(0,length(z)),order.by=index(z)) #state var - predcited
# P = xts(rep(0,length(z)),order.by=index(z)) #state estimate covariance - updated
# Pp = xts(rep(0,length(z)),order.by=index(z)) #state estimate covariance - predicted
# u = xts(rep(0,length(z)),order.by=index(z)) #controls
# y = xts(rep(0,length(z)),order.by=index(z)) #measurement error 
# S = xts(rep(0,length(z)),order.by=index(z)) #inovation variance
# K = xts(rep(0,length(z)),order.by=index(z)) #Kalman Gain



#loop generating filtered sig
x = rep(0,length(z)) #state var - updated
xp = rep(0,length(z)) #state var - predcited
P = rep(0,length(z)) #state estimate covariance - updated
Pp = rep(0,length(z))#state estimate covariance - predicted
u = rep(0,length(z)) #controls
y = rep(0,length(z)) #measurement error 
S = rep(0,length(z)) #inovation variance
K = rep(0,length(z)) #Kalman Gain



#initial state
x[1]=0
xp[1]=x[1]
P[1] = 100

#loops that does KF
loops=length(x)
for( k in 2:loops){
#predict
xp[k] = F*x[k-1] + B*u[k-1]
Pp[k] = F*P[k-1]*F + Q

#update
y[k] = z[k] - H*xp[k]
S[k] = H*Pp[k]*H + R
K[k] = Pp[k]*H* (1/S[k]) #Kalman Gain
x[k] = xp[k] + K[k]*y[k]
P[k] = (1-K[k]*H)*Pp[k]
#browser()
}

#Analyse results
head(x)
head(y)
result = zoo(H*x[1:loops], order.by=index(data)[1:loops]) #Scale by H to compare below
plot(x,type='l')
plot(xp,type='l')
plot(y,type='l')
plot(z,typ='l')
plot(K)
pdata_short = merge(result,data)
pdata = togdata(pdata_short)
ggplot(data=pdata, aes(x=time, y=value, colour=variable))+ geom_line()



#Do by dlm
#fit a order 1 KF
buildfun = function(x){
  #dlmModPoly(1, dV= exp(x[1]), dW = exp(x[2]) )
   dlm(FF = x[1], V = exp(x[2]) , GG = x[3], W = exp(x[4]), m0 = 0, C0 = 100)
}


fit = dlmMLE(Nile, parm=c(1,0,1,0), build = buildfun)
fit$par
fit$convergence
fit$value

dlmNile = buildfun(fit$par)
dlmNile$C0
par0 = c(107,10,0.996,-1000)
dlmNile = buildfun(par0)
W(dlmNile)
V(dlmNile)
filterNile = dlmFilter(Nile, dlmNile)
filtered = as.zoo(filterNile$f)
filterNile$D.C

index(filtered) = index(pdata_short)
pdata_short2 = merge(pdata_short,filtered)
colnames(pdata_short2)=c('patrick','observed','dlm')
pdata = togdata(pdata_short2)

ggplot(data=pdata, aes(x=time, y=value, colour=variable))+ geom_line()

buildFun <- function(x) {
 m <- dlmModPoly(1, dV = exp(x[1]))
 m$JW <- matrix(1)
 m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
 j <- which(time(Nile) == 1899)
 m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
 return(m)
}

fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun)
fit$conv


dlmNileJump <- buildFun(fit$par)

V(dlmNileJump)
dlmNileJump$X

nileJumpFilt <- dlmFilter(Nile, mod1)

plot(Nile, type = 'o', col = "seagreen")
lines(dropFirst(nileJumpFilt$m), type = 'o', pch = 20, col = "brown")

nileBuild <- function(par) {
  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
}
nileMLE <- dlmMLE(Nile, rep(0,2), nileBuild); nileMLE$conv
nileMod <- nileBuild(nileMLE$par)
V(nileMod)
W(nileMod)
nileFilt <- dlmFilter(Nile, nileMod)
nileSmooth <- dlmSmooth(nileFilt)
plot(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]), plot.type='s',
     col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))

dataKF = as.xts(SP500[,'Adj Close'])
dataKF = dataKF["2013/"]
plot(dataKF)
nlmBuild <- function(par) {
  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
}
nlmSP500 = dlmMLE(dataKF,parm=c(0,0),build=nlmBuild)
nlmSP500$convergence
nlmSP500$par
nlmSP500mod = nlmBuild(nlmSP500$par)
W(nlmSP500mod)
V(nlmSP500mod)
dlmSP500Filt = dlmFilter(as.ts(dataKF),nlmSP500mod)

plot(as.ts(dataKF), type = 'o', col = "seagreen")
lines(dropFirst(dlmSP500Filt$m), type = 'l', pch = 20, col = "brown")