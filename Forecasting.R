library(stats)

#Let's try forecasting AUDUSD rate and evaluate predictions

#First let's fit arma model using Box-Jenkins



#Check levels to see if stationary
acf(AUDUSD)
pacf(AUDUSD)

#acf says nope - could do Dickey Fuller, but let's try this

#Let's difference - should be stationary now
AUDUSD
diffAUDUSD = diff(AUDUSD)
diffAUDUSD = diffAUDUSD[2:length(diffAUDUSD),]

#Split into training and test
n = 30
leaveout = n
train = diffAUDUSD[1:(length(diffAUDUSD)-(n+1)),]
test = diffAUDUSD[(length(diffAUDUSD)-n):length(diffAUDUSD),]

#Check form
acf(train)
pacf(train)
#Looks like AR1

mod1 = arma(train,order=c(3,0))
summary(mod1)


#All have pretty shite t-values, not looking good but let's
#see how we go

#Checking ACF and PACF seems like theres no serial correlation in
#resids
plot(mod1)

mod1$coef




#Check if resids are normal
jarque.bera.test(mod1$residuals[-1])
hist(mod1$residuals[-1],breaks=30)

#Try testing forecast vs test

modStats = arima(train, order=c(3,0,0))
print(modStats)
modStats$arma
summary(modStats)
pred = predict(modStats,n.ahead=1)

predError = coredata(test) - pred$pred
test

predError/ pred$se
plot(train)
plot(test)
plot(coredata(test),type='l')
lines(coredata(pred$pred), col=2)

plot(diffAUDUSD)
par(mfrow=c(1,1))


#We want to fit model then make one step ahead forecast using
#fitted model for every period


#train

#predict
