library('quantmod')
library('Quandl')

getFX('USD/AUD',from = Sys.Date()-499)
getFX('EUR/USD',from = Sys.Date()-499)
getFX('USD/JPY',from = Sys.Date()-499)
getFX('USD/GBP',from = Sys.Date()-499)
getFX('USD/CHF',from = Sys.Date()-499)

Quandl.auth('ai3HEfkXjxkuhLzdn2n8')
start = as.character(Sys.Date()-499)
end = as.character(Sys.Date())

AUDUSDFutures = Quandl("OFDP/FUTURE_AD1",type='xts',start_date=start,end_date=end)
EURUSDFutures = Quandl("OFDP/FUTURE_EC1",type='xts',start_date=start,end_date=end)
JPYUSDFutures = Quandl("OFDP/FUTURE_JY1",type='xts',start_date=start,end_date=end)
GBPUSDFutures = Quandl("OFDP/FUTURE_BP1",type='xts',start_date=start,end_date=end)
CHFUSDFutures = Quandl("OFDP/FUTURE_SF1",type='xts',start_date=start,end_date=end)

AUDUSD = 1/USDAUD
colnames(AUDUSD)='AUD.USD'
JPYUSD = 1/USDJPY
colnames(JPYUSD)='JPY.USD'
GBPUSD = 1/USDGBP
colnames(GBPUSD)='GBP.USD'
CHFUSD = 1/USDCHF
colnames(CHFUSD)='CHF.USD'

AUDFXandFutures = merge(AUDUSD, AUDUSDFutures,all=F)

colnames(AUDFXandFutures)
plot(as.zoo(AUDFXandFutures[,c('AUD.USD','Open')]))
spread = (AUDFXandFutures[,'Open']-AUDFXandFutures[,'AUD.USD'])
plot(spread)
Sys.Date()





head(USDAUD)
plot(1/USDAUD)

write.zoo(AUDUSD,'AUDUSD.csv',index.name='Date',sep=',')
write.zoo(JPYUSD,'JPYUSD.csv',index.name='Date',sep=',')
write.zoo(EURUSD,'EURUSD.csv',index.name='Date',sep=',')
write.zoo(GBPUSD,'GBPUSD.csv',index.name='Date',sep=',')
write.zoo(CHFUSD,'CHFUSD.csv',index.name='Date',sep=',')

fxSeries = merge(AUDUSD,JPYUSD,EURUSD,GBPUSD,CHFUSD)
head(fxSeries)
write.zoo(fxSeries,'AllFXSeries.csv',index.name='Data',sep=',')
dfFX = as.data.frame(fxSeries)
plot(dfFX)

cor(fxSeries)
plot(as.zoo(fxSeries))
tail(fxSeries)
