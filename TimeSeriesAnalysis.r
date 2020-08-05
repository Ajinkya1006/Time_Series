

library(TSA)
data_ts <- read.table("D:/predective_analysis/assignment/extra data/Souvenir.txt",header=TRUE)
data_ts <- as.data.frame(data_ts)
periodogram(data_ts)
data.ts <- ts(data_ts,frequency = 12,start = c(2007,4))
plot.ts(data.ts,main="TS",col="blue")
abline(reg=lm(data.ts~time(data.ts)))
library(TTR)
decomp <- decompose(data.ts)
plot(decomp)
library(forecast)
fit <- hw(data.ts,seasonal="additive") #seasonality component is same throughout timeseries

fit$model     #AIC for the model is 1848.568
fit$fitted
accuracy(fit)  #RSME of model is 5353.944
forecast(fit,48)
plot(forecast(fit,48))
lines(fitted(fit),col="red")
Box.test(resid(fit),type = "Ljung-Box")
#as the p0value is very small , null hypothesis is regected. model does not show lack of fit
#arima model
#adf test to check whether the data is stationary or non-stationary
adf.test(data.ts) 
#as p-value is higher, data seems to be non-stationary
#using differecing,making data stationary
data_2 <- diff(data.ts,differences = 5)
plot(data_2)
#data looks stationary now, d value is selected as 5
#finding MA and AR value
acf(data.ts,lag.max = 10) #it seems that decay in acf plot is slow as data is stationary.
pacf(data.ts,lag.max = 10)#it seems that pacf plot cuts off after 5th lag


acf(data_2,lag.max = 10)
pacf(data_2,lag.max = 10)

#fitting Arima model using p=5,d=5 and q=5 and adding seasonality component

fit1 <- Arima(data.ts,order = c(5,5,5),seasonal=list(order=c(0,1,1)),method = "ML")
fit1$aic  #AIC of model is 1407.08
accuracy(fit1)  #RSME of model is 4519.776
pred <- predict(fit1,n.ahead = 4*12)
ts.plot(data.ts,pred$pred,log="y",lty=c(1,3))
