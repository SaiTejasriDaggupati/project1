install.packages("Metrics") 
library(Metrics)
install.packages("MLmetrics")
library(MLmetrics)
install.packages("forecast")
library("forecast")
install.packages("tseries")
library(tseries)
install.packages("TSPred")
library(TSPred)

data <- read.csv("C:\\Users\\user\\OneDrive\\Desktop\\female birth rate.csv")
class(data)
summary(data)

mdata <- ts(data = data[,2],start = 1959 , frequency = 365)
class(mdata)
plot(mdata)

train <- ts(data[1:292,2],start = 1959 , frequency = 365)
test <- ts(data[293:365,2],start = 1959 , frequency = 365)

ddata<-diff(train)
ddata
plot.ts(ddata)
class(ddata)

d2data<-diff(ddata)
d2data
plot.ts(d2data)
class(d2data)

adf.test(mdata)
adf.test(train)
adf.test(ddata) #data is stationary
adf.test(d2data)


acf(train)
pacf(train,lag = 20)

acf(ddata)
pacf(ddata,lag = 20)

acf(d2data)#1,2
pacf(d2data,lag = 20)

length(test)


auto.arima(ddata)#0,0,1
auto.arima(train)#0,1,1
auto.arima(d2data)#5,0,0

m1<- arima(train,order = c(5,0,0))#2001.87
m1
m2<- arima(train,order = c(0,0,1))#1984.4.56
m2
m3<- arima(train,order = c(0,1,1))#1975.71 aic less
m3

fit <- predict(m1,73)
fit

fit2 <- predict(m2,73)
fit2

fit3<- predict(m3,73)
fit3



MAPE(data$Births[293:365],fit$pred)#12.461
MAPE(data$Births[293:365],fit2$pred)#15.88
MAPE(data$Births[293:365],fit3$pred)#12.464

fit4<- predict(m1,103)
fit4

tail(fit4$pred,30)

ts.plot(data$Births[293:365],fit$pred, lty = c(1,3), col=c(5,2))








