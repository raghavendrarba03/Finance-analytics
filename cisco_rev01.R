#choose necessary code for compiling
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
library(astsa)
library(ggplot2)
library(forecast)
library(tseries)
library(xts)
library(zoo)
library(scales)
library(rlang)

# Read the Dataset
path<- file.path("C:\\Users\\Raghavendra Reddy\\Desktop\\Finance analytics\\Assignment - 1\\dataset_cisco.csv")
cisco<- read.csv(path, stringsAsFactors = FALSE)

#Explore the dataset
head(cisco, n=10)
tail(cisco)
str(cisco)
class(cisco)

#convert dataf.frame to time series data
cisco.ts <- ts(cisco, frequency = 4, start = c(2009,4), end = c(2016,1))
head(cisco.ts)
tail(cisco.ts)
class(cisco.ts)
summary(cisco.ts)

#summary of time stamps
cycle(cisco.ts)
start(cisco.ts)
end(cisco.ts)

#check for missing values
sum(is.na(cisco.ts))
cisco.tsc<- cisco.ts[,2:22]# drop the column having missing value.
head(cisco.tsc)# visualise
sum(is.na(cisco.tsc))# check again for missing values.

#Plot and check is there any trend
plot(cisco.tsc[,21])
abline(reg=lm(cisco.tsc[,21]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,21]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,21], alternative='stationary', k=3)# since p>0.05, series is not stationary


#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,21])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,21]))
acf(diff(log(cisco.tsc[,21])))
pacf(diff(log(cisco.tsc[,21])))


(fit <- arima(log((cisco.tsc[,21])), c(0, 1, 0),seasonal = list(order = c(0, 1, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,21],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For produc sales#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,1])
abline(reg=lm(cisco.tsc[,1]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,1]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,1], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,1])
summary(model1)


#plot ACF and PACF
acf(log(cisco.tsc[,1]))
acf(diff(log(cisco.tsc[,1])))
pacf(diff(log(cisco.tsc[,1])))


(fit <- arima(log((cisco.tsc[,1])), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,2],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For service sales#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,2])
abline(reg=lm(cisco.tsc[,2]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,2]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,2], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,2])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,2]))
acf(diff(log(cisco.tsc[,2])))
pacf(diff(log(cisco.tsc[,2])))


(fit <- arima(log((cisco.tsc[,2])), c(2, 2, 0),seasonal = list(order = c(2, 2, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,2],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For cost product#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,4])
abline(reg=lm(cisco.tsc[,4]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,4]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,4], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,4])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,4]))
acf(diff(log(cisco.tsc[,4])))
pacf(diff(log(cisco.tsc[,4])))


(fit <- arima(log((cisco.tsc[,4])), c(0, 2, 2),seasonal = list(order = c(0, 2, 2), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,4],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For cost service#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,5])
abline(reg=lm(cisco.tsc[,5]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,5]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,5], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,5])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,5]))
acf(diff(log(cisco.tsc[,5])))
pacf(diff(log(cisco.tsc[,5])))


(fit <- arima(log((cisco.tsc[,5])), c(0, 1, 0),seasonal = list(order = c(0, 1, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,5],2.718^pred$pred, log = "y", lty = c(1,3))


#######################################For opex R&D#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,8])
abline(reg=lm(cisco.tsc[,8]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,8]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,8], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,8])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,8]))
acf(diff(log(cisco.tsc[,8])))
pacf(diff(log(cisco.tsc[,8])))


(fit <- arima(log((cisco.tsc[,8])), c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,8],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For opex sales and distribution#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,9])
abline(reg=lm(cisco.tsc[,9]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,9]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,9], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,9])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,9]))
acf(diff(log(cisco.tsc[,9])))
pacf(diff(log(cisco.tsc[,9])))


(fit <- arima(log((cisco.tsc[,9])), c(1, 0, 0),seasonal = list(order = c(1, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,9],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For opex general and adminstritative#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,10])
abline(reg=lm(cisco.tsc[,10]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,10]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,10], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,10])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,10]))
acf(diff(log(cisco.tsc[,10])))
pacf(diff(log(cisco.tsc[,10])))


(fit <- arima(log((cisco.tsc[,10])), c(0, 0, 0),seasonal = list(order = c(0, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,10],2.718^pred$pred, log = "y", lty = c(1,3))

#######################################For opex amortization#########################################################
#Plot and check is there any trend
plot(cisco.tsc[,11])
abline(reg=lm(cisco.tsc[,11]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,11]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,11], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,11])
summary(model1)

#plot ACF and PACF
acf(log(cisco.tsc[,11]))
acf(diff(log(cisco.tsc[,11])))
pacf(diff(log(cisco.tsc[,11])))


(fit <- arima(log((cisco.tsc[,11])), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,11],2.718^pred$pred, log = "y", lty = c(1,3))


#######################################For opex restructuring and other charges########################################################
#Plot and check is there any trend
plot(cisco.tsc[,12])
abline(reg=lm(cisco.tsc[,12]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,12]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,12], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,12])
summary(model1)


(fit <- arima(log((cisco.tsc[,12])), c(0, 0, 0),seasonal = list(order = c(0, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,12],2.718^pred$pred, log = "y", lty = c(1,3))



#######################################For interest income########################################################
#Plot and check is there any trend
plot(cisco.tsc[,15])
abline(reg=lm(cisco.tsc[,15]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,15]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,15], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,15])
summary(model1)


(fit <- arima(log((cisco.tsc[,15])), c(1, 2, 0),seasonal = list(order = c(1, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,15],2.718^pred$pred, log = "y", lty = c(1,3))


#######################################For interest expense########################################################
#Plot and check is there any trend
plot(cisco.tsc[,16])
abline(reg=lm(cisco.tsc[,16]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,16]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,16], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,16])
summary(model1)


(fit <- arima(log((cisco.tsc[,16])), c(2, 0, 0),seasonal = list(order = c(0, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,16],2.718^pred$pred, log = "y", lty = c(1,3))


#######################################For income other########################################################
#Plot and check is there any trend
plot(cisco.tsc[,17])
abline(reg=lm(cisco.tsc[,17]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,17]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,17], alternative='stationary', k=3)# since p>0.05, series is not stationary

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,17])
summary(model1)


(fit <- arima(log((cisco.tsc[,17])), c(0, 0, 0),seasonal = list(order = c(0, 0, 1), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
2.718^pred$pred
ts.plot(cisco.tsc[,16],2.718^pred$pred, log = "y", lty = c(1,3))


#######################################For income tax provision########################################################
#Plot and check is there any trend
plot(cisco.tsc[,20])
abline(reg=lm(cisco.tsc[,20]~time(cisco.tsc)))

#Box plots of the time series.
boxplot(cisco.tsc[,20]~cycle(cisco.tsc),xlab="Year", ylab="$ million")

#test stationarity with Time series Augumented Dickey-fuller test.
adf.test(cisco.tsc[,20], alternative='stationary', k=3)# since p>0.05, series is not stationary

#library(Metrics)


#train test split.
#train = cisco.tsc[1:20,20]
#valid = cisco.tsc[21: nrow(cisco.tsc),20]

#train
#valid

#fit ARIMA Model
#model1<- auto.arima(train)
#summary(model1)


#forecast
#forecast = predict(model1,6)
#forecast

#fit ARIMA Model
model1<- auto.arima(cisco.tsc[,20])
summary(model1)

(fit <- arima(log((cisco.tsc[,20])), c(0, 0, 0),seasonal = list(order = c(0, 0, 0), period = 4)))
pred <- predict(fit, n.ahead = 1*4)
pred1<-2.718^pred$pred
pred1
ts.plot(cisco.tsc[,20],2.718^pred$pred, log = "y", lty = c(1,3))

#evaluation
#rmse(valid[cisco.tsc[,20]], forecast$pred)

