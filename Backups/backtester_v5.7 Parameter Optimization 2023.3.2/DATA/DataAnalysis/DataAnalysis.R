Stock <- read.csv("01.csv", header=TRUE, sep=",", dec=".", fileEncoding="UTF-8-BOM")
StockDates <- as.Date(Stock$Index)



#Step 1 - Plotting the data

#Draw Original Series
par(mfrow=c(1,2))
plot(StockDates, Stock$Open, type="l", xlab="Time", ylab="Open Price")
plot(StockDates, Stock$Close, type="l", xlab="Time", ylab="Close Price")

#Visualising the Data
par(mfrow=c(2,3))
plot(StockDates, Stock$Close, type="l", xlab="Time", ylab="Close Price", main="Original")
boxplot(Stock$Close, main="Boxplot")
library(vioplot)
vioplot(Stock$Close, col="gold", main="Violin plot")
hist(Stock$Close, main="Histogram")
library(MASS)
truehist(Stock$Close, main="Scaled/true histogram and kernel density")
lines(density(Stock$Close),col="red", lwd=3)
library(car)
qqPlot(Stock$Close, main="Quantile-quantile plot")

#Time Series
StockTS <- ts(Stock$Open, start=0, frequency=120)

#The classical decomposition model
plot(stl(StockTS, s.window="periodic"))




#Step 2 - Removing trend and seasonality

par(mfrow=c(2,3))

plot(StockTS) #The original
plot(diff(StockTS), type="l", pch=19) #The first order difference
plot(diff(StockTS, differences=2), type="l", pch=19) #The second order difference

#Plot the corresponding empirical autocorrelation functions
acf(StockTS)
acf(diff(StockTS, lag=52))
acf(diff (diff(StockTS, lag=52)))



#Step 3 - Noise Analysis

library(forecast)

#Fit AR(1), MA(1), ARMA(1,1) models
fit_ar1 <- Arima(StockTS, order=c(1,0,0))
fit_ma1 <- Arima(StockTS, order=c(0,0,1))
fit_arma11 <- Arima(StockTS, order=c(1,0,1))

#Print the parameter estimates
fit_ar1 
fit_ma1 
fit_arma11 

#Assess the model fit
checkresiduals(fit_ar1)
checkresiduals(fit_ma1)
checkresiduals(fit_arma11)



#Additional - Correlation Analysis
Stock <- read.csv("10.csv", header=TRUE, sep=",", dec=".", fileEncoding="UTF-8-BOM")
head(Stock)
#Remove the date index
Stock <- Stock[,-1]
head(Stock)
#Use of function cor()
cor(Stock$Close, Stock$Volume, method='pearson')
#Use of function rcorr()
library(Hmisc)
Stock <- as.matrix(Stock)
rcorr(Stock)
#Visualization
library(PerformanceAnalytics)
chart.Correlation(Stock)




