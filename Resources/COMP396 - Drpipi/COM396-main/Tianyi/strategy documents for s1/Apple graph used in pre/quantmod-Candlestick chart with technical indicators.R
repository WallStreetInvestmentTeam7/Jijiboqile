library(TTR)
library(zoo)
library(xts)
library(quantmod)

getSymbols("AAPL")
chartSeries(AAPL,TA=c(addCCI(),addMACD()),subset='last 3 months',theme=chartTheme('white'))
