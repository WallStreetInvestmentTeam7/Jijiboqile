library(quantmod)

rm(list = ls())
setwd("/Users/zyx/backtester_v5.7/DATA/PART2")
price <- as.xts(read.zoo("03.csv",header=TRUE,sep=",",colClasses = c("Date", rep("numeric",5))))

n <- nrow(price)
m <- nrow(price)-1000

#pdf(file = "k.pdf")
chartSeries(price[c(m:n)],theme = chartTheme("white"),
            up.col = "red",dn.col = "green",name = "series_07",
            time.scale = 0.5,line.type = "l",bar.type = "ohlc",
            major.ticks='auto', minor.ticks=TRUE)
#dev.off()
chartSeries(price[c(m:n)],theme = chartTheme("white"),TA = "addSMA(n=5);addSMA(n=50);addMACD(type = 'EMA')")

getSymbols("AMD", from="2005-11-01", to="2006-08-01")
print(AMD)
AMD$stopLongATR <- -3.5*ATR(HLC(AMD),5)[,"atr"]
print(AMD$stopLongATR)
AMD$stopShortATR <- 3.5*ATR(HLC(AMD),5)[,"atr"]

chartSeries(AMD, TA="addTA(runMax(Cl(AMD)+AMD$stopLongATR,10), on=1);
addTA(runMin(Cl(AMD)+AMD$stopShortATR,10), on=1)")
AMD$trail <- 0
AMD$AMD.lagCl <- lag(Cl(AMD))
print(AMD$AMD.lagCl )
for(i in 6:NROW(AMD)) {
  trail1 <- coredata(AMD$trail[i-1])
  
  if(Cl(AMD)[i] > trail1 && AMD$AMD.lagCl[i] > trail1) {
    AMD$trail[i] <- max(trail1,coredata(Cl(AMD)[i]+AMD$stopLongATR[i]))
  } else
    if(Cl(AMD)[i] < trail1 && AMD$AMD.lagCl[i] < trail1) {
      AMD$trail[i] <- min(trail1,coredata(Cl(AMD)[i]+AMD$stopShortATR[i]))
    } else
      if(Cl(AMD)[i] > trail1) {
        AMD$trail[i] <- coredata(Cl(AMD)[i]+AMD$stopLongATR[i])
      } else {
        AMD$trail[i] <- coredata(Cl(AMD)[i]+AMD$stopShortATR[i])
      }
}

chartSeries(AMD, TA = "addTA(AMD$trail, on=1)")



