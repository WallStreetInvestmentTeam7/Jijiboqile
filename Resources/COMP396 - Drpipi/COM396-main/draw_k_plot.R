library(quantmod)

rm(list = ls())
setwd("D:/RStudio/backtester_v5.6/DATA/PART1")
price <- as.xts(read.zoo("10.csv",header=TRUE,sep=",",colClasses = c("Date", rep("numeric",5))))

n <- nrow(price)
m <- nrow(price)-1000

#pdf(file = "k.pdf")
chartSeries(price[c(m:n)],theme = chartTheme("white"),up.col = "red",dn.col = "green",name = "10k_plot",time.scale = 0.5,line.type = "l",bar.type = "ohlc",major.ticks='auto', minor.ticks=TRUE)
#dev.off()
