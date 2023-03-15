
# Stock <-  lapply(Stockall, function(x) x[1:500])
# StockDates <- as.Date(Stock$Index)
# StockTS <- ts(Stock$Open, start=0, frequency=30)
# plot.ts(StockTS)
# model<-HoltWinters(StockTS,gamma=FALSE,l.start=8227,b.start= -197)
# plot(model)
# library(forecast)
# premodel<-forecast(model, h=10)  
# plot(premodel)
#install.packages("trend")
#install.packages("cpm") #初次使用需安装，以后就不需要了
library(cpm)
library(trend)

Stockall <-read.csv("/Users/zyx/backtester_v5.7/DATA/PART2/05.csv",header=TRUE, sep=",", dec=".", fileEncoding="UTF-8-BOM")
# Stock <-  lapply(Stockall, function(x) x[1:500])
# mk.test(Stockall$Close)
# par(mfrow=c(2,1))
#plot(Stockall$Close, type="l", lwd="3", col="steelblue", ylim=c(10,50), xlab="day", ylab="close")
# shapiro.test(Stockall$Close) #检验数据是否服从高斯分布，发现不服从。所以选择一个非高斯分布的方法使用
# #        Shapiro-Wilk normality test
# 
# # data:  df$y
# # W = 0.89808, p-value = 1.899e-10
# 
# cpm.res = processStream(Stockall$Close, cpmType = "Kolmogorov-Smirnov")
# # 可视化变点
# plot(Stockall$Close, type="l", lwd="3", col="steelblue", ylim=c(10,50), xlab="day", ylab="close")
# abline(v = cpm.res$changePoints, lwd = 1.5, col = "red")
# # 变点坐标信息提取
# print(cpm.res$changePoints)
# diffcl <- Delt(Stockall$Close,type = ("arithmetic"))
# diffcl[which(abs(diffcl)>0.1,)]
# plot(diffcl)
# p <- sqrt(252) * sd(diff(log(Stockall$Close))) * 100
# print(p)
# 
# fit <- lm(Close~Close,data = Stockall)
# summary(fit)
# fitted(fit)
# residuals(fit)
# plot(Stockall$Close,Stockall$Close,xlab = "Close",ylab = "Close")
# abline(fit,lty=2,col = "red")

stock <- xts(Stockall[,-1],order.by = as.Date(Stockall[,1]))

sseVol = aggregate(stock$Close, as.numeric(format(index(stock$Close), "%Y")),
                   function(ss) coredata(tail(TTR:::volatility(
                     ss,
                     n=NROW(ss),
                     calc="close"), 1)))
cat("Series 05:", sseVol, '\n')

