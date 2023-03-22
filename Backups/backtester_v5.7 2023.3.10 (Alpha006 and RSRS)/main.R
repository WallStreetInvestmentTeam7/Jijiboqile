source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# load data
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:500])
#choose strategy from example_strategies
strategy <- "combination"

# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
    strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

# split data in two (e.g. for in/out test)
# numDays <- nrow(dataList[[1]])
# inSampDays <- 550
# 
# # in-sample period
# dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
#dataList <- lapply(dataList, function(x) 
                               #x[(inSampDays+1):numDays])
# dataList_ins<- lapply(dataList, function(x) x[1:500])
# datalist_out <- lapply(dataList, function(x) x[501:1000])

sMult <- 0.20 # slippage multiplier

results <- backtest(dataList,getOrders,params,sMult)
#print(results) 
plotResults(dataList,results)
#pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
#cat("Profit:", results$aggProfit, '\n')
# print(params)

openDiffs <- lapply(dataList,function(x) diff(x$Close))

toPlot <- do.call(cbind,openDiffs)
colnames(toPlot) <- paste("Series",sprintf("%02d",1:10))

# plot.zoo(toPlot,
#          main="Open on open simple differences",
#          cex.axis=1.2,
#          cex.main=2,
#          yax.flip=TRUE)
# dev.copy(pdf,file.path(path,"opendiffs.pdf"))
# dev.off()
# 
# 
# absOpenDiffs    <- lapply(openDiffs,abs)
# avgAbsDiffs <- sapply(absOpenDiffs,mean,na.rm=TRUE)
# opensOnFirstDay <- sapply(dataList,function(x) first(x)$Open)
# 
# tab <- cbind(opensOnFirstDay,
#              avgAbsDiffs,
#              abs(avgAbsDiffs)/opensOnFirstDay)
# colnames(tab) <- c("Open","Mean abs diff","Mean abs diff/Open")
# 
# print(tab)
opens <- sapply(dataList,function(x) first(x)$Open)
largestOpen <- max(opens)
positionSizes <- round(largestOpen/opens)
print(list(sizes = positionSizes))
print(positionSizes)
print(opens)
is.list(opens)

# open_prices_xts <- xts(c(23.25, 23.30, 22.90), order.by = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")))
# colnames(open_prices_xts) <- "Open"
# print(open_prices_xts)
# # extract open prices and select max open price
# max_open_price <- max(coredata(open_prices_xts)[,1])
# 
# # print max open price
# print(max_open_price)


print("over")

