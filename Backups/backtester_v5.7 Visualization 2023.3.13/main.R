source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# load data
dataList <- getData(directory="PART1")
#dataList <- lapply(dataList, function(x) x[0:500])
# choose strategy from example_strategies
strategy <- "combination2" 

# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
  strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

#Initialize the threshold
thr006Vector = 40
thr018 = -100
thr034 = 200

x <- c()
y <- c()

# split data in two (e.g. for in/out test)
# numDays <- nrow(dataList[[1]])
# inSampDays <- 500
# 
# # in-sample period
# dataList <- lapply(dataList, function(x) x[1:inSampDays])
# 
# # out-of-sample period
# dataList <- lapply(dataList, function(x) 
#                                x[(inSampDays+1):numDays])

sMult <- 0.20 # slippage multiplier

#Loop through the threshold
# for (i in thr006Vector){
#   thr006 = i
#   results <- backtest(dataList,getOrders,params,sMult)
#   pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
# 
#   x <- append(x, thr006)
#   y <- append(y, pfolioPnL$fitAgg)
# 
#   print(pfolioPnL$fitAgg)
# }


########################################################
strategyMatrix <- matrix(ncol = 10)
runningDays <- 1000
date <- vector()
currentPosition <- vector()
#dateList <- c()
#########################################################



#Store the threshold and its performance into a dataframe
#Output the data
results <- backtest(dataList,getOrders,params,sMult)
#print(results)
plotResults(dataList,results)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
#cat("Profit:", results$aggProfit, '\n')
# # Mydata <- data.frame(x,y)
# # write.csv(Mydata, file = "Mydata.csv", row.names = FALSE)
# 
# 
# opens <- sapply(dataList,function(x) first(x)$Open)
# largestOpen <- max(opens)
# #positionSizes <- round(largestOpen/opens)
# # print(list(sizes = positionSizes))
# # print(positionSizes)
#   
# openDiffs <- lapply(dataList,function(x) diff(x$Open))
# absOpenDiffs    <- lapply(openDiffs,abs)
# avgAbsDiffs <- sapply(absOpenDiffs, function(x) mean(x[x > 0]))
# #print(absOpenDiffs)
# largestAvgAbsDiffs <- max(avgAbsDiffs)
# positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
# params<- list(sizes=positionSizes)
# print(params)
# # 
