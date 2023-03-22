source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); 

#################################################################
# To change things:
# comment out the current choice and uncomment a different choice
#################################################################

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# BACKTEST PARAMETERS
numOfDays <- 500 # don't use all available days to start with!
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])
sMult <- 0.20 # slippage multiplier

################################################################

#################################################################
# PLOTS FOR BUY AND HOLD
#################################################################
strategyFile <-'strategies/fixed.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# buy and hold 1
params<- list(sizes=rep(1,10)) # position size of 1 for all series

backtestAndPlot(path='images',
                filename='buy_and_hold_one_chartSeries',
                plotType='chartSeries')

backtestAndPlot(path='images',
                filename='buy_and_hold_one_ggplot2',
                plotType='ggplot2')

#################################################################
# PLOT FOR INVERSELY PROPORTIONAL POSITION SIZING
#################################################################

closes <- sapply(dataList,function(x) x[1,"Close"])
largestClose <- max(closes)
positionSizes <- round(largestClose/closes)
params<- list(sizes=positionSizes) # inversely proportional to starting close

backtestAndPlot(path='images',
                filename='buy_and_hold_inversely_prop')

#################################################################
# PLOTS FOR bbands_contrarian
#################################################################

strategyFile <-'strategies/bbands_contrarian.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

params<- list(lookback=10,sdParam=1.25,series=1:5,posSizes=rep(1,10))

backtestAndPlot(path='images',
                filename='bbands_one')

params<- list(lookback=10,sdParam=1.25,series=1:5,posSizes=positionSizes)

backtestAndPlot(path='images',
                filename='bbands_inversely_prop')
