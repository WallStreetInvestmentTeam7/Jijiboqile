############################################################################
# This file is setup to use:
# - the data for assignment 2 (a2)
# - the a2_strategy_template
# You can use this file to test a single parameter combination
# (for which you should set params, start_period and end_period as required)
# In particular you can use this file to test your implementation of getOrders 
# (when you have done it) with the examples in a2.pdf
# For creating results.yaml, see the hints in a2.pdf
############################################################################
source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')
source('framework/utilities.R')
# Read in data -- here with the A2 direction; subset it as required
dataList <- getData(directory="PART1")
# subset data: choose the period to run on 
#dataList <- lapply(dataList, function(x) x[1:200])

# Choose strategy -- this should be called strategy.R when you submit it
strategyFile <- 'strategies/dmaMACD.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# Strategy parameters -- this will not be an empty list when you are done
#params <- list()

#ins_period <- ID$startIn:ID$endIn #in-sample period
#out_period <- ID$startOut:ID$endOut #out-sample period
dataList_ins<- lapply(dataList, function(x) x[1:500])
datalist_out <- lapply(dataList, function(x) x[501:1000])

#set parameters
short_lookback <- c(5,10)
medium_lookback <- c(50,60)


# all_combine <- function(vec){
#   vec <- sort(vec)
#   out_list <- list()
#   index <- 1
#   for(i in 2:(length(vec))){
#     sub_com <- combn(vec, i)
#     for(j in (1:ncol(sub_com))){
#       out_list[[index]] <- sub_com[, j]
#       index = index +1
#     }
#   }
#   return(out_list)
# }
# series_l <- all_combine(7)
# series_l
params_lookback <-expand.grid(short=short_lookback,medium=medium_lookback,series= c(1,7))

for ( i in 1:nrow(params_lookback)) {
  ishort <- as.integer(params_lookback[i,][1])
  ilong <- as.integer(params_lookback[i,][2])
  #ilong <- as.integer(params_lookback[i,][3])
  lookbacks <- list(short = ishort,long = ilong)

  iseries <- as.data.frame(params_lookback$series[i])

  s <- c()

  for (j in 1:nrow(iseries)) {
  
  s <- append( s , iseries[j,] )

  }

  params <- list(lookbacks=lookbacks)
  print("Parameters:")
  print(params)
  #
  cat("Profit:", results$aggProfit, '\n')
}

# cat("Profit:", results$aggProfit, '\n')


