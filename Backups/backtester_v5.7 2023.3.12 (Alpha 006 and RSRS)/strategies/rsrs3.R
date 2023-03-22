

maxRows <- 3100

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  stopLossLevel <- allzero
  #Iterate through the series in params$series
  for (i in 1:length(params$series)){ 
    
    #start from the 19th day
     if(store$iter > params$lookback){   
       
      #Get every stock's volume and closed price data
      High = store$high[,i]
      #print(High)
      Low = store$low[,i]
      #print(High)

      startIndex <- store$iter - params$lookback
      
      for (j in startIndex:store$iter){
        
       HighList <- High[startIndex:(store$iter-1)]
       LowList <- Low[startIndex:(store$iter-1)]
       
      # print(class(HighList))

      #OLS regression
       fit=lm(HighList~LowList) 
      # print(fit)
       rsrs <- as.numeric(fit$coefficients[2])
      # print(paste("rsrs =",rsrs))

       if (rsrs < 0.8){
         pos[params$series[i]] <- -params$posSizes[params$series[i]]
         stopLossLevel[params$series[i]] <- Low[store$iter] * (1 - 50/100)
       }
       else if (rsrs > 1.1){
         pos[params$series[i]] <- params$posSizes[params$series[i]]
         stopLossLevel[params$series[i]] <- High[store$iter] * (1 + 50/100)
       }
       else{
         pos[params$series[i]] <- 0
       }
       # check if stop loss is triggered
       if ((pos[params$series[i]] > 0) & (Low[store$iter] <= stopLossLevel[params$series[i]])) {
         pos[params$series[i]] <- -currentPos[params$series[i]] # close the long position
       }
       if ((pos[params$series[i]] < 0) & (High[store$iter] >= stopLossLevel[params$series[i]])) {
         pos[params$series[i]] <- -currentPos[params$series[i]] # close the short position
       }

      }
  
    }
  }
  
#Update market orders
marketOrders <- marketOrders + pos

return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
    
  
  
  
  ###############################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
###############################################################################

# functions for managing the store
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

# functions for managing the High
initHigh  <- function(newRowList,series) {
  high <- matrix(0,nrow=maxRows,ncol=length(series))
  return(high)
}
updateHigh <- function(high, newRowList, series, iter) {
  for (i in 1:length(series))
    high[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(high)
}

#functions for managing the Low
initLow  <- function(newRowList,series) {
  low <- matrix(0,nrow=maxRows,ncol=length(series))
  return(low)
}
updateLow <- function(low, newRowList, series, iter) {
  for (i in 1:length(series))
    low[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(low)
}

#
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
                     high=initHigh(newRowList,series),
                     low=initLow(newRowList,series) ))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  return(store)
}