maxRows <- 3100
getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  #main strategy logic
  #Iterate through the series in params$series
  
  #rsrs  <- c()
 #start from the 318th day
if(store$iter > params$lookback_m + params$lookback){   
  for (i in 1:length(params$series)){ 
   
      #Get every stock's high and low price data
      High = store$high[,i]
      Low = store$low[,i]
      
     # calculate Nth day's RSRS
      startIndex <- store$iter - params$lookback #params$lookback=10, start from 301th day   
      rsrs_n <- Cal_RSRS(High, Low, startIndex,store$iter)
      #print(rsrs_n)
    
     # calculate previous M days RSRS
      startIndex_m <- store$iter - params$lookback_m #start from 11th day
      rsrs_m <- Cal_RSRS_M(High, Low, startIndex_m, params$lookback, store$iter)
      

      #z-score rsrs
      rsrs_z <- (rsrs_n - mean(rsrs_m))/sd(rsrs_m)
      #rsrs <- c(rsrs, rsrs_z)
      
      
      #decide if we should go long/short/flat (returning 1/-1/0)
       if (rsrs_z < 0.7){
         pos[params$series[i]] <- -params$posSizes[params$series[i]]
       }
       else if (rsrs_z > 0.7){
         pos[params$series[i]] <- params$posSizes[params$series[i]]
       }
       else{
         pos[params$series[i]] <- 0
       }

 
    } 
    
   
  }
  
  
  #rsrs

#Update market orders
marketOrders <- -currentPos + pos


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

Cal_RSRS <- function(High, Low, startIndex, iter) 
  for (j in startIndex:iter){
    HighList <- High[startIndex:(iter-1)]
    LowList <- Low[startIndex:(iter-1)]       
    #OLS linear regression
    fit=lm(HighList~LowList) 
    nrsrs <- as.numeric(fit$coefficients[2])
    return(nrsrs)
}

Cal_RSRS_M <- function(High, Low, startIndex,lookback,iter){
  mrsrs = c()
  for (i in startIndex:iter){ #定义i循环10?????310
    HighList <- High[(i-lookback):i]
    LowList <- Low[(i-lookback):i]       
    #OLS linear regression
    fit=lm(HighList~LowList) 
    mrsrs=c(mrsrs,as.numeric(fit$coefficients[2]))#所有rsrs集合为向?????
    #print(mrsrs)
}
  return(mrsrs)
}