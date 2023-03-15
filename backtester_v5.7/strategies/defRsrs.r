maxRows <- 3100

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero;limitPos <- allzero
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
  
  #main strategy logic
  #Iterate through the series in params$series
  for (i in 1:length(params$series)){ 
    
    #start from the 311th day
    if(store$iter > params$lookback_m + params$lookback){ 
      #print(store$iter)
      #Get every stock's high and low price data
      High = store$high[,i]
      Low = store$low[,i]
      Open = store$open[,i]
      Close = store$cl[,i]
      opall <- matrix(store$open,ncol = length(params$series))
      openprice <- apply(opall,2, function(x) mean(x[x>0]))
      largestopen <- max(opall)
      #print(round(largestopen/openprice))
      rsrsPos <- round(largestopen/openprice)
      
      # calculate Nth day's RSRS(lookback10)
      startIndex <- store$iter - params$lookback #params$lookback=10, start from 301th day   
      rsrs_n <- Cal_RSRS(High, Low, startIndex,store$iter)
      
      
      # calculate previous M days RSRS
      startIndex_m <- store$iter - params$lookback_m #start from 11th day
      rsrs_m <- Cal_RSRS_M(High, Low, startIndex_m, params$lookback, store$iter)
      #print(sd(rsrs_m))
      #print(rsrs_m)
      
      #z-score rsrs on Nth day
      rsrs_z <- (rsrs_n - mean(rsrs_m))/sd(rsrs_m)
      ATR <- Cal_ATR(High, Low,Close, startIndex, params$lookback,store$iter)
      ATR <- as.numeric(ATR)
      ##print(ATR)
      stop <- ATR*0.8
      #print(stop)
      op <- Open[store$iter]
      mean_p <- (High[store$iter]+Low[store$iter])/2
      
      
      
     
      #decide if we should go long/short/flat (returning 1/-1/0)
      if (rsrs_z > 0.7){         
        pos[params$series[i]] <- round(rsrs_n * rsrsPos[i])/68
        #print(params$posSizes[i])
        # if(as.numeric(op) <= as.numeric(mean_p) - as.numeric(stop)){
        #   limitPos[params$series[i]] <- -pos[params$series[i]]
        #   limitPrices1[i] <- mean_p - stop
        # 
        # }
        
      }
      
      else if (rsrs_z < 0.7){
        pos[params$series[i]] < -round(rsrs_n * rsrsPos[i])/68
        # if(as.numeric(op) >= as.numeric(mean_p) - as.numeric(stop)){
        #   limitPos[params$series[i]] <- pos[params$series[i]]
        #   limitPrices1[i] <- mean_p + stop
        # }
        # # 
      }
      
      else{
        pos[params$series[i]] <- 0
        
        
      }
      
      
    } 
    
  }
  
  
  
  #Update market orders
  marketOrders <- allzero + pos
  #limitOrders1 <- limitOrders1 + limitPos
  
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
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

#functions for managing the Open
initOpenStore  <- function(newRowList,series) {
  openStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(openStore)
}
updateOpenStore  <- function(openStore, newRowList, series, iter) {
  for (i in 1:length(series))
    openStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(openStore)
}


#
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHigh(newRowList,series),
              low=initLow(newRowList,series),
              open=initOpenStore(newRowList,series) ))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  store$open <- updateOpenStore(store$open,newRowList,series,store$iter)
  return(store)
}

Cal_RSRS <- function(High, Low, startIndex, iter){
  for (j in startIndex:iter){
    HighList <- High[startIndex:(iter-1)]
    LowList <- Low[startIndex:(iter-1)]       
    #OLS linear regression
    fit=lm(HighList~LowList) 
    nrsrs <- as.numeric(fit$coefficients[2])
    return(nrsrs)
  }
}

Cal_RSRS_M <- function(High, Low, startIndex_m,lookback,iter){
  N = startIndex_m
  mrsrs = c()
  for (i in N:iter){ #定义i循环10�??310
    HighList <- High[(i-lookback):i]
    LowList <- Low[(i-lookback):i]       
    #OLS linear regression
    fit=lm(HighList~LowList) 
    mrsrs=c(mrsrs,as.numeric(fit$coefficients[2]))#所有rsrs集合为向�??
  }
  return(mrsrs)
}

Cal_ATR <- function(High,Low,Close ,startIndex,lookback,iter){
  nTR <- 0
  ATR <- 0
  for( i in startIndex+1:iter){
    #52~69
    TR <- max((High[i]-Low[i]), abs(High[i]-Close[i-1]), abs(Close[i-1]-Low[i]))
    nTR <- nTR + TR 
  } 
  ATR <- nTR/lookback
  return(ATR)
}

