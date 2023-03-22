maxRows <- 11000 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  
  # initialize store at the beginning
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  # initialize market orders and limit orders
  marketOrders <- allzero
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
  
  marketOrders1 <- allzero; 
  marketOrders2 <- -currentPos;
  
  # initialize dmaMACD strategy's market position
  dmaPos <- allzero
  #dmaLastTransaction <- store$dmaPos
  
  #initial rsrs strategy position to be all 0
  rsrsPos <- allzero
  #rsrsAccumulatePosition <- store$rsrsAccumulatePosition
  
  #main strategy logic
  #-------------------------------------------
  #Iterate through the series in params$series
  
  #--------logic for rsrs-----------------------------------
  
  
  if(store$iter > params$rsrs_lookback_m + params$rsrs_lookback){
    
    for (i in 1:length(params$series)){
      
      #Get every stock's high and low price data
      High = store$high[,i]
      Low = store$low[,i]
      
      # calculate Nth day's RSRS
      startIndex <- store$iter - params$rsrs_lookback #params$lookback=10, start from 301th day
      rsrs_n <- Cal_RSRS(High, Low, startIndex,store$iter)
      #print(rsrs_n)
      
      # calculate previous M days RSRS
      startIndex_m <- store$iter - params$rsrs_lookback_m #start from 11th day
      rsrs_m <- Cal_RSRS_M(High, Low, startIndex_m, params$rsrs_lookback, store$iter)
      
      
      #z-score rsrs
      rsrs_z <- (rsrs_n - mean(rsrs_m))/sd(rsrs_m)
      #print(rsrs_z)
      #rsrs <- c(rsrs, rsrs_z)
      
      startIndexma <-  store$iter - params$dmalookback$long
      startIndexmacd <-  store$iter - params$macdlookback
      
      cl <- newRowList[[params$series[i]]]$Close
      
      
      short_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      
      ######################################################
      #Initialize Alpha 006
      thr006 <- params$thr006
        
        #Get every stock's volume and closed price data
        VOLUME = store$vol[,i]
        CLOSE = store$cl[,i]
        
        #For the first specified days, store the first day to today's volume and close price
        if(store$iter<=params$obday){
          VOLUMELIST <- VOLUME[0:store$iter]
          CLOSELIST <- CLOSE[0:store$iter]
        }
        
        #After the specified days, store the most recent n days' data
        #n is the observed day and is passed in through parameter "obday"
        else if(store$iter>params$obday){
          VOLUMELIST <- VOLUME[as.numeric(store$iter-params$obday):store$iter]
          CLOSELIST <- CLOSE[as.numeric(store$iter-params$obday):store$iter]
        }
        
        #Apply Alpha006 equation
        #Get Everyday's new alpha rate
        alpha006 = -1*cor(as.vector(CLOSELIST), as.vector(VOLUMELIST), use = "everything", method="pearson")
        #print(paste("alpha006 =",alpha006))
      #########################################################
      
      #decide if we should go long/short/flat (returning 1/-1/0)
      if (rsrs_z < 0.7 && alpha006*100 < thr006){
        rsrsPos[params$series[i]] <- -params$posSizes[params$series[i]]
        if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short)))
            >
            last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
            &&short_ma <= long_ma
            && macd$macd < macd$signal
        ) {
          
          dmaPos[params$series[i]] <- -params$posSizes[params$series[i]]
          # limitOrders1[i] <- -currentPos[params$series[i]]
          # limitPrices1[i] <- short_ma * (1 + 0.05)
          # if(long_ma >= short_ma* 1.08){
          #   dmapos[params$series[i]] <- -currentPos[params$series[i]]
          # }
        }
        
        
      }
      else if (rsrs_z > 0.7 && alpha006*100 > thr006){
        rsrsPos[params$series[i]] <- params$posSizes[params$series[i]]
        
        if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
            <
            last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
            &&short_ma >= long_ma
            && macd$macd >macd$signal ) {
          
          dmaPos[params$series[i]] <-  params$posSizes[params$series[i]]
          # limitOrders1[i] <- -currentPos[params$series[i]]
          # limitPrices1[i] <- short_ma * (1 - 0.05)
          # if(short_ma >= long_ma* 1.08){
          #   dmapos[params$series[i]] <- -currentPos[params$series[i]]
          # }
          # 
        }
        
        # if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
        #     <
        #     last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
        #     &&short_ma >= long_ma
        #     && macd$macd >macd$signal ) {
        #   
        #   dmaPos[params$series[i]] <-  params$posSizes[params$series[i]]
        #   # limitOrders1[i] <- -currentPos[params$series[i]]
        #   # limitPrices1[i] <- short_ma * (1 - 0.05)
        #   if(short_ma >= long_ma* 1.08){
        #     pos[params$series[i]] <- -currentPos[params$series[i]]
        #   }
        #   
        #   
        #   
        # }
        
        
        #when there is a cross in double moving average and macd indicators
        
        # else if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short)))
        #          >
        #          last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
        #          &&short_ma <= long_ma
        #          && macd$macd < macd$signal
        # ) {
        #   
        #   dmaPos[params$series[i]] <- -params$posSizes[params$series[i]]
        #   # limitOrders[i] <- -currentPos[params$series[i]]
        #   # limitPrices[i] <- short_ma * (1 + 0.05)
        #   # if(long_ma >= short_ma* 1.08){
        #   #   pos[params$series[i]] <- -currentPos[params$series[i]]
        #   # }
        # }
        # else{
        #   dmaPos[params$series[i]] <- 0
        # }
        
      }
      else{
        rsrsPos[params$series[i]] <- 0
        dmaPos[params$series[i]] <- 0
      }
      
      
    }
    
  }
  
  
  
  #--------logic for dmaMACD-----------------------------------
  # if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {
  #   startIndexma <-  store$iter - params$dmalookback$long
  #   startIndexmacd <-  store$iter - params$macdlookback
  #   
  #   maxOpen <- 0
  #   for (i in 1:length(params$series)) {
  #     cl <- newRowList[[params$series[i]]]$Close
  #     op <- newRowList[[params$series[i]]]$Open
  #     maxOpen <-  max(maxOpen,newRowList[[params$series[i]]]$Open)
  #     short_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
  #     long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
  #     macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
  #                                     nFast=params$macdFast, nSlow=params$macdSlow,
  #                                     maType=params$macdMa, percent=TRUE)))
  #     # macd_2 <- as.data.frame(last(MACD(store$cl[startIndexmacd:(store$iter-1),i],
  #     #                                 nFast=params$macdFast, nSlow=params$macdSlow,
  #     #                                 maType=params$macdMa, percent=TRUE)))
  #     
  #     
  #     #when there is a cross in double moving average and macd indicators
  #     #it is a clear entry point and it also implies the future up-ward trending
  #     #last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
  #     #<
  #     # last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
  #     #&&
  #     if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
  #         <
  #         last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
  #         &&short_ma >= long_ma
  #         && macd$macd >macd$signal ) {
  #       
  #       dmaPos[params$series[i]] <-  1
  #       # limitOrders[i] <- -currentPos[params$series[i]]
  #       # limitPrices[i] <- short_ma * (1 - 0.05)
  #       # if(short_ma >= long_ma* 1.08){
  #       #   pos[params$series[i]] <- -currentPos[params$series[i]]
  #       # }
  #       
  #       
  #       
  #     }
  #     
  #     
  #     #when there is a cross in double moving average and macd indicators
  #     #it is a clear exit point and it also implies the future down-ward trending
  #     # last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short)))
  #     # >
  #     #   last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
  #     # &&
  #     else if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short)))
  #              >
  #              last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
  #              &&short_ma <= long_ma
  #              && macd$macd < macd$signal
  #     ) {
  #       
  #       dmaPos[params$series[i]] <- -1
  #       # limitOrders[i] <- -currentPos[params$series[i]]
  #       # limitPrices[i] <- short_ma * (1 + 0.05)
  #       # if(long_ma >= short_ma* 1.08){
  #       #   pos[params$series[i]] <- -currentPos[params$series[i]]
  #       # }
  #     }
  #     else{
  #       dmaPos[params$series[i]] <- 0
  #     }
  #     
  #   }
  #   
  # }
  
  
  #(123456)
  # Store all the values we need
  store$dmaPos <- dmaPos
  store$rsrsPos <- rsrsPos
  
  #Update market orders
  #Update market orders
  marketOrders2 <- -currentPos + rsrsPos
  marketOrders1 <- allzero + dmaPos
  
  marketOrders <- marketOrders1+marketOrders2 
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


#---------------------------------------------------------------
#Functions of rsrs strategy;
Cal_RSRS <- function(High, Low, startIndex, iter)
  for (j in startIndex:iter){
    HighList <- High[startIndex:(iter-1)]
    LowList <- Low[startIndex:(iter-1)]
    #OLS linear regression
    fit=lm(HighList~LowList)
    nrsrs <- as.numeric(fit$coefficients[2])
    return(nrsrs)
  }

Cal_RSRS_M <- function(High, Low, startIndex_m,lookback,iter){
  N = startIndex_m
  mrsrs = c()
  for (i in N:iter){ #定义i循环
    
    HighList <- High[(i-lookback):i]
    LowList <- Low[(i-lookback):i]
    #OLS linear regression
    fit=lm(HighList~LowList)
    #store all rsrs values as a vector
    mrsrs=c(mrsrs,as.numeric(fit$coefficients[2]))
  }
  return(mrsrs)
}
#---------------------------------------------------------------


###############################################################################
# All the subsequent functions were designed to simplify and
# improve the readaility of getNewPos();
#
# While not required, this type of function-based approach is advisable
# and these functions will likely have analogues in your strategies
###############################################################################

#-------------------------------------------
#DmaPos records the position of dmaMACD strategy, which will be used for the strategy's stop loss
updateDmaPos <- function(store, dmaPos) {
  store$dmaPos <- dmaPos
  return(store)
}
#cciAccumulatePosition records the position of rsrs strategy, which will be used for the strategy's stop loss
updateRsrsPos <- function(store, rsrsPos) {
  store$rsrsPos <- rsrsPos
  return(store)
}
#-------------------------------------------


#-------------------------------------------
# functions for managing the Close
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

#functions for managing the Volume
initVolStore  <- function(newRowList,series) {
  volStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}
updateVolStore <- function(volStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}
#-------------------------------------------


#-------------------------------------------
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              vol=initVolStore(newRowList,series),
              high=initHigh(newRowList,series),
              low=initLow(newRowList,series),
              dmaPos = rep(0,10),
              rsrsPos = rep(0,10)))
}
updateStore <- function(store, newRowList, series, action) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  return(store)
}

