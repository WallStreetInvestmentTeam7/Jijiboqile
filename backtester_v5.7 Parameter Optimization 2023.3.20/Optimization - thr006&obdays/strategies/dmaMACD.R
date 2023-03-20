maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  store <- updateStore(store, newRowList, params$series)
  
  
  marketOrders <- allzero; pos <- allzero
  #print(pos)
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
  
 
  if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {  
   
    
    startIndexma <-  store$iter - params$dmalookback$long
    startIndexmacd <-  store$iter - params$macdlookback
    startIndexkdj <- store$iter - params$kdjlookback
    
    for (i in 1:length(params$series)) {
        
      closeP <- as.vector(newRowList[[params$series[i]]]$Close)
      ma <- SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short)
      short_ma <- last(ma)
      #last_short_ma <- ma[nrow(ma)-1,]
      long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      #print(short_ma)
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      KDJ<- as.data.frame(last(stoch(store$cl[startIndexkdj:store$iter,i],nFastK = params$nFastK, 
                         nFastD = params$nFastD,
                         nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)))
      Kline <- KDJ$fastK * 100
      # KDJyes <- as.data.frame(last(stoch(store$cl[startIndexmacd:(store$iter-1),i],nFastK = params$nFastK, 
      #                                    nFastD = params$nFastD,
      #                                    nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)))
      
      
      #codes for 绝对值
      # OpenDiffs <- diff(store$open)
      # absOpenDiffs <- matrix(abs(OpenDiffs),ncol = length(params$series))
      # avgAbsDiffs <- apply(absOpenDiffs,2,function(x) mean(x))
      # largestAvgAbsDiffs <- max(avgAbsDiffs)
      #print(abs(short_ma - long_ma))
      #codes for maxopen/mean(open)
      opall <- matrix(store$open,ncol = length(params$series))
      openprice <- apply(opall,2, function(x) mean(x[x>0]))
      largestopen <- max(opall)
      #print(as.vector(newRowList[[params$series[i]]]$Close))
      dmaPos <- round(largestopen/openprice)
      
      #print(round(largestAvgAbsDiffs/avgAbsDiffs))
      #when there is a cross in double moving average and macd indicators
      #it is a clear entry point and it also implies the future up-ward trending 
      
      if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short))) 
        <
        last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
        &&short_ma >= long_ma
          && macd$macd >macd$signal ) {
       
        pos[params$series[i]] <- (dmaPos[i]) * abs(short_ma - long_ma)/long_ma*100
        #here to identify if it possible for a trend reverse
        if(Kline<50 && closeP < short_ma){
          limitOrders1[i] <- -pos[params$series[i]]
          limitPrices1[i] <- short_ma * (1 - 0.05)
        }

       
      }
      
      #when there is a cross in double moving average and macd indicators
      #it is a clear exit point and it also implies the future down-ward trending
      # last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
      # >
      #   last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
      # &&
      else if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
        >
        last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
        &&short_ma <= long_ma
        && macd$macd < macd$signal
               ) {
          pos[params$series[i]] <- -abs(short_ma - long_ma)/long_ma*(dmaPos[i])*100
          if(Kline>50 && closeP > short_ma){
            limitOrders1[i] <- pos[params$series[i]]
            limitPrices1[i] <- short_ma * (1 + 0.05)
          }
          # limitOrders1[i] <- -currentPos[params$series[i]]
          # limitPrices1[i] <- short_ma * (1 + 0.05)
          # if(long_ma >= short_ma* 1.08){
          #   pos[params$series[i]] <- -currentPos[params$series[i]]
          # }
          
        }
      else{
        pos[params$series[i]] <- 0
      }
     
    }
    
  }
  
  
  
  marketOrders <- marketOrders + pos
  #print(marketOrders)
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}
#about updating close price
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
#about updating volume
initVolStore  <- function(newRowList,series) {
  volStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}
updateVolStore <- function(volStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}
#about updating open prices
initOpenStore  <- function(newRowList,series) {
  openStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(openStore)
}
updateOpenStore  <- function(openStore, newRowList, series, iter) {
  for (i in 1:length(series))
    openStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(openStore)
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

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              vol=initVolStore(newRowList,series),
              high=initHigh(newRowList,series),
              low=initLow(newRowList,series), 
              open=initOpenStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series, action) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$open <- updateOpenStore(store$open,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  return(store)
  }
################function for calculate highest high price and lowest low price in one period###########
# Cal_KDJ_E <- function(High, Low, Open, Close, iter,)

