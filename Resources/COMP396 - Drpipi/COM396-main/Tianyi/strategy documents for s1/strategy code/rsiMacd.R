# The strategy will take long (short) postion whenever the price reaches the
# cci oversold or overbought line. When the price exceeds these two lines 
# too much, I will clear the position directly to avoid loss. 
# Meanwhile, using MACD to lighten up positions. 

maxRows <- 3100

rsiOverSold <- 30
rsiOverBought <- 70

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero
  
  # As cci has different lookback with MACD, 
  # this if statement is focused on cci
  if (store$iter > params$lookback) {
    
    startIndex <-  store$iter - params$lookback
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=14,
                      maType=list(maUp=list(EMA),maDown=list(WMA))))
      
      if (rsi < rsiOverSold && !is.na(rsi)) {
        pos[params$series[i]] <- 1
      }
      else if (rsi > rsiOverBought && !is.na(rsi)) {
        pos[params$series[i]] <- -1
      }
      
      # stop loss when the price reaches cciStop times of lines
      # if (cci < cciStop*cciOverSold && !is.na(cci) |
      #     cci > cciStop*cciOverBought && !is.na(cci)) {
      #   pos[params$series[i]] <- -currentPos[params$series[i]]
      # }
    }
  }
  
  # this if statement is focused on MACD
  if (store$iter > params$macdLookback) {
    
    startIndex <-  store$iter - params$macdLookback
    
    for (i in 1:length(params$series)) {
      
      macd <<- last(MACD(store$cl[startIndex:store$iter,i],
                        nFast=params$macdFast, nSlow=params$macdSlow,
                        maType=params$macdMa, percent=TRUE))
      
      # I think the market has issued a sell signal at this time, 
      # so I lighten up a share
      if (macd[,"signal"] > macd[,"macd"]) {
        pos[params$series[i]] <- -1
      } 
    }
  }
  
  pos <- pos #check the position sizes
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}