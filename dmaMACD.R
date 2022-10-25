maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero
  
  for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close

      #print(cl)
      #prices <- as.xts(store$cl[[params$series[i]]])

      short_ma <- last(EMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(EMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      #print(last((SMA(store$cl[startIndex:store$iter-1,i],n=params$dmalookbacks$short))))
      #print(short_ma) 
     
      if (last((EMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
          <=
          last((EMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
          &&
          short_ma > long_ma
          &&macd$macd > macd$signal) {
        pos[params$series[i]] <- 1

      }

      #
      else if (last((EMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
               >=
               last((EMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
               &&
               short_ma < long_ma
               &&
               macd$macd < macd$signal) {

        pos[params$series[i]] <- -1
      }
    }
  }
  
  

  pos <- pos
  marketOrders <- marketOrders + pos
  
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
