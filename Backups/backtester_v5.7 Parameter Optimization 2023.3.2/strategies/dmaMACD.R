maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$dmalookback$long) {
    startIndex <-  store$iter - params$dmalookback$long
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      print(cl)
      #prices <- as.xts(store$cl[[params$series[i]]])

      short_ma <- last(SMA(store$cl[startIndex:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(SMA(store$cl[startIndex:store$iter,i],n=params$dmalookbacks$long))

      #
      if (short_ma > long_ma) {
        pos[params$series[i]] <- 1

      }

      #
      else if (short_ma < long_ma) {

        pos[params$series[i]] <- -1
      }
    }
  }
  
  if (store$iter > params$macdlookback) {

    startIndex <-  store$iter - params$macdlookback

    for (i in (1:length(params$series))) {

      #prices <- as.xts(store$cl[[params$series[i]]])
      macd <- last(MACD(store$cl[startIndex:store$iter,i],
                             nFast=params$macdFast, nSlow=params$macdSlow,
                             maType=params$macdMa, percent=TRUE))
      
      

      #dea <- EMA(prices$Close,macdSig,wilder = FALSE,ratio = NULL)
      #dif <- EMA(prices$Close,macdFast,wilder = FALSE,ratio = NULL) - EMA(prices$Close,macdSlow,wilder = FALSE,ratio = NULL)
      
      # A MACD less than 0 suggests a downward trend.
      if(macd < 0){
        pos[params$series[i]] <- -1

      } 
      # a macd more than 0 suggests a upward trend.
      # else if (macd > 0){
      #   pos[params$series[i]] <- 1
      # }
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
