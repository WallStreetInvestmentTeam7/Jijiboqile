maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
 
  
  if (is.null(store))  store <- initStore(newRowList,params$series)
     store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero
   pos <- allzero
  
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
 
  
  if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {
    startIndexma <-  store$iter - params$dmalookback$long
    startIndexmacd <-  store$iter - params$macdlookback
    #print(store$cl)
    print(max(store$open))
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      pricesdma <- store$cl[startIndexma:store$iter,i]
      pricesmacd <- store$cl[startIndexmacd:store$iter,i]
      priceslast <- store$cl[startIndexma:store$iter-1,i]
  
      op <- newRowList[[params$series[i]]]$Open
     
    #   macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
    #                                   nFast=params$macdFast, nSlow=params$macdSlow,
    #                                   maType=params$macdMa, percent=TRUE)))
    #   #print(last((EMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))) )
    #   #when there is a cross in double moving average and macd indicators
    #   #it is a clear entry point and it also implies the future up-ward trending 
    #   if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
    #       <
    #       last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
    #       &&
    #       short_ma >= long_ma
    #       && macd$macd > macd$signal ) {
    #     pos[params$series[i]] <-  1
    #     
    #   }
    #   
    # 
    #   #when there is a cross in double moving average and macd indicators
    #   #it is a clear exit point and it also implies the future down-ward trending
    #   else if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short))) 
    #            >
    #            last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
    #            &&
    #            short_ma <= long_ma
    #            && macd$macd < macd$signal
    #           
    #            ) {
    # 
    #     pos[params$series[i]] <- -1
    #     
    #   }
    #   else{
    #     pos[params$series[i]] <- 0
    #   }
    #   #Stop loss on price reversal of more than 5%
    #   # if ( pos >0 && newRowList[[params$series[i]]]$Close * long_stop >= newRowList[[params$series[i]]]$Close-1
    #   #    ) {
    #   #   pos[params$series[i]] <- -currentPos[params$series[i]]
    #   # }
    #   
      pos[params$series[i]] <- getsignal(getdma(pricesdma, priceslast,params$dmalookbacks),
                                         getmacd(pricesmacd,params$macdFast,params$macdSlow,params$macdMa),
                                         op
                                         ) * getPos(op)
      
     }
   
    
  }
  getdma <- function(pricesdma, priceslast,lookbacks){
    short_ma <- last(SMA(pricesdma,n=lookbacks$short))
    long_ma <- last(SMA(pricesdma,n=lookbacks$long))
    preshort <- last(SMA(priceslast,n=lookbacks$short))
    prelong <- last(SMA(priceslast,n=lookbacks$long))
    ret <- list(short = short_ma,long=long_ma,preshort = preshort,prelong = prelong)
    return(ret)
    
  }
  getmacd <- function(pricesmacd,fast,slow,ma){
    macd <- as.data.frame(last(MACD(pricesmacd,
                                    nFast=fast, nSlow=slow,
                                    maType=ma, percent=TRUE)))
    return(macd)
  }
    # 
  getsignal <- function(dma_list,macd,op){
    if(dma_list$preshort < dma_list$prelong &&dma_list$short >= dma_list$long 
       && macd$macd>macd$signal
       )
      return(1)
    else if(dma_list$preshort > dma_list$prelong &&dma_list$short <= dma_list$long 
            && macd$macd<macd$signal
      )
      return(-1)
    else{
      return(0)
    }
  }
  getPos <- function(op,constant = ){
    # This function should return (constant divided by current_close) 
    # rounded down to the nearest integer\
    
    return(round(constant/op))
  }
  
  
 
  marketOrders <- marketOrders + pos
  #print(marketOrders)
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series), open=initOpenStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series, action) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$open <- updateOpenStore(store$open,newRowList,series,store$iter)
  return(store)
}

