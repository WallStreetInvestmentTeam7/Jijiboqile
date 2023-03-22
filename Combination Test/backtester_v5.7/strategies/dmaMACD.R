maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  
  marketOrders <- allzero; pos <- allzero
  
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
 
  
  # if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {
  #   startIndexma <-  store$iter - params$dmalookback$long
  #   startIndexmacd <-  store$iter - params$macdlookback
  
  
  
  combined_vec <- c()
  # loop through each series
  for (i in 1:length(params$series)) {
    # find the vector open price for the current series
    open_vec <- as.numeric(newRowList[[params$series[i]]]$Open)
    list_open <- list(as.numeric(newRowList[[params$series[i]]]$Open))
    print(length(list_open))
    combined_vec <- c()
    # for (j in 1:nrow(open_vec)) {
    #   # add the maximum open price to the vector
    #   combined_vec <- c(combined_vec, open_vec)
    #   
    # }
    
  }
  
  #print(as.numeric(newRowList[[params$series[i]]]$Open))
  #print(list(as.numeric(newRowList[[params$series[i]]]$Open)))
  # find the maximum value in the combined vector
  #maxOpen_final <- max(maxOpen_vec)
  
  # view the final output
  # print(maxOpen_final)
  
  
  if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {  
   
    
    startIndexma <-  store$iter - params$dmalookback$long
    startIndexmacd <-  store$iter - params$macdlookback
    
    for (i in 1:length(params$series)) {
        
        
        cl <- newRowList[[params$series[i]]]$Close
        op <- newRowList[[params$series[i]]]$Open
      short_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      
      #when there is a cross in double moving average and macd indicators
      #it is a clear entry point and it also implies the future up-ward trending 
      
      if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short))) 
        <
        last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
        &&short_ma >= long_ma
          && macd$macd >macd$signal ) {
        
        pos[params$series[i]] <-  1
        #print(pos[params$series[i]])
        limitOrders1[i] <- -currentPos[params$series[i]]
        limitPrices1[i] <- short_ma * (1 - 0.05)
        # if(short_ma >= long_ma* 1.08){
        #   pos[params$series[i]] <- -currentPos[params$series[i]]
        # }
       
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
        
          pos[params$series[i]] <- -1
          limitOrders1[i] <- -currentPos[params$series[i]]
          limitPrices1[i] <- short_ma * (1 + 0.05)
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

