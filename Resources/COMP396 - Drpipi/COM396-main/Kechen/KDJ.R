library(TTR)
maxRows <- 3100

#visualization
strategyMatrix <- matrix(ncol = 9)
runningDays <- 1000
date <- vector()


getOrders <- function(store,newRowList,currentPos,info,params) {

  allzero  <- rep(0,length(newRowList)) 
  #visualization
  currentPosition <- vector()
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; 
  pos <- allzero
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;

  if (store$iter > params$smaLookback) {
    
    startIndex <-  store$iter - params$kdjLookback
    startIndexSMA <-  store$iter - params$smaLookback
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      op <- newRowList[[params$series[i]]]$Open
      
      clnum <- as.numeric(newRowList[[params$series[i]]]$Close)
      opnum <- as.numeric(newRowList[[params$series[i]]]$Open)
      dif   <- (clnum-opnum)/opnum

      #calculate the index
      
      KDlist <- stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD, 
                      nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)
      
      KD <- last(KDlist)
      
      #compute the yesterday KD
      KD0<- last(KDlist[-nrow(KDlist),])
      
      Jline <- 3*KD[,'fastK']- 2*KD[,'fastD'] 
      Jline0<- 3*KD0[,'fastK']- 2*KD0[,'fastD'] 
      
      sma <- last(SMA(store$cl[startIndexSMA:store$iter,i],params$smaLookback))
      
      Psizing<- 0 
      stoploss<- NA
      #cat("Jline0",Jline0," and Jline",Jline,"\n")
      
      #make the decision
      if (Jline0 < params$Jlow && Jline > params$Jlow && !is.na(Jline)&& !is.na(Jline0)) {
        
        Psizing<- dnorm(0.8, mean = 1, sd = 0.1)
        pos[params$series[i]] <- 1
        stoploss<- as.numeric(newRowList[[params$series[i]]]$Low)

        #limitOrders1[params$series[i]]<- limitOrders1[params$series[i]] - pos[params$series[i]]
        #limitPrices1[params$series[i]]<-   sma
        
        #limitOrders2[params$series[i]]<- limitOrders2[params$series[i]] - pos[params$series[i]]
        #limitPrices2[params$series[i]]<-  stoploss
        
      }else if (Jline0 > params$Jhigh && Jline < params$Jhigh &&!is.na(Jline)&& !is.na(Jline0)) {
        #dnorm(0.8, mean = 1, sd = 0.1)
        Psizing<- (0.8-Jline)*0.6+1*0.4
        pos[params$series[i]] <- -1
        stoploss<- as.numeric(newRowList[[params$series[i]]]$High)
       
        #price hit the ma80 then sell
        #limitOrders1[params$series[i]]<- limitOrders1[params$series[i]]  - pos[params$series[i]]
        #limitPrices1[params$series[i]]<- sma 
        
        #set the stop loss at the Highest price yesterday
        #limitOrders2[params$series[i]]<-  limitOrders2[params$series[i]] - pos[params$series[i]]
        #limitPrices2[params$series[i]]<-  stoploss
      }else{
        pos[params$series[i]] <- 0
      }
      
      #visualization
      currentPosition <- append(currentPosition,
                                currentPos[params$series[i]])
    }
    
    #visualization
    date <<- append(date,index(newRowList[[1]]))
    strategyMatrix <<- rbind(strategyMatrix,currentPosition)
    
    if(store$iter==runningDays-2){
      
      # Because of the initialization, the first row of the matrix 
      # will be NA, need to delete it before plotting
      strategyMatrix <- strategyMatrix[-1,]
      
      # Produce all the plots of running series
      for(i in 1:length(params$series)){

        # Save as png
        png(paste("Graph", toString(params$series[i]), ".png"),
            width = 1920, height = 1080, units = "px")
        
        # x axis: date, generate from newRowList
        # y axis: current position of the specified time series
        matplot(date,strategyMatrix[,i],
                ylab="Current Position", type='l')
        dev.off()
      }
    }
    
  }    
  pos <- pos #check the position sizes
  marketOrders <- marketOrders + pos

  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
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
