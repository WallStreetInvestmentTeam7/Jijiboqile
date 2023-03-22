maxRows <- 3100 # used to initialize a matrix to store closing prices

#source('framework/data.R'); 
#source('framework/backtester.R')
#source('framework/processResults.R'); 
#source('example_strategies.R');

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; pos <- allzero
  
  # #Initialize threshold value
  thr006 <- params$thr006
  #thr018 <- params$thr018
  #thr034 <- params$thr034
  
  #Iterate through the series in params$series
  for (i in params$series){
    #using open prices difference as the position normalization
    OpenDiffs <- diff(store$ope)
    absOpenDiffs <- matrix(abs(OpenDiffs),ncol = length(params$series))
    avgAbsDiffs <- apply(absOpenDiffs,2,function(x) mean(x[x>0]))
    largestAvgAbsDiffs <- max(avgAbsDiffs)
    posnorm <- round(largestAvgAbsDiffs/avgAbsDiffs)
    
    #thr006 <- params$thr006
    #Ignore the first day, or it shall occur error
    if(store$iter>1){
      
      #Get every stock's volume and closed price data
      VOLUME = store$vol[,i]
      CLOSE = store$cl[,i]
      OPEN = store$ope[,i]
      
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
      print(paste("alpha006 =",alpha006))
      
      #Apply Alpha018 equation
      #Get Everyday's new alpha rate
      #(Ignore the first ten days' data, or shall occur error)
      # if (store$iter > 10){
      #   OPENLIST <- OPEN[0:store$iter]
      #   CLOSELIST <- CLOSE[0:store$iter]
      #   amplitude <- CLOSE[store$iter]-OPEN[store$iter]
      #   alpha018vector = -1*rank(runSD(abs(CLOSELIST-OPENLIST), n=5) + amplitude + runCor(CLOSELIST, OPENLIST, n = 10))
      #   
      #   #Find the current days' alpha 018 value
      #   alpha018 <- alpha018vector[store$iter]
      # }
      
      #Apply Alpha 034 equation
      #Get Everyday's new alpha rate
      #(Ignore the first five days' data, or shall occur occur
      # if (store$iter > 5){
      #   OPENLIST <- OPEN[0:store$iter]
      #   CLOSELIST <- CLOSE[0:store$iter]
      #   RETURNLIST <- (CLOSELIST-OPENLIST)/OPENLIST
      #   alpha034vector = rank((1 - rank((runSD(RETURNLIST, n=2) / runSD(RETURNLIST, n=5)))) + (1-rank(diff(CLOSELIST))))
      #   
      #   #Find the current days' alpha 034 value
      #   alpha034 <- alpha034vector[store$iter]
      # }
      
      
      #Only trade after 10 days (because of alpha 018)
      if (store$iter > 1){
        #Change Position
        if (alpha006*100 < thr006){
          pos[params$series[i]] <- -abs(alpha006) * posnorm[i]/100
        }
        else if (alpha006*100 > thr006){
          pos[params$series[i]] <- abs(alpha006) * posnorm[i]/100
        }
        else if (alpha006*100 == thr006){
          pos[params$series[i]] <- 0
        }
      }
      
    }
  }
  
  #Update market orders
  marketOrders <- allzero + pos
  
  
  
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


initVolStore  <- function(newRowList,series) {
  volStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}
updateVolStore <- function(volStore, newRowList, series, iter) {
  for (i in 1:length(series))
    volStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}

initOpeStore  <- function(newRowList,series) {
  opeStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opeStore)
}
updateOpeStore <- function(opeStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opeStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opeStore)
}


initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series),ope=initOpeStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$ope <- updateOpeStore(store$ope,newRowList,series,store$iter)
  return(store)
}