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
  
  marketOrders <- -currentPos; pos <- allzero
  
  #Initialize threshold value
  thr <- params$thr
  
  #Iterate through the series in params$series
  for (i in params$series){
    
    #Ignore the first day, or it shall occur error
    if(store$iter>1){
    
      #Get every stock's volume and closed price data
      VOLUME = store$vol[,i]
      CLOSE = store$cl[,i]
      
      #For the first specified days, store the first day to today's volume and close price
      if(store$iter<=params$obday){
        VOLUMELIST <- VOLUME[0:store$iter]
        CLOSELIST <- CLOSE[0:store$iter]
      }
      
      #After the specified days, store the most recent 10 days' data
      else if(store$iter>params$obday){
        VOLUMELIST <- VOLUME[as.numeric(store$iter-params$obday):store$iter]
        CLOSELIST <- CLOSE[as.numeric(store$iter-params$obday):store$iter]
      }
    
      #Apply Alpha006 equation
      #Get Everyday's new alpha rate
      alpha = -1*cor(as.vector(CLOSELIST), as.vector(VOLUMELIST), use = "everything", method="pearson")
      
      print(paste("day", store$iter))
      print(paste("series no.", i))
      print(paste("alpha006 =",alpha))
    
      #Change Position
      if (alpha*100 < thr){
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      }
      else if (alpha*100 > thr){
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
      else if (alpha*100 == thr){
        pos[params$series[i]] <- 0
      }
    
    }
  }
    
  #Update market orders
  marketOrders <- -currentPos + pos
  
  
#  #Initialize PD ratio and Threshold
#  BestThreshold <- 0
#  PD <- -Inf
#  Iteration <- 0  
  
#  #Get data
#  dataList <- getData(directory="PART1")

#  #Iterate through all possible alpha rate
#  for (a in -100:100){
#    
#    #Iterate through the series in params$series
#    for (i in params$series){
#      
#      #Get every stock's volume and closed price data
#      VOLUME = store$vol[,i]
#      CLOSE = store$cl[,i]
#      
#      #Alpha006 equation
#      #Get Everyday's alpha rate
#      alpha = -1*cor(CLOSE[store$iter], VOLUME[store$iter], method="pearson")
#      
#      #Change Position
#      if (alpha*100 < a){
#        pos[params$series[i]] <- -1
#      }
#      else if (alpha*100 > a ){
#        pos[params$series[i]] <- 1
#      }
#      else if (alpha*100 == a){
#        pos[params$series[i]] <- 0
#      }
#      
#      #Update market orders
#      marketOrders <- -currentPos + pos
#    }
#    
#    #The return list of the strategy
#    store=store
#    marketOrders=marketOrders
#    limitOrders1=allzero
#    limitPrices1=allzero
#    limitOrders2=allzero
#    limitPrices2=allzero
#    
#    #Store a temporary "getOrders" list
#    temOrders <- list(store, marketOrders, limitOrders1, limitPrices1, limitOrders2, limitPrices2)
#    
#    #Calculate the result by calling backtester
#    sMult <- 0.20 # slippage multiplier
#    is_valid_example_strategy <- function(strategy) { 
#      strategy %in% example_strategies
#    }
#    stopifnot(is_valid_example_strategy(strategy))
#    load_strategy(strategy) # function from example_strategies.R
    
    
    
#    results <- backtest(dataList,temOrders,params,sMult) #Error-infinite recursive
#    #####################################(Calculate Result)########################################
#    
#    numOfDays <- nrow(dataList[[1]])
#    numOfSeries <- length(dataList)
#    
#    # initialise as 0-vector of length length(dataList) 
#    newPosList <- vector(mode="numeric", length = length(dataList))
#    
#    # Initialisation of getOrders with first row of data, via is.null(store)
#    store <- NULL #(Note: Variable "store" occur potentially bug!)
#    
#    # pnlList will store trading results
#    # initialize lists of 0 rows; getRowList(dataList,1) used to get date for each via index()
#    # pnlList <- mapply(function(x, y) xts(x, index(y)),0, getRowList(dataList,1), SIMPLIFY = FALSE)
#    pnlList <- lapply(1:numOfSeries,function(x) matrix(0,nrow=numOfDays,ncol=1))
#    positionValuesList <- lapply(1:numOfSeries,function(x) matrix(0,nrow=numOfDays,ncol=1))
#    netWorthList <- rep(0, numOfDays)
#    
#    # vector that stores a 1 for every day a position was taken in some
#    # series and a 0 otherwise
#    # initialized as all zero vecotr 
#    posCounter <-  0
#    
#    nonxtsDataList <- lapply(dataList, function(x) as.matrix(x))
#    
#    balance <- 1000000
#    newNetWorth <- balance
#    netWorthList[[1]] <- balance
#    
#    bankrupt <- FALSE # Are we bankrupt?
#    
#    # MAIN LOOP
#    for (i in 2: (numOfDays-1)) { # cannot hold on day 1; day 1 data is given to strategy on day 2
#      
#      oldPosList    <- newPosList
#      
#      info = list(balance=balance, netWorth=newNetWorth)
#      
#      #Modified - Not using "getOrders" function which might occur infinite recursion
#      #Instead, using temporary list
#      x <- ?????
#      x <- getOrders(store, getRowList(dataList,i-1), oldPosList, info, params) #Error-infinite recursive
#    }
#    
#    ############################################################################################
      
    
#    pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')
#    
#    #Get the PD-ratio
#    NewPD <- pfolioPnL$fitAgg
#    
#    #Compare Pd-ratio and Update the best threshold
#    if (NewPD>=PD){
#      PD <- NewPD
#      BestThreshold <- a/100
#    }
#    
#    #Print Iteration
#    Iteration++
#    print("Iteration" +Iteration)
#  }
  
  
#  #Initializing Again
#  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
#  
#  if (is.null(store)) store <- initStore(newRowList,params$series)
#  store <- updateStore(store, newRowList, params$series)
#  
#  marketOrders <- -currentPos; pos <- allzero
#  
#  #Apply the Best Threshold
#  for (i in params$series){
#    
#    VOLUME = store$vol[,i]
#    CLOSE = store$cl[,i]
#    
#    #Alpha006
#    alpha = -1*cor(CLOSE, VOLUME, method="pearson")
#    
#    #Change Position
#    if (alpha*100 < BestThreshold){
#      pos[params$series[i]] <- -1
#    }
#    else if (alpha*100 > BestThreshold){
#      pos[params$series[i]] <- 1
#    }
#    else if (alpha*100 == BestThreshold){
#      pos[params$series[i]] <- 0
#    }
#    
#    #Update market orders
#    marketOrders <- -currentPos + pos
#  }
  
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


initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),vol=initVolStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  return(store)
}