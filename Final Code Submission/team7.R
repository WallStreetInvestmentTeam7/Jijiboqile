params = list(dmalookbacks = list(short=as.integer(10),
                                  long=as.integer(80)),
              macdlookback = as.integer(50),
              macdFast=12, macdSlow=26, macdSig=9, macdMa="EMA", series = 1:10,
              kdjlookback=15,nFastK=5,nFastD=3,
              nSlowD=3, 
              rsrs_lookback = 18,rsrs_lookback_m= 50,
              series = 1:10,
              thr006=65,
              obday=30)

maxRows <- 11000 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  #Initialize store at the beginning
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #Initialize market orders and limit orders
  marketOrders <- allzero
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
  

  #Initialize dmaMACD strategy's market position
  dmaPos <- allzero
  
  #Initial position for rsrs & alpha006
  rsrsPos <- -currentPos
  a006Pos <- -currentPos
  
  
  #-----------------------------------logic for rsrs & alpha006-----------------------------------
  
  #Trade after lookback period
  if(store$iter > params$rsrs_lookback_m + params$rsrs_lookback){
    
    #Iteerate through stocks
    for (i in 1:length(params$series)){
      
      #Position normalization
      clall <- matrix(store$cl,ncol = length(params$series))
      closeprice <- apply(clall,2, function(x) mean(x[x>0]))
      largestclose <- max(clall)
      posnorm <- round(largestclose/closeprice)
      
      #Position sizing and capital allocation
      estCostToBuy <- sum(posnorm * closeprice)
      targetspent_a <- 300000
      targetspent_r <- 50000
      multiplier_a <- targetspent_a/estCostToBuy
      multiplier_r <- targetspent_r/estCostToBuy

      #Get indicators
      HIGH = store$high[,i]
      LOW = store$low[,i]
      OPEN <- store$ope[,i]
      CLOSE <- store$cl[,i]
      VOLUME <- store$vol[,i]
      
      #Get profit & loss for yesterday
      prev_close <- CLOSE[store$iter-2]
      cur_open <- OPEN[store$iter-1]
      next_open <- OPEN[store$iter]
      
      slippage <- abs(prev_close-next_open)*0.2 #Run from day 2, where oldPos would always be 0, until penultimate day
      pnl_yesterday1 <- store$rsrsPos[params$series[i]] * (next_open - cur_open) - abs(store$rsrsPos[params$series[i]]) * slippage
      pnl_yesterday2 <- store$a006Pos[params$series[i]] * (next_open - cur_open) - abs(store$rsrsPos[params$series[i]]) * slippage
      
      pnl_yesterday <- pnl_yesterday1 + pnl_yesterday2

      #Calculate Nth day's RSRS
      startIndex <- store$iter - params$rsrs_lookback #params$lookback=10, start from 301th day
      rsrs_n <- Cal_RSRS(HIGH, LOW, startIndex,store$iter)

      #Calculate previous M days' RSRS
      startIndex_m <- store$iter - params$rsrs_lookback_m #start from 11th day
      rsrs_m <- Cal_RSRS_M(HIGH, LOW, startIndex_m, params$rsrs_lookback, store$iter)

      #Calculate Z-score rsrs
      rsrs_z <- (rsrs_n - mean(rsrs_m))/sd(rsrs_m)
      
      #Calculate Average True Rate (ATR) for limit orders stop loss
      ATR <- as.numeric(Cal_ATR(HIGH, LOW, CLOSE, startIndex, params$rsrs_lookback, store$iter))
      stop <- ATR*0.8
      op <- OPEN[store$iter]
      mean_p <- (HIGH[store$iter]+LOW[store$iter])/2
      
      #Initialize Alpha 006
      thr006 <- params$thr006
      
      #For the days that before observe days
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
      
      #Get Alpha018 ranking indicators after 10 days
      if (store$iter > 10){
        OPENLIST <- OPEN[0:store$iter]
        CLOSELIST <- CLOSE[0:store$iter]
        amplitude <- CLOSE[store$iter]-OPEN[store$iter]
        alpha018vector = -1*rank(runSD(abs(CLOSELIST-OPENLIST), n=5) + amplitude + runCor(CLOSELIST, OPENLIST, n = 10))
        
        #Find the current days' alpha 018 value
        alpha018 <- alpha018vector[store$iter]
        rank018 <- alpha018/store$iter
      }
      
      #Set position
      #Investors have negative attitude towards price trend, go short
      if (rsrs_z < 0.7){
        
        rsrsPos[params$series[i]] <- -round(rsrs_n * (posnorm[i])) * multiplier_r
        
        #rsrs limit order stop loss
        if(as.numeric(op) >= as.numeric(mean_p) - as.numeric(stop)){
          limitOrders1[i] <- rsrsPos[params$series[i]]
          limitPrices1[i] <- mean_p + stop
        }
      }
      
      #Investors have positive attitude towards price trend, go long
      else if (rsrs_z > 0.7){
        
        rsrsPos[params$series[i]] <- round(rsrs_n * (posnorm[i]))* multiplier_r
        
        #rsrs limit orders stop loss
        if(as.numeric(op) <= as.numeric(mean_p) - as.numeric(stop)){
          limitOrders2[i] <- -rsrsPos[params$series[i]]
          limitPrices2[i] <- mean_p - stop
        }
        
        #If alpha006 further indicates that price trend is positive, go long further
        if (alpha006*100 > thr006){
          
          #If alpha018 ranking indicates that there is a top 20% ranking on positive price trend, add 100% more position
          if(rank018 <= 0.2){
            a006Pos[params$series[i]] <- round(1+alpha006) * posnorm[i]*multiplier_a
          }
          
          #If alpha018 ranking indicates that there is a 20%-50% ranking on positive price trend, add 50% more position
          else if(rank018 <= 0.5 && rank018 > 0.2 ){
            a006Pos[params$series[i]] <- round(1+alpha006) * posnorm[i]*(multiplier_a/2)
          }
          
          #If alpha018 ranking indicates that there is a under 50% ranking on positive price trend, add 25% more position
          else{
            a006Pos[params$series[i]] <- round(1+alpha006) * posnorm[i] *(multiplier_a/4)
          }
        }
      }
      
      #Investors' attitude towards price trend is unknown
      else{
          rsrsPos[params$series[i]] <- 0
          a006Pos[params$series[i]] <- 0
      }
      
      #General stop loss for rsrs & alpha006
      #Cut the position to half when yesterdays' simulation on return is negative
      if (pnl_yesterday<0){
        rsrsPos <- rsrsPos/2
        a006Pos <- a006Pos/2
      }
    }
  }
  #---------------------------------------------------------------------------------------

  #-----------------------------------logic for dmaMACD-----------------------------------
  
  #Trade after lookback Period  
  if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {
    
    #Get Indicators
    startIndexma <-  store$iter - params$dmalookback$long
    startIndexmacd <-  store$iter - params$macdlookback
    startIndexkdj <- store$iter - params$kdjlookback
    
    #Iterate through stocks
    for (i in 1:length(params$series)) {
      
      #Get indicators
      HIGH = store$high[,i]
      LOW = store$low[,i]
      OPEN <- store$ope[,i]
      CLOSE <- store$cl[,i]
      VOLUME <- store$vol[,i]
      
      #Get profit & loss for yesterday
      prev_close <- CLOSE[store$iter-2]
      cur_open <- OPEN[store$iter-1]
      next_open <- OPEN[store$iter]
      
      slippage <- abs(prev_close-next_open)*0.2 # run from day 2, where oldPos would always be 0, until penultimate day
      
      pnl_yesterday3 <- store$dmaPos[params$series[i]] * (next_open - cur_open) - abs(store$dmaPos[params$series[i]]) * slippage
      
      #Calculate short-term, long-term simple moving average & MACD
      closeP <- as.vector(newRowList[[params$series[i]]]$Close)
      short_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      
      #Calculate KDJ indicator for stop loss
      KDJ<- as.data.frame(last(stoch(store$cl[startIndexkdj:store$iter,i],nFastK = params$nFastK, 
                                     nFastD = params$nFastD,
                                     nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)))
      
      #Calculate the assignment interval of the K indicator
      Kline <- KDJ$fastK * 100
      
      #Using maximum close price and mean(close) to set position normalization
      clall <- matrix(store$cl,ncol = length(params$series))
      closeprice <- apply(clall,2, function(x) mean(x[x>0]))
      largestclose <- max(clall)
      posnorm1 <- round(largestclose/closeprice)
      
      #Position Sizing & Capital Allocation
      estCostToBuy <- sum(posnorm1 * closeprice)
      targetspent <- 450000
      multiplier_d <- targetspent / estCostToBuy
      
      #When there is a cross in double moving average and macd indicators
      #It is a clear entry point and it also implies the future up-ward trending
      if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
          <
          last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
          &&short_ma >= long_ma
          && macd$macd >macd$signal ) {
        
        #If alpha018 ranking indicates that there is a top 20% ranking on positive price trend, add 100% more position
        if(rank018 <= 0.2){
          dmaPos[params$series[i]] <-  abs(short_ma - long_ma)*multiplier_d/long_ma*(posnorm1[i])*100
        }
        
        #If alpha018 ranking indicates that there is a 20%-50% ranking on positive price trend, add 50% more position
        else if(rank018 <= 0.5 && rank018 > 0.2 ){
          dmaPos[params$series[i]] <-  (abs(short_ma - long_ma)/long_ma)*100*posnorm1[i]*(multiplier_d/2)
        }
        
        #If alpha018 ranking indicates that there is a under 50% ranking on positive price trend, add 25% more position
        else{
          dmaPos[params$series[i]] <-  (abs(short_ma - long_ma)/long_ma)*100*posnorm1[i]*(multiplier_d/4)
        }
        
        #Upward trending of price will stop rapidly
        #Using limit orders to stop loss
        if(Kline<50 && closeP < short_ma){
          limitOrders1[i] <- -dmaPos[params$series[i]]
          limitPrices1[i] <- short_ma * (1 - 0.05)
        }
      }

      #When there is a cross in double moving average and macd indicators
      #It is a clear exit point and it also implies the future down-ward trending
      else if (last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$short)))
               >
               last((SMA(store$cl[startIndexma:store$iter-1,i],n=params$dmalookbacks$long)))
               &&short_ma <= long_ma
               && macd$macd < macd$signal
      ) {
        dmaPos[params$series[i]] <- -abs(short_ma - long_ma)/long_ma*(posnorm1[i])*100*multiplier_d
        
        #Downward trending of price will stop rapidly
        #Using limit orders to stop loss
        if(Kline>50 && closeP > short_ma){
          limitOrders1[i] <- dmaPos[params$series[i]]
          limitPrices1[i] <- short_ma * (1 + 0.05)
        }
      }
      else{
        dmaPos[params$series[i]] <- 0
      }
      
      #General stop loss for dmaMACD
      #Cut the position to half when yesterdays' simulation on return is negative
      if (pnl_yesterday3<0){
        dmaPos <- dmaPos/2
      }
    }
  }
  #---------------------------------------------------------------------------------------
  
  #Update positions from the above strategies
  store <- updateDmaPos(store, dmaPos)
  store <- updateRsrsPos(store, rsrsPos)
  store <- updateAlpha006Pos(store, a006Pos)
  
  #Sum to get the final market order positions
  marketOrders <- marketOrders + rsrsPos + dmaPos + a006Pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}


#-----------------------------------Additional Functions-----------------------------------

#Functions of rsrs strategy;
Cal_RSRS <- function(HIGH,LOW,  startIndex, iter)
  for (j in startIndex:iter){
    HighList <- HIGH[startIndex:(iter-1)]
    LowList <- LOW[startIndex:(iter-1)]
    
    #OLS linear regression
    fit=lm(HighList~LowList)
    nrsrs <- as.numeric(fit$coefficients[2])
    return(nrsrs)
  }

Cal_RSRS_M <- function(HIGH,LOW, startIndex_m,lookback,iter){
  N = startIndex_m
  mrsrs = c()
  for (i in N:iter){ 
    HighList <- HIGH[(i-lookback):i]
    LowList <- LOW[(i-lookback):i]
    
    #OLS linear regression
    fit=lm(HighList~LowList)
    
    #Store all rsrs values as a vector
    mrsrs=c(mrsrs,as.numeric(fit$coefficients[2]))
  }
  return(mrsrs)
}

Cal_ATR <- function(HIGH,LOW,CLOSE,startIndex,lookback,iter){
  nTR <- 0
  for( i in startIndex+1:iter){
    #52~69
    TR <- max((HIGH[i]-LOW[i]), abs(HIGH[i]-CLOSE[i-1]), abs(CLOSE[i-1]-LOW[i]))
    nTR <- nTR + TR 
  } 
  ATR <- nTR/lookback
  return(ATR)
}

#dmaPos records the position of dmaMACD strategy
updateDmaPos <- function(store, dmaPos) {
  store$dmaPos <- dmaPos
  return(store)
}
#rsrsPos records the position of rsrs strategy
updateRsrsPos <- function(store, rsrsPos) {
  store$rsrsPos <- rsrsPos
  return(store)
}
#a006Pos records the pos of Alpha006 strategy
updateAlpha006Pos <- function(store, a006Pos) {
  store$a006Pos <- a006Pos
  return(store)
}

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

#functions for managing open prices
initOpeStore  <- function(newRowList,series) {
  opeStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opeStore)
}
updateOpeStore <- function(opeStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opeStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opeStore)
}

#Initialize store
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              vol=initVolStore(newRowList,series),
              high=initHigh(newRowList,series),
              low=initLow(newRowList,series),
              ope=initOpeStore(newRowList,series),
              dmaPos=rep(0,ncol= length(params$series)),
              rsrsPos=rep(0,ncol= length(params$series)),
              a006Pos=rep(0,ncol= length(params$series))
             ))
}

#Update store
updateStore <- function(store, newRowList, series, action) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$ope <- updateOpeStore(store$ope,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  return(store)
}
#---------------------------------------------------------------------------------------