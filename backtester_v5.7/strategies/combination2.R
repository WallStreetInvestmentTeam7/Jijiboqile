maxRows <- 11000 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  #Initializing
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  
  # initialize store at the beginning
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  # initialize market orders and limit orders
  marketOrders <- allzero
  
  limitOrders1=allzero;
  limitPrices1=allzero;
  limitOrders2=allzero;
  limitPrices2=allzero;
  
  # marketOrders1 <- allzero; 
  # marketOrders2 <- -currentPos;
  # marketOrders3 <- -currentPos;
  # initialize dmaMACD strategy's market position
  dmaPos <- allzero
  #dmaPos2 <- store$dmaPos
  
  #initial rsrs strategy position to be all 0
  rsrsPos <- allzero
  #rsrsPos2 <- store$rsrsPos
  #rsrsAccumulatePosition <- store$rsrsAccumulatePosition
  a006Pos <- allzero
  
  #main strategy logic
  #-------------------------------------------
  #Iterate through the series in params$series
  
  #---------------------------logic for Alpha006---------------------------
  
  
  #For the first specified days, store the first day to today's volume and close price
  
  # for (i in 1:length(params$series)) {
  #   
  #   #using open prices difference as the position normalization
  #   OpenDiffs <- diff(store$ope)
  #   absOpenDiffs <- matrix(abs(OpenDiffs),ncol = length(params$series))
  #   avgAbsDiffs <- apply(absOpenDiffs,2,function(x) mean(x[x>0]))
  #   largestAvgAbsDiffs <- max(avgAbsDiffs)
  #   posnorm <- round(largestAvgAbsDiffs/avgAbsDiffs)
  #   
  #   #Get every stock's volume and closed price data
  #   VOLUME = store$vol[,i]
  #   CLOSE = store$cl[,i]
  #   OPEN = store$ope[,i]
  #   
  #   #Initialize Alpha 006
  #   thr006 <- params$thr006
  #   
  #   if(store$iter<=params$obday){
  #     VOLUMELIST <- VOLUME[0:store$iter]
  #     CLOSELIST <- CLOSE[0:store$iter]
  #   }
  #   
  #   #After the specified days, store the most recent n days' data
  #   #n is the observed day and is passed in through parameter "obday"
  #   else if(store$iter>params$obday){
  #     VOLUMELIST <- VOLUME[as.numeric(store$iter-params$obday):store$iter]
  #     CLOSELIST <- CLOSE[as.numeric(store$iter-params$obday):store$iter]
  #   }
  #   
  #   #Apply Alpha006 equation
  #   #Get Everyday's new alpha rate
  #   alpha006 = -1*cor(as.vector(CLOSELIST), as.vector(VOLUMELIST), use = "everything", method="pearson")
  #   #print(paste("alpha006 =",alpha006))
  #   
  #   #Do not trade in the first day
  #   if (store$iter > 1){
  #     #Change Position
  #     if (alpha006*100 < thr006){
  #       a006Pos[params$series[i]] <- -posnorm[i]
  #     }
  #     else if (alpha006*100 > thr006){
  #       a006Pos[params$series[i]] <- posnorm[i]
  #     }
  #     else if (alpha006*100 == thr006){
  #       a006Pos[params$series[i]] <- 0
  #     }
  #   }
  #   
  # }
  # 
  
  #--------logic for rsrs-----------------------------------
  
  
  if(store$iter > params$rsrs_lookback_m + params$rsrs_lookback){

    for (i in 1:length(params$series)){

      #using open prices difference as the position normalization
      OpenDiffs <- diff(store$ope)
      absOpenDiffs <- matrix(abs(OpenDiffs),ncol = length(params$series))
      avgAbsDiffs <- apply(absOpenDiffs,2,function(x) mean(x[x>0]))
      largestAvgAbsDiffs <- max(avgAbsDiffs)
      posnorm <- round(largestAvgAbsDiffs/avgAbsDiffs)

      #indicators for RSRS strategy
      #Get every stock's high and low price data
      High = store$high[,i]
      Low = store$low[,i]

      # calculate Nth day's RSRS
      startIndex <- store$iter - params$rsrs_lookback #params$lookback=10, start from 301th day
      rsrs_n <- Cal_RSRS(High, Low, startIndex,store$iter)
      #print(rsrs_n)

      # calculate previous M days RSRS
      startIndex_m <- store$iter - params$rsrs_lookback_m #start from 11th day
      rsrs_m <- Cal_RSRS_M(High, Low, startIndex_m, params$rsrs_lookback, store$iter)

      #z-score rsrs
      rsrs_z <- (rsrs_n - mean(rsrs_m))/sd(rsrs_m)
#########################################################################      
      # ATR <- Cal_ATR(High, Low,Close, startIndex, params$lookback,store$iter)
      # ATR <- as.numeric(ATR)
      # ##print(ATR)
      # stop <- ATR*0.8
      # #print(stop)
      # op <- Open[store$iter]
      # mean_p <- (High[store$iter]+Low[store$iter])/2
########################################################################
      #Get every stock's volume and closed price data
      VOLUME = store$vol[,i]
      CLOSE = store$cl[,i]
      OPEN = store$ope[,i]

      #Initialize Alpha 006
      thr006 <- params$thr006

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
      #print(paste("alpha006 =",alpha006))
#############################################################################


      if (rsrs_z < 0.7){
        
        rsrsPos[params$series[i]] <- -round(rsrs_n * (posnorm[i]/68))
        #Only trade after 1 days (because of alpha 018)
        if (store$iter > 1){
          #Change Position
          if (alpha006*100 < thr006){
            a006Pos[params$series[i]] <- -round(abs(alpha006)) * posnorm[i]/100
          }
          else if (alpha006*100 > thr006){
            a006Pos[params$series[i]] <- round(abs(alpha006)) * posnorm[i]/100
          }
          else if (alpha006*100 == thr006){
            a006Pos[params$series[i]] <- 0
          }
        }
      }

      else if (rsrs_z > 0.7){
        # if(rsrsPos[params$series[i]] != 0){
        #   rsrsPos[params$series[i]] <- -currentPos[params$series[i]]+ round(rsrs_n * posnorm[i])
        # }
        # else{
        #   rsrsPos[params$series[i]] <- round(rsrs_n * posnorm[i])
        # }
        rsrsPos[params$series[i]] <- round(rsrs_n * (posnorm[i]/68))

      }
      else{
        
          rsrsPos[params$series[i]] <- 0
        
        #rsrsPos[params$series[i]] <- 0

      }

    }



  }

  
  if (store$iter > params$dmalookbacks$long && store$iter > params$macdlookback) {


    startIndexma <-  store$iter - params$dmalookback$long
    startIndexmacd <-  store$iter - params$macdlookback
    startIndexkdj <- store$iter - params$kdjlookback

    for (i in 1:length(params$series)) {


      closeP <- as.vector(newRowList[[params$series[i]]]$Close)
      short_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$short))
      long_ma <- last(SMA(store$cl[startIndexma:store$iter,i],n=params$dmalookbacks$long))
      macd <- as.data.frame(last(MACD(store$cl[startIndexmacd:store$iter,i],
                                      nFast=params$macdFast, nSlow=params$macdSlow,
                                      maType=params$macdMa, percent=TRUE)))
      KDJ<- as.data.frame(last(stoch(store$cl[startIndexkdj:store$iter,i],nFastK = params$nFastK, 
                                     nFastD = params$nFastD,
                                     nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)))
      Kline <- KDJ$fastK * 100
      #using maximum close price and mean(close) to set position normalization
      clall <- matrix(store$cl,ncol = length(params$series))
      closeprice <- apply(clall,2, function(x) mean(x[x>0]))
      largestclose <- max(clall)
      #print(round(largestopen/openprice))
      posnorm1 <- round(largestclose/closeprice)
      #print(dmaPos)
      #print(round(largestAvgAbsDiffs/avgAbsDiffs))
      #when there is a cross in double moving average and macd indicators
      #it is a clear entry point and it also implies the future up-ward trending

      if (last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$short)))
          <
          last((SMA(store$cl[startIndexma:(store$iter-1),i],n=params$dmalookbacks$long)))
          &&short_ma >= long_ma
          && macd$macd >macd$signal ) {

        dmaPos[params$series[i]] <-  abs(short_ma - long_ma)/long_ma*(posnorm1[i])*100

        if(Kline<50 && closeP < short_ma){
          limitOrders1[i] <- -dmaPos[params$series[i]]
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

        dmaPos[params$series[i]] <- -abs(short_ma - long_ma)/long_ma*(posnorm1[i])*100
        if(Kline>50 && closeP > short_ma){
          limitOrders1[i] <- dmaPos[params$series[i]]
          limitPrices1[i] <- short_ma * (1 + 0.05)
        }
      }
      else{
        dmaPos[params$series[i]] <- 0
      }

    }

  }
  #print(dmaPos)
  
  # Store all the values
  store <- updateDmaPos(store, dmaPos)
  store <- updateRsrsPos(store, rsrsPos)
  store <- updateAlpha006Pos(store, a006Pos)
  #Update market orders
  # marketOrders2 <- -currentPos+ rsrsPos
  # marketOrders1 <- allzero + dmaPos
  # marketOrders3 <- -currentPos + a006Pos
  
  marketOrders <- marketOrders + rsrsPos + dmaPos + a006Pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=allzero,limitPrices2=allzero))
}


#---------------------------------------------------------------
#Functions of rsrs strategy;
Cal_RSRS <- function(High, Low, startIndex, iter)
  for (j in startIndex:iter){
    HighList <- High[startIndex:(iter-1)]
    LowList <- Low[startIndex:(iter-1)]
    #OLS linear regression
    fit=lm(HighList~LowList)
    nrsrs <- as.numeric(fit$coefficients[2])
    return(nrsrs)
  }

Cal_RSRS_M <- function(High, Low, startIndex_m,lookback,iter){
  N = startIndex_m
  mrsrs = c()
  for (i in N:iter){ #定义i循环
    
    HighList <- High[(i-lookback):i]
    LowList <- Low[(i-lookback):i]
    #OLS linear regression
    fit=lm(HighList~LowList)
    #store all rsrs values as a vector
    mrsrs=c(mrsrs,as.numeric(fit$coefficients[2]))
  }
  return(mrsrs)
}
#---------------------------------------------------------------


###############################################################################
# All the subsequent functions were designed to simplify and
# improve the readaility of getNewPos();
#
# While not required, this type of function-based approach is advisable
# and these functions will likely have analogues in your strategies
###############################################################################

#-------------------------------------------
#dmaPos records the position of dmaMACD strategy, which will be used for the strategy's stop loss
updateDmaPos <- function(store, dmaPos) {
  store$dmaPos <- dmaPos
  return(store)
}
#rsrsPos records the position of rsrs strategy, which will be used for the strategy's stop loss
updateRsrsPos <- function(store, rsrsPos) {
  store$rsrsPos <- rsrsPos
  return(store)
}
#a006Pos records the pos of Alpha006 strategy
updateAlpha006Pos <- function(store, a006Pos) {
  store$a006Pos <- a006Pos
  return(store)
}
#-------------------------------------------


#-------------------------------------------
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

#-------------------------------------------


#-------------------------------------------
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              vol=initVolStore(newRowList,series),
              high=initHigh(newRowList,series),
              low=initLow(newRowList,series),
              ope=initOpeStore(newRowList,series),
              dmaPos = rep(0,10),
              rsrsPos = rep(0,10),
              a006Pos = rep(0,10)))
}
updateStore <- function(store, newRowList, series, action) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$ope <- updateOpeStore(store$ope,newRowList,series,store$iter)
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$high <- updateHigh(store$high,newRowList,series,store$iter)
  store$low <- updateLow(store$low,newRowList,series,store$iter)
  return(store)
}
