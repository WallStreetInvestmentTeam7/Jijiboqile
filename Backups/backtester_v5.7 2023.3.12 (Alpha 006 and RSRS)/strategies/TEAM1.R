maxRows <- 11000

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  # initialize store at the beginning
  if (is.null(store)){ 
    store <- initStore(newRowList,params$series)    
  }
  
  # update store each transaction day
  store <- updateStore(store, newRowList, params$series)
  
  # initialize market orders and limit orders
  marketOrders <- allzero
  limitOrders <- allzero; limitPos <- allzero; limitPrice <- allzero
  
  # initialize cci-based strategy's market position
  cciPos <- allzero
  # in order to stop all of the cci-based strategy's position
  cciAccumulatePosition <- store$cciAccumulatePosition
  
  #initial momentum strategy position to be all 0
  momentumPos <- allzero
  #store the momentum last transaction
  momentumLastTransaction <- store$momentumPos
  
  #initial Donchian Channel strategy position to be all 0
  dcPos <- allzero
  #store the Donchian Chnannel last transaction
  dcLastTransaction <- store$dcPos
  #parameter for the position sizing of the Donchian Channel
  dcCoefficient <- 50000
  
  #We use correlation coefficient got from momentum strategy to put constraint on DonchianChannel strategy
  #During the first 225(=momentumTestlength) days, momentum strategy is collecting data and 
  #won't work. So only DonchianChannel strategy and cci-based strategy will work during this time
  if(store$iter > params$DCLookback && store$iter <= params$momentumTestlength) {
    
    startIndex <-  store$iter - params$DCLookback
    # Get the maximum close price of all the time series, for position sizing.
    maxCl <- 0
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    for (i in 1:length(params$series)) {
      #close price of each series
      cl <- newRowList[[params$series[i]]]$Close
      #get the daily high and low and close price and merge into one data frame
      Merge <- cbind(store$high[startIndex:store$iter,i],store$low[startIndex:store$iter,i],store$cl[startIndex:store$iter,i])
      Merge <- as.data.frame(Merge)
      
      #change the column name of the data frame to use the DonchainChannel function
      colnames(Merge)[1] <- "High"
      colnames(Merge)[2] <- "Low"
      colnames(Merge)[3] <- "Close"
      
      #calculate the Donchian Channel high and low bound according to the lookback parameter
      dc <- last(DonchianChannel(Merge[,c("High","Low")],n=params$DCLookback,include.lag = TRUE))
      #calculate the simple moving average according to the lookback parameter
      movingAverage <- last(SMA(Merge[,c("Close")],n=params$maLookback))
      
      if (movingAverage< (dc[,3])) {
        #if the moving average is lower than the Donchian Channel Low-bound, long the position for that trading day
        #position sizing is set here
        
        #on the numerator, dcCoefficient represents relative size of the Donchain Channel strategy to the entire strategy
        
        #maxCl/Cl will give a fairly equal weighting to each series on the position sizing
        #eg: if the maximum close price of the 10 series is 1000, series 1 is 10, series 2 is 20
        #the allocation for the series that has 1000 close price will be 1000/1000=1
        #series 1 = 1000/10 =100, series 2 = 1000/ 20 = 50
        #this is to allocate equal weighting for each series
        
        #dc[,3]-movingAverage represents how far the moving average price leaves from the Donchian Channel
        #the bigger the difference, the bigger the position size
        
        #finally divided by close price to gain a fair position for this series
        #so that the sizing of each series will not be influenced by the nominal stock price
        dcPos[params$series[i]] <- dcCoefficient*(maxCl/cl)*(dc[,3]-movingAverage)/cl
        
        #trading stop loss
        #this is to set a hard limit to make sure the single order money amount of Donchian Channel strategy
        #in order to avoid potential great loss caused by oversized transaction
        if (dcPos[params$series[i]]*cl>200000){
          dcPos[params$series[i]] <- 200000/cl
        }
        
        #the following code gains cumulative position
        #if the previous trading day's position size is not 0, add the current position size and previous trading position
        #so that in the next getOrder trading day when there's no trade, the position size can be reduced to 0
        
        #if this is the first day the trading is activated, use the current position
        if(dcLastTransaction[params$series[i]] != 0) {
          dcLastTransaction[params$series[i]] <- dcLastTransaction[params$series[i]] + dcPos[params$series[i]]
          
        } else{
          dcLastTransaction[params$series[i]] <- dcPos[params$series[i]]
          
        }
      }
      else if (movingAverage > (dc[,1])) {
        #if the moving average is bigger than the Donchian Channel High-bound, short the position
        #the parameter applies to the comment above
        dcPos[params$series[i]] <- -dcCoefficient*(maxCl/cl)*(movingAverage-dc[,1])/cl
        if (dcPos[params$series[i]]*cl< -200000){
          dcPos[params$series[i]] <- -200000/cl
        }
        if(dcLastTransaction[params$series[i]] != 0) {
          dcLastTransaction[params$series[i]] <- dcLastTransaction[params$series[i]] + dcPos[params$series[i]]
          
        } else{
          dcLastTransaction[params$series[i]] <- dcPos[params$series[i]]
          
        }
      }
      else{
        dcPos[params$series[i]] = -dcLastTransaction[params$series[i]]
        dcLastTransaction[params$series[i]] <- 0
        
      }
    }
  }
  
  #The following if statement contains the combination of momentum strategy and DonchianChannel strategy
  #Momentum strategy and DonchianChannel strategy won't work at the same time
  #Use the correlation test to determine whether apply momentum strategy or DonchianChannel strategy
  #Momentum strategy is a long-term trend following strategy
  #DonchianChannel strategy is a mean reversion strategy
  
  #If the series pass the correlation test, apply the momentum strategy, 
  #otherwise, apply DonchianChannel strategy
  #The main idea is that if the series passes the correlation test, it indicates that 
  #the following 30 days are expected to follow the past 225(=momentumTestlength) day's trend
  #So probably, DonchianChannel strategy will not catch the low point in this period 
  if(store$iter > params$momentumTestlength) {
    
    testlength <- params$momentumTestlength
    lookback <- params$momentumLookback
    holddays <- params$momentumHolddays
    maxCl <- 0
    
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    #To get the correlation coefficient got from last correlatio test
    corr <- store$cor
    
    for (i in 1:length(params$series)) {
      
      startIndex <-  store$iter - params$DCLookback
      startDay = 30*(store$iter%/%30)-90
      endDay = 30*(store$iter%/%30)
      
      #every 30 days, the momentum strategy will do a correlation test and decide whether to long or to short
      if(store$iter %% 30 == 1) {
        #The following if stratement is used to stop the momentum strategy's position
        #if the series passed correlation test last time and the position kept being held in the past 30 days
        if(corr[params$series[i]] > 0.2){
          if(momentumLastTransaction[params$series[i]] != 0) {
            momentumPos[params$series[i]] <- -momentumLastTransaction[params$series[i]]
            momentumLastTransaction[params$series[i]] <- 0
          }
        }
        corr[params$series[i]] = 0
        #initialize two parameters used for the correlation test
        lookback_return <- c()
        holddays_return <- c()
        
        #lookback return = the close price of one day of series i- the close price 45(=lookback) days ago
        #holddays return = the return we get if we hold 1 position of series i for 90(=holddays) days
        #right after we get the lookback return
        #In the past 225(=testlength) days, get 90(testlength-lookback-holddays) pairs of lookback return
        #and corresponding holddays return
        for (k in 1:90) {
          lookback_return <- c(lookback_return, store$cl[store$iter+k-(testlength-lookback+2),i]-store$cl[store$iter+k-testlength-1,i])
          holddays_return <- c(holddays_return, store$cl[store$iter+k-lookback-2,i]- store$cl[store$iter+k-(testlength-lookback+1),i])
        }
        
        #Apply cor() function to get to correlation coefficient of the lookback return and holddays return
        #A correlation coefficient bigger than 0.2 indicates a strong trend in the past 225 days
        corr[params$series[i]] = cor(lookback_return,holddays_return)
        
        #When we apply the momentum strategy, we have to consider a special situation where 
        #though we caught a trend in the past 225 days, the trend stoped a few days ago,
        #in this case, stop loss will be triggered right after we hold a position.
        #We try to aviod this situation by comparing cl price of series i and EMA(store$cl[store$iter-89:store$iter,i], 90)[91]
        if(cor(lookback_return,holddays_return)>=0.2) {
          if(i != 3){
            #If the series i pass the correlation test, 
            #long if the close price of series i is bigger than the cl price of series i 90 days ago
            if (store$cl[store$iter-1,i] - store$cl[store$iter-90,i] > 0 && store$cl[store$iter,i] >= EMA(store$cl[store$iter-89:store$iter,i], 90)[91]) {
              limitPrice[params$series[i]] <-newRowList[[params$series[i]]]$Close
              limitPos[params$series[i]] <- 100000 %/% newRowList[[params$series[i]]]$Close
              #record the position of the transaction which will be stored in store
              momentumLastTransaction[params$series[i]] <- limitPos[params$series[i]]
            }
            #If the series i pass the correlation test, 
            #short if the close price of series i is smaller than the cl price of series i 90 days ago
            else if(store$cl[store$iter-1,i] - store$cl[store$iter-90,i] < 0 && store$cl[store$iter,i] <= EMA(store$cl[store$iter-89:store$iter,i], 90)[91]) {
              limitPrice[params$series[i]] <- newRowList[[params$series[i]]]$Close
              limitPos[params$series[i]] <- -(100000 %/% newRowList[[params$series[i]]]$Close)
              #record the position of the transaction which will be stored in store
              momentumLastTransaction[params$series[i]] <- limitPos[params$series[i]]
            }
          }
        }
      }
      
      #If the series i failed in the correaltion test last time,
      #apply DonchianChannel strategy in the following 30 days.
      #The code below is just the same with line 59-133, has been explained
      else if(corr[i]< 0.2) {
        cl <- newRowList[[params$series[i]]]$Close
        Merge <- cbind(store$high[startIndex:store$iter,i],store$low[startIndex:store$iter,i],store$cl[startIndex:store$iter,i])
        Merge <- as.data.frame(Merge)
        
        colnames(Merge)[1] <- "High"
        colnames(Merge)[2] <- "Low"
        colnames(Merge)[3] <- "Close"
        
        dc <- last(DonchianChannel(Merge[,c("High","Low")],n=params$DCLookback,include.lag = TRUE))
        movingAverage <- last(SMA(Merge[,c("Close")],n=params$maLookback))
        closePrice <-Merge[,c("Close")]
        
        if (movingAverage< (dc[,3])) {
          
          dcPos[params$series[i]] <- dcCoefficient*(maxCl/cl)*(dc[,3]-movingAverage)/cl
          if (dcPos[params$series[i]]*cl>200000){
            dcPos[params$series[i]] <- 200000/cl
          }
          if(dcLastTransaction[params$series[i]] != 0) {
            dcLastTransaction[params$series[i]] <- dcLastTransaction[params$series[i]] + dcPos[params$series[i]]
            
          } else{
            dcLastTransaction[params$series[i]] <- dcPos[params$series[i]]
            
          }
        }
        
        else if (movingAverage > (dc[,1])) {
          
          dcPos[params$series[i]] <- -dcCoefficient*(maxCl/cl)*(movingAverage-dc[,1])/cl
          if (dcPos[params$series[i]]*cl< -200000){
            dcPos[params$series[i]] <- -200000/cl
          }
          if(dcLastTransaction[params$series[i]] != 0) {
            dcLastTransaction[params$series[i]] <- dcLastTransaction[params$series[i]] + dcPos[params$series[i]]
            
          } else{
            dcLastTransaction[params$series[i]] <- dcPos[params$series[i]]
            
          }
        }
        
        else{
          dcPos[params$series[i]] = -dcLastTransaction[params$series[i]]
          
          dcLastTransaction[params$series[i]] <- 0
        }
      }
      
      #If the series i passed the correaltion test last time,momentum strategy was applied
      #test whether to stop loss or not every day in the following 30 days
      else if(corr[i]>=0.2) {
        #Limit order may fail, so if the series i passed the correlation test,
        #at the next day, test if the limit order succeeds.
        if(store$iter %% 30 == 2){
          #if the position we stored in store is bigger than 0, we long last day
          #If today's low price of series i is bigger than the cl price last day, limit orders fail
          #So set the recorded position to zero in store
          if(momentumLastTransaction[params$series[i]] > 0){
            if(newRowList[[params$series[i]]]$Low > store$cl[store$iter-1,params$series[i]]){
              momentumLastTransaction[params$series[i]] <- 0
            }
          }
          #if the position we stored in store is smaller than 0, we short last day
          #If today's low price of series i is smaller than the cl price last day, limit orders fail
          #set the recorded position to zero in store
          else if(momentumLastTransaction[params$series[i]] < 0){
            if(newRowList[[params$series[i]]]$High < store$cl[store$iter-1,params$series[i]]){
              momentumLastTransaction[params$series[i]] <- 0
            }
          }
        }
        
        #The following if and else if statement is momentum strategy's stop loss and stop win.
        if(momentumLastTransaction[params$series[i]] > 0 && store$cl[endDay,i] >= EMA(store$cl[startDay:endDay,i], 90)[91]){
          if (newRowList[[params$series[i]]]$Close <= EMA(store$cl[startDay:endDay,i], 90)[91]) {
            momentumPos[params$series[i]] <- -(100000 %/% store$cl[endDay+1,params$series[i]])
            momentumLastTransaction[params$series[i]] <- 0
          } else if(newRowList[[params$series[i]]]$Close >= 1.13 * store$cl[endDay,i]){
            momentumPos[params$series[i]] <- -(100000 %/% store$cl[endDay+1,params$series[i]])
            momentumLastTransaction[params$series[i]] <- 0
          }
        }
        else if (momentumLastTransaction[params$series[i]] < 0 && store$cl[endDay,i] <= EMA(store$cl[startDay:endDay,i], 90)[91]) {
          if (newRowList[[params$series[i]]]$Close >= EMA(store$cl[startDay:endDay,i], 90)[91]) {
            momentumPos[params$series[i]] <- 100000 %/% store$cl[endDay+1,params$series[i]]
            momentumLastTransaction[params$series[i]] <- 0
          }else if (newRowList[[params$series[i]]]$Close <= 0.87 * store$cl[endDay,i]) {
            momentumPos[params$series[i]] <- 100000 %/% store$cl[endDay+1,params$series[i]]
            momentumLastTransaction[params$series[i]] <- 0
          }
        }
      }
    }
    
    limitOrders <- limitOrders + limitPos
    #Update the correlation coefficient in store got from the correlation test if new test is committed.
    store <- updateCorr(store,corr)
  }
  
  # The following if statement contains a strategy 
  # focused on the Commodity Channel Index and the kdj(stochastic oscillator) indicator.
  # The core idea is mean reversion, 
  # when the market declines, it will continue to add positions, and vice versa.
  
  # Long and Short conditions: based on CCI() and stoch() functions
  # Position management: performed by the difference between 
  # the cci value and the cci value of yesterday.
  # Stop loss: BBands() function. If the upper band or lower band touched, 
  # then clean the position.
  if (store$iter > params$cciLookback) {
    
    startIndex <-  store$iter - params$cciLookback
    
    # Loop all the time series
    for (i in 1:length(params$series)) {
      # Get the latest close price
      cl <- newRowList[[params$series[i]]]$Close
      
      # Get the cci value list and KD value list, 
      # for recording today's cci/kdj value and yesterday's value
      # cciYesterday is used to manage the position size
      # kdjYesterday is used to calculate J line, and function as 
      # one of the control statement
      cciList <- CCI(store$cl[startIndex:store$iter,i],
                     n=params$cciLookback,c=params$cciMeanDev)
      cci <- last(cciList)
      cciYesterday <- cciList[nrow(cciList)-1,]
      
      KDlist <- stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD,
                      nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)
      KD <- last(KDlist)
      KDYesterday<- last(KDlist[-nrow(KDlist),])
      
      # Calculate the J line through the KD indicator, 
      # for catching the lowest point in the short period
      Jline <- 3*KD[,'fastK']- 2*KD[,'fastD']
      JlineYesterday<- 3*KDYesterday[,'fastK']- 2*KDYesterday[,'fastD']
      
      # Long operation
      # When the value of cci falls into the oversold zone and 
      # the J line crosses the Jlow from bottom to top, 
      # we think the stock price will start to rebound, so long operation is taken
      if (cci < params$cciOverSold && !is.na(cci) && !is.na(cciYesterday)
          && JlineYesterday < params$Jlow && Jline > params$Jlow
          && !is.na(Jline)&& !is.na(JlineYesterday)) {
        
        # Add long position in market order
        # The position is managed by the difference between the cci value of two days, 
        # the larger the difference, the more positions are added. 
        # Because we believe that from a short-term perspective, 
        # a big fall must be accompanied by a big rise.
        cciPos[params$series[i]] <- 1*(abs(round(cci-cciYesterday)))/cl
        
        # Record in store, for stop loss
        cciAccumulatePosition[params$series[i]] <-
          cciAccumulatePosition[params$series[i]] + cciPos[params$series[i]]
      }
      
      # Short operation
      # When the value of cci rises into the overbought zone and 
      # the J line crosses the Jhigh from top to bottom, 
      # we think the stock price will start to rebound, so short operation is taken
      else if (cci > params$cciOverBought && !is.na(cci) && !is.na(cciYesterday)
               && JlineYesterday > params$Jhigh && Jline < params$Jhigh
               &&!is.na(Jline)&& !is.na(JlineYesterday)) {
        
        # Add short position in market order
        # The explaination is the same with above
        cciPos[params$series[i]] <- -1*(abs(round(cci-cciYesterday)))/cl
        
        # Record in store, for stop loss
        cciAccumulatePosition[params$series[i]] <-
          cciAccumulatePosition[params$series[i]] + cciPos[params$series[i]]
      }
      
      # Stop loss
      # When the close price touches the upper and lower rails 
      # of the Bollinger Band, we clean all the position of this strategy.
      if (store$iter > params$BBLookback) {
        
        startIndex <-  store$iter - params$BBLookback
        
        # Get the latest close price
        cl <- newRowList[[params$series[i]]]$Close
        
        # Get the Bollinger Band rails
        bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                              n=params$BBLookback,sd=params$BBsd))
        
        if (cl < bbands[,"dn"]) {
          
          # Reset the position of this strategy
          cciPos[params$series[i]] <- -cciAccumulatePosition[params$series[i]]
          
          # Clear the accumulated value
          # so that the position of this sub-strategy can be accumulated again
          cciAccumulatePosition[params$series[i]] <- 0
        }
        else if (cl > bbands[,"up"]) {
          
          # Reset the position of this strategy
          cciPos[params$series[i]] <- -cciAccumulatePosition[params$series[i]]
          
          # Clear the accumulated value
          # so that the position of this sub-strategy can be accumulated again
          cciAccumulatePosition[params$series[i]] <- 0
        }
      }
    }
  }
  
  # Store all the values we need
  store <- updateMomentumPos(store,momentumLastTransaction)
  store <- updateCciPos(store,cciPos,cciAccumulatePosition)
  store <- updateDcPos(store, dcLastTransaction)
  
  # Set the market order to be the sum of all the strategies
  marketOrders <- marketOrders + momentumPos + dcPos + cciPos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders,limitPrices1=limitPrice,
              limitOrders2=allzero,limitPrices2=allzero))
}


initStore <- function(newRowList,series) {
  return(list(iter=0,high=initHighStore(newRowList,series),low=initLowStore(newRowList,series),cl=initClStore(newRowList,series)
              , cor = rep(0,10), momentumPos = rep(0,10),
              dcPos = rep(0,10), cciPos = rep(0,10), cciAccumulatePosition = rep(0,10)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$high <- updateHighStore(store$high,newRowList,series,store$iter) 
  store$low <- updateLowStore(store$low,newRowList,series,store$iter) 
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}

#momentumPos records the position of momentum strategy, which will be used for the strategy's stop loss
updateMomentumPos <- function(store, momentumPos) {
  store$momentumPos <- momentumPos
  return(store)
}
#DcPos records the position of DonchianChannel strategy, which will be used for the strategy's stop loss
updateDcPos <- function(store, dcPos) {
  store$dcPos <- dcPos
  return(store)
}
#cciAccumulatePosition records the position of cci-based strategy, which will be used for the strategy's stop loss
updateCciPos <- function(store, cciPos, cciAccumulatePosition) {
  store$cciPos <- cciPos
  store$cciAccumulatePosition <- cciAccumulatePosition
  return(store)
}
#cor records the correaltion coefficient of each series got from last correaltion test,
#which is used for momentum strategy's stop loss and constrainting the transaction of DonchianChannel strategy
updateCorr <- function(store,corr) {
  store$cor <- corr
  return(store)
}
initHighStore  <- function(newRowList,series) {
  HighStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HighStore)
}
updateHighStore <- function(HighStore, newRowList, series, iter) {
  for (i in 1:length(series))
    HighStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HighStore)
}
initLowStore  <- function(newRowList,series) {
  LowStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LowStore)
}
updateLowStore <- function(LowStore, newRowList, series, iter) {
  for (i in 1:length(series))
    LowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LowStore)
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