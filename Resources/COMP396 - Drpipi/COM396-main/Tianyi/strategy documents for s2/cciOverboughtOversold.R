# "cciOverboughtOversold"=list(cciLookback=20, series=c(1,2,3,4,5,6,8,9,10),
#                                                  cciMeanDev=0.015, kdjLookback=20,
#                                                  cciOverSold=-100, cciOverBought=100,
#                                                  nFastK=14,nFastD=3,nSlowD=5,Jhigh=0.8,Jlow=0.2)
maxRows <- 3100

getOrders <- function(store,newRowList,currentPos,info,params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- allzero; cciPos <- allzero
  
  if (store$iter > params$cciLookback) {
    
    startIndex <-  store$iter - params$cciLookback
    
    # Position Sizing
    maxCl <- 0
    for (i in 1:length(params$series)){
      maxCl <- max(maxCl,newRowList[[params$series[i]]]$Close)
    }
    
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      # KDJ
      op <- newRowList[[params$series[i]]]$Open
      
      cciList <- CCI(store$cl[startIndex:store$iter,i],
                     n=params$cciLookback,c=params$cciMeanDev)
      cci <- last(cciList)
      cciYesterday <- cciList[nrow(cciList)-1,]
      
      KDlist <- stoch(store$cl[startIndex:store$iter,i],
                      nFastK = params$nFastK, nFastD = params$nFastD, 
                      nSlowD = params$nSlowD, bounded = TRUE,smooth = 1)
      
      KD <- last(KDlist)
      
      #compute the yesterday KD
      KDYesterday<- last(KDlist[-nrow(KDlist),])
      
      Jline <- 3*KD[,'fastK']- 2*KD[,'fastD'] 
      JlineYesterday<- 3*KDYesterday[,'fastK']- 2*KDYesterday[,'fastD'] 
      
      if (cci < params$cciOverSold && !is.na(cci) && !is.na(cciYesterday)
          && JlineYesterday < params$Jlow && Jline > params$Jlow 
          && !is.na(Jline)&& !is.na(JlineYesterday)) {
        
        # pos[params$series[i]] <- abs(round(cci-cciYesterday))
        cciPos[params$series[i]] <- 1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
        store <- updateCciPos(store,cciPos)
      }

      else if (cci > params$cciOverBought && !is.na(cci) && !is.na(cciYesterday)
               && JlineYesterday > params$Jhigh && Jline < params$Jhigh
               &&!is.na(Jline)&& !is.na(JlineYesterday)) {
        # pos[params$series[i]] <- -abs(round(cci-cciYesterday))
        cciPos[params$series[i]] <- -1*(maxCl/last(cl))*
          (abs(round(cci-cciYesterday)))/last(cl)
        store <- updateCciPos(store,cciPos)
      }
    }
  }
  
  marketOrders <- marketOrders + cciPos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,
              limitPrices1=allzero,
              limitOrders2=allzero,
              limitPrices2=allzero))
}

updateCciPos <- function(store, cciPos) {
  store$cciPos <- cciPos
  return(store)
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
  return(list(iter=0,cl=initClStore(newRowList,series),
              cciPos=cbind(0,0,0,0,0,0,0,0,0,0)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}