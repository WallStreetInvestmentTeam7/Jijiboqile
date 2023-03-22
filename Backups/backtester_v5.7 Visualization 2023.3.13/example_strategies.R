example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "TMA",
                        "dmaMACD",
                        'WTY',
                        "defRsrs",
                        "alpha101",
                        "combination",
                        "TEAM1",
                        "combination2"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi"=list(lookback=10,threshold=30, series = 1:10),
                    "bbands_contrarian"=list(lookback=20,sdParam=2.0,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "TMA"= list(lookbacks=list(short=as.integer(10),
                                               medium=as.integer(50),                          
                                               long=as.integer(250)),series=1:10),
                    "dmaMACD"=list(dmalookbacks = list(short=as.integer(10),
                                                    long=as.integer(50)),
                                   macdlookback = as.integer(50),
                                   macdFast=12, macdSlow=26, macdSig=9, macdMa="EMA", series = 1:10,
                                   posSizes=as.numeric(list(95,3341,15,146,20,19956,1,2159,8,94))),
                    "WTY"=list(cciLookback=20, macdLookback=50, series=7, 
                                   cciMeanDev=0.015, 
                                   macdFast=12, macdSlow=26, macdSig=9, macdMa="SMA"),
                    "defRsrs"=list(lookback = 18,lookback_m= 50, series = 1:10,posSizes=as.numeric(list(95,3341,15,146,20,19956,1,2159,8,94))),
                    "alpha101" = list(series=c(1,2,3,4,5,6,7,8,9,10), thr006= 40, obday= 20,posSizes=as.numeric(list(95,3341,15,146,20,19956,1,2159,8,94))),
                    "combination" = list(dmalookbacks = list(short=as.integer(10),
                                                             long=as.integer(50)),
                                         macdlookback = as.integer(50),
                                         macdFast=12, macdSlow=26, macdSig=9, macdMa="EMA", 
                                         rsrs_lookback = 18,rsrs_lookback_m= 50,
                                         series = 1:10,
                                         thr006=62,
                                         obday=30), 
                    "TEAM1" = list(series=1:10,
                                   cciLookback=20, cciMeanDev=0.015, 
                                   cciOverSold=-130, cciOverBought=130,
                                   BBLookback=50,BBsd=5,
                                   kdjLookback=20,nFastK=14,nFastD=3,
                                   nSlowD=5,Jhigh=0.8,Jlow=0.2,
                                   DCLookback=19,maLookback=3,
                                   momentumLookback=90, momentumTestlength=225,
                                   momentumHolddays=45),
                    "combination2" = list(dmalookbacks = list(short=as.integer(20),
                                                              long=as.integer(150)),
                                          macdlookback = as.integer(50),
                                          macdFast=12, macdSlow=26, macdSig=9, macdMa="EMA", 
                                          rsrs_lookback = 18,rsrs_lookback_m= 50,
                                          series = 1:10,
                                          thr006=62,
                                          obday=30))

                    

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
