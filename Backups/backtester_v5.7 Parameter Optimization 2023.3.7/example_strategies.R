example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "TMA",
                        "dmaMACD",
                        "WTY",
                        "alpha101"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=30,series=1),
                    "bbands_contrarian"=list(lookback=20,sdParam=2.0,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "TMA"= list(lookbacks=list(short=as.integer(10),
                                               medium=as.integer(50),                          
                                               long=as.integer(250)),series=1:10),
                    "dmaMACD"=list(dmalookbacks = list(short=as.integer(50),
                                                    long=as.integer(300)),
                                   macdlookback = as.integer(50),
                                   macdFast=12, macdSlow=26, macdSig=9, macdMa="SMA", series = 1:10),
                    "WTY"=list(cciLookback=20, macdLookback=50, series=c(1,2,3,4,5,6,7,8,9,10), 
                                   cciMeanDev=0.015, 
                                   macdFast=12, macdSlow=26, macdSig=9, macdMa="SMA"),
                    "alpha101"=list(series=1:10, obday=35,posSizes=as.numeric(list(323,6989,40,374,47,8244,1,6681,14,83)))
)


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
