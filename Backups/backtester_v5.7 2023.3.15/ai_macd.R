library(tidyverse)
library( quantmod )

# Load the stock data
symbol <- "AAPL"
getSymbols( symbol )

# Create a data frame
df <- data.frame( Date = index(get(symbol)), coredata(get(symbol)) )

# Calculate the Moving Average Convergence Divergence (MACD)
df$macd <- MACD( Cl(get(symbol)), nFast=12, nSlow=26, nSig=9)$macd
df$signal <- MACD( Cl(get(symbol)), nFast=12, nSlow=26, nSig=9)$signal

# Calculate the simple moving averages
df$sma_long <- SMA( Cl(get(symbol)), n=200 )
df$sma_short <- SMA( Cl(get(symbol)), n=50 )
trading_strategy <- function( df ) {
  
  # Initialize variables
  position <- "neutral"
  pnl <- 0
  
  for ( i in 2:nrow(df) ) {
    
    # Check for a long signal
    if ( df$sma_short[i] > df$sma_long[i] && df$macd[i] > df$signal[i] && position == "neutral" ) {
      position <- "long"
      pnl <- pnl - df$Adj.Close[i] * 100
    }
    
    # Check for a short signal
    if ( df$sma_short[i] < df$sma_long[i] && df$macd[i] < df$signal[i] && position == "long" ) {
      position <- "neutral"
      pnl <- pnl + df$Adj.Close[i] * 100
    }
    
    # Check for a stop loss
    if ( position == "long" && df$Adj.Close[i] < ( df$Adj.Close[i-1] * 0.95 ) ) {
      position <- "neutral"
      pnl <- pnl + df$Adj.Close[i] * 100
    }
    
  }
  
  return( pnl )
  
}
pnl <- trading_strategy( df )
cat( "Profit/Loss:", pnl, "\n" )