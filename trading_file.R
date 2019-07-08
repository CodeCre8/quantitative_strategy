
#================================================================================================
# TREND FOLLOWING STRATEGY
# Function: 
# 1. Track the price movement
# 2. Check to see if a minimum pre-determined amount called "reversal size" has been crossed
# Logic: 
#   Update "benchmark" with the consistent price movement and No Change. 
#   Wait for significant directional change in price movement.
#   If the directional change is >= "reversal size" then Buy/Sell.
# Purpose: 
# Make money to buy at the bottom of an upward price moment.
# Make money to sell at the top of a downward price moment.
#================================================================================================
#================================================================================================
# Stages in the trading strategy
# 1. Input
# 2. Comparsion
# 3. Signal
#================================================================================================
# Inputs
# Tick Size: The minimum price movement reported by the exchange and it is usually fixed at 0.05.
# Box Size: A, indicator that is a multple of tick size and number of boxes.
# Reversal Size: An indicator that is a multiple of box size and number of boxes.
# Benchmark: It is updated with every significant price movement and what the closing prices are compared to.
#================================================================================================
# Comparison
# Check diff in prices & benchmark
#================================================================================================
# Signal
# Trade lots & mark to market
#================================================================================================


# Load and plot dataset
#================================================================================================
tradedata <- read.csv("http://assets.datacamp.com/course/quantinsti/data_ch3.csv", header = TRUE, stringsAsFactors = FALSE)

plot(tradedata$Price, type="l", col = "blue")


# Create a function that takes 3 arguments: trading data, bsiz_box, brev_box
#================================================================================================
adaptive_strategy <- function(tradedata, bsiz_box, brev_box) {
  
  # Initialize Input Parameters
  # Tick Size
  ticksize <- 0.05
  # Create Box Size in terms of prices (Tick Size)
  bsiz <- bsiz_box * ticksize
  # Create Reversal Size in terms of prices (Tick Size)
  brev <- brev_box * ticksize
  
  # Initialize output variables
  tradedata$signal <- ""
  tradedata$tqty <- 0
  tradedata$tprice <- 0
  tradedata$tprofit <- 0
  tradedata$mtm <- 0
  
  # Initialize the benchmark column
  tradedata$benchmark <- 0
  
  # Initialize counters & parameters
  bench <- tradedata$Price[1]
  currpos <- 0
  nrows <- nrow(tradedata)
  
  # Write a For loop to do the following:
  # 1. Signal Generation
  # 2. Benchmark Update
  # 3. tprofit & mtm update
  # 4. Output trade lots & mark to market
  #================================================================================================
  # Condition for changing benchmark
  # Condition 1: currpos > 0 && Price[i] - benchmark[i] >= bsiz
  # Condition 2: currpos < 0 && benchmark[i] - Price[i] >= bsiz
  #================================================================================================
  # Pseudo code for the For loop
  
  #   for row 2 to nrows in data frame:
  #     assign updated bench value to benchmark[i]
  #     compare benchmark[i] to the close price[i] with brev:
  #       if condition for "buy" is met:
  #         update tradedata columns to reflect "buy"
  #       else if condition for "sell" is met:
  #         update tradedata columns to reflect "sell"
  #       else
  #         update tradedata columns to reflect "no change"
  #       endif
  #     update the value for bench
  #     output profit column
  #   endfor
  
  for ( i in 2:nrows ) {
    # assign updated bench value to benchmark
    tradedata$benchmark[i] <- bench
    # if condition for "buy" is met:
    if (( currpos < 1 ) && ( tradedata$Price[i] - tradedata$benchmark[i] ) >=  brev ) {
      tradedata$signal[i] <- "buy"
      tradedata$tqty[i] <- if ( currpos == 0 ) { 1 } else { 2 }
      tradedata$tprice[i] <- tradedata$Ask[i]
      currpos <- currpos + tradedata$tqty[i]
    }
    # else if condition for "sell" is met:
    else if (( currpos > -1 ) && ( tradedata$benchmark[i] - tradedata$Price[i] ) >= brev ) {
      tradedata$signal[i] <- "sell"
      tradedata$tqty[i] <- if ( currpos == 0 ) { -1 } else { -2 }
      tradedata$tprice[i] <- tradedata$Bid[i]
      currpos <- currpos + tradedata$tqty[i]
    }
    # update tradedata columns to reflect "no change"
    else {
      tradedata$signal[i] <- "no change"
      tradedata$tqty[i] <- 0
      tradedata$tprice[i] <- 0
    }
    # update benchmark only after the signal generation code has run
    if(currpos > 0 && tradedata$Price[i] - bench >= bsiz) {
      bench <- bench + (round(100 * (tradedata$Price[i] - bench),0) %/% (100 * bsiz)) * bsiz
    } 
    else if (currpos < 0 && bench - tradedata$Price[i] >= bsiz) {
      bench <- bench - (round(100 * (bench - tradedata$Price[i]),0) %/% (100 * bsiz)) * bsiz
    }
    
    tradedata$tprofit[i] <- - tradedata$tqty[i] * tradedata$tprice[i]
    tradedata$mtm[i] <- sum(tradedata$tprofit[1:i]) + currpos * tradedata$Price[i]
  }
  return(tradedata)
  print(tradedata$pro)
}

adaptive_strategy(tradedata, 4, 3)


























