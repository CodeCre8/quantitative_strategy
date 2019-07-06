# ============================================================================
# CODE A BASIC TRADING STRATEGY
# ============================================================================
# ============================================================================
# 1. Define the Strategy
# If Price movement from time "t1" to time "t2" is greater than or equal to 
# "parameter" trigger an order which is opposite to the existing one.
# If the existing order is "bought", place a order for sell.
# If the existing order is "sold", place a order for buy.
# ============================================================================
# ============================================================================
# 2. Strategy Assumptions
# Only one trading lot is allowed
# Always one trade is open after first signal
# - Trading quantity for 1st signal = 1 lot
# - Trading quantity for subsequent signals = 2 lots
# EXAMPLE: After 1st signal (buy 1 lot, always a buy), 
#          2nd signal (sell 2 lots): 1 sell cancels 1 buy, net 1 lot sold
#          3rd signal (buy 2 lots): 1 buy cancels a sell, net 1 lot buy
#          ...
# Trade price is always market order
# - When signal says "sell": Bid Price
# - When signal says "buy": Ask Price
# ============================================================================
# ============================================================================
# 3. Strategy Parameter and tradedata
# PARAMETER: A pre-determined absolute difference between p(t2) - p(t1)
# INPUT DATA: Prices - OHLC but only using Close
#             Bid price - Best price at which someone is willing to buy
#                       - Sell orders at this price
#             Ask price - Best price at which someone is willing to sell
#                       - Buy orders at this price
#             Time stamps - Use serial number(1,2,3,4,...) as proxy for time stamp
# OUTPUT DATA: signal
#              tqty
#              tprice
#              tprofit
#              mtm (mark to market) - Total profit from the strategy up till a row
# ============================================================================
# ============================================================================
# 4. Trading Signal Generation Logic
# 4.1 Whether to buy or sell
# 4.2 At what price
# 4.3 How much do we buy or sell
# This strategy is to look at the differences between the 
# closing prices and the existing positions to generate the buy or sell signals
# CRITERION: if p(t2)-p(t1) >= parameter then buy/sell else no trade
# CURRENT POSITION:
# If the closing price of the previous day is greater than today then 
# currpos = 0 (No position)
# currpos = 1 (Bought position)
# currpos = -1 (Sold position)
# Buy/Sell: If not bought, buy now, else sell now
# ============================================================================
# ============================================================================
# 5. Output Variables
# 5.1 During execution: 
#   - tqty: Trading quantity is the number of trading lots
#   - tprice: Trading price for the marker order
#   - tprofit: Trading profit calculated for the order placed/executed in a specific row
# 5.2 After execution:
#   - mtm: Add tprice from all the rows till row i + currpos in row i * closing price in row i
#   - Traded lots: Absolute sum of tqty + 1


# Load the dataset
# ============================================================================
tradedata <- read.csv("http://assets.datacamp.com/course/quantinsti/data_ch2.csv", header = TRUE, stringsAsFactors = FALSE)

# Initialize the variables for the PNL (profit and loss) table to the dataset
# ============================================================================
# signal for the particular row, buy, sell, or no change
tradedata$signal <- ""
# quantity to be traded
tradedata$tqty <- 0
# price at which a trade would take place.
# For selling, this equals the best bid, for buying this is the best ask.
tradedata$tprice <- 0
# profit for a particular trade.
tradedata$tprofit <- 0
# cumulative profit after squaring off the last open position till that point
tradedata$mtm <- 0

# Define & Initialize Counters
# ============================================================================
# total number of rows in the dataframe
nrows <- nrow(tradedata)
# current position: 0 - no position, 1 - bought position, -1 - sold position
currpos <- 0

# The logic behind the if condition for signal generation
# for row 2 to nrows in data frame:
#   if absolute price difference >= parameter:
#     if condition for buying is met:
#       update the output columns to "buy"
#     else
#       update the output columns to "sell"
#     endif
#   else
#     update the output columns to "change"
#
#   update profit columns
#
#   endfor


# Set up the function to run the basic function with if condition for signal generation
# ============================================================================
run_basic_strategy <- function (tradedata, parameter) {
  for ( i in 2:nrows ) {
    # Test the whether there is a trigger based on the parameter
    if ( abs( tradedata$Price[i] - tradedata$Price[i-1] ) >= parameter ) {
      # if the diff between the prices is greater than parameter &
      # Test the state of currpos
      if ( currpos < 1 ) {
        # The currpos is not bought
        tradedata$signal[i] <- "buy"
        tradedata$tprice[i] <- tradedata$Ask[i]
        tradedata$tqty[i] <- if ( currpos == 0 ) { 1 } else { 2 }
        currpos <- currpos+tradedata$tqty[i]
      } 
      else {
        # currpos is sold
        tradedata$signal[i] <- "sell"
        tradedata$tprice[i] <- tradedata$Bid[i]
        tradedata$tqty[i] <- if ( currpos == 0 ) { -1 } else { -2 }
        currpos <- -1
      }
    }
    else {
      # no change
      tradedata$signal[i] <- "no change"
      tradedata$tprice[i] <- 0
      tradedata$tqty[i] <- 0
    }
    # tprofit
    tradedata$tprofit[i] <- - tradedata$tqty[i] * tradedata$tprice[i]
    # mtm
    tradedata$mtm[i] <- sum(tradedata$tprofit[1:i]) + currpos * tradedata$Price[i]
  }
  num_traded_lots <- sum(abs(tradedata$tqty)) + 1
  mtm_final <- tradedata$mtm[nrows]
  print(tradedata)
  print(num_traded_lots)
  print(mtm_final)
}



# Calculate Profit & Traded Lots
# ============================================================================
# Did my strategy got any fills? num_traded_lots
# Did my strategy make any profit? mtm_final
run_basic_strategy(tradedata, 9)
