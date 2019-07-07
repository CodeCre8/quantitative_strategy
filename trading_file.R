
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
# Stage 1 - Input
# Initial Inputs (parameters): Tick Size, Box Size & Reversal Size.
# Tick Size: The minimum price movement reported by the exchange and it is usually fixed at 0.05.
# Box Size: A, indicator that is a multple of tick size and number of boxes.
# Reversal Size: An indicator that is a multiple of box size and number of boxes.
# Benchmark: It is updated with every significant price movement and what the closing prices are compared to.
#================================================================================================
# Stage 2 - Comparison
# 1. Diff in prices & benchmark
#================================================================================================


# Initialize Input Parameters
#================================================================================================
# Tick Size
ticksize <- 0.05
# Box Size in terms of number of boxes. It denotes the box size which is the 
# minimal change required to update the value of benchmark
bsiz_box <- 4
# Reversal Size in terms of number of boxes. It denotes the reversal size which is the 
# mininal change required to generate signal
brev_box <- 3
# Create Box Size in terms of prices (Tick Size)
bsiz <- bsiz_box * ticksize
# Create Reversal Size in terms of prices (Tick Size)
brev <- brev_box * ticksize


# Initialize counters & parameters
#================================================================================================
bench <- tradedata$Price[i]
currpos <- 0
nrows <- nrow(tradedata)


# Initialize output variables
#================================================================================================
tradedata$signal <- ""
tradedata$tqty <- 0
tradedata$tprice <- 0
tradedata$tprofit <- 0
tradedata$mtm <- 0


# Initialize the benchmark column
#================================================================================================
tradedata$benchmark <- 0


# Load and plot dataset
#================================================================================================
tradedata <- read.csv("http://assets.datacamp.com/course/quantinsti/data_ch3.csv", header = TRUE, stringsAsFactors = FALSE)

plot(tradedata$Price, type="l", col = "blue")













