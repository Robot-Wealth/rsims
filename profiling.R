# profiling
library(profvis)
library(microbenchmark)
library(tidyverse)

load("~/rsims/data/backtest_df.RData")

# get weights as a wide matrix (could do equal weight, in proportion to factor/signal, top n etc)
backtest_theo_weights <- backtest_df %>%
  select(date, starts_with("theo_weight_")) %>%
  data.matrix()

# NA weights should be zero
backtest_theo_weights[is.na(backtest_theo_weights)] <- 0

# get prices as a wide matrix
backtest_prices <- backtest_df %>%
  select(date, starts_with("price_")) %>%
  data.matrix()

# fees - reasonable approximation of actual costs (spread + market impact + commission)
fees <- tribble(
  ~tier, ~fee,
  0, 0.,  # use for cost-free simulations
  1, 0.0010,
  2, 0.0015,
  3, 0.0020,
  4, 0.0025,
  5, 0.0030,
  6, 0.0035,
  7, 0.0040
)

# simulation parameters
initial_cash <- 10000
capitalise_profits <- FALSE  # remain fully invested?
trade_buffer <- 0.
fee_tier <- 0.
commission_pct <- fees$fee[fees$tier==fee_tier]

# simulation benchmarking and optimisation
microbenchmark(
  cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits),
  times = 10
)
# Unit: milliseconds
#      min       lq     mean   median       uq      max  neval
# 136.7269 142.2624 172.5471 150.1005 223.7039 247.2416     10

profvis({
  cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits)
}, interval = 0.0075)


