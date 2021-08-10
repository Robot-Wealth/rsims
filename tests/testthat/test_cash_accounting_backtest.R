# TESTS:

# commission calcs correct - edge cases?
# test when sum(abs(weights)) are greater than 1
# also need tests on relevant data structures in cash_backtest
library(tidyverse)

testthat::test_that("New positions are correct for different values of trade_buffer", {
  # get into initial positions
  current_prices <- c(100, 60, 20)
  current_theo_weights <- c(0.5, 0.3, -0.2)
  current_positions <- rep(0, 3)
  cap_equity <- 1000
  testthat::expect_equal(
    positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.),
    c(5., 5., -10.)
  )

  # buy, sell, no change
  current_weights <- c(0.3, 0.35, -0.23)
  current_positions <- current_weights*cap_equity/current_prices
  testthat::expect_equal(
    positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.04),
    c(0.46*cap_equity/100, 0.34*cap_equity/60, -0.23*cap_equity/20.) # c(buy up to 0.5-0.04, sell down to 0.3+0.04, no change)
  )

  # edge case: current position is equivalent to theo weight +/- trade_buffer
  current_weights <- c(0.55, 0.25, -0.25)
  current_positions <- current_weights*cap_equity/current_prices
  testthat::expect_equal(
    positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.05),
    current_positions  # no change
  )

  # TODO:
  # edge case: sign of theo weight is different to current weight but inside trade buffer



})

testthat::test_that("Inputs to cash_backtest are legitimate", {
  # setup
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

  # simulation parameters
  initial_cash <- 1000
  capitalise_profits <- FALSE  # remain fully invested?
  fee_tier <- 0.
  commission_pct <- 0.01

  # negative trade buffer throws error
  trade_buffer <- -0.05
  testthat::expect_error(cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits))

  # TODO:
  # misaligned dates throws error
  # misaligned column names throws error (if we want to keep column names... maybe push this upstream)
  # unequal number of columns throws error
  # unequal number of rows throws error - or perhaps we want a warning, and for the backtest to run to the shorter of the two?
})

