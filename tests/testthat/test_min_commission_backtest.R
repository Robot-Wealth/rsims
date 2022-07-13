test_that("Misaligned prices and weights errors", {

})

test_that("Different dimensions of prices and weights matrixes errors", {

})

test_that("Different dimensions of prices and unadjusted prices matrixes errors", {

})

test_that("Only integer shares traded", {

})

test_that("Account equity falling below margin requirement triggers margin call", {

})

test_that("Leveraged short equal weight blows up account", {
  etfs <- readRDS(test_path("fixtures", "etfs.rds"))
  num_assets <- length(unique(etfs$ticker))

  # example data: equal dollar weight
  etfs <- etfs %>%
    dplyr::mutate(target_weight = -5./num_assets)

  etfs_wide <- etfs %>%
    tidyr::pivot_wider(names_from = ticker, values_from = c(close, closeadjusted, target_weight))

  # get weights as a wide matrix
  target_weights <- etfs_wide %>%
    dplyr::select(date, tidyr::starts_with("target_weight_")) %>%
    dplyr::rename_with(.fn = ~ stringr::str_remove(., "target_weight_")) %>%
    data.matrix()

  # NA weights should be zero
  target_weights[is.na(target_weights)] <- 0

  # get adjusted prices as a wide matrix
  backtest_prices <- etfs_wide %>%
    dplyr::select(date, tidyr::starts_with("closeadjusted_")) %>%
    dplyr::rename_with(.fn = ~ stringr::str_remove(., "closeadjusted_")) %>%
    data.matrix()

  # get unadjusted prices as a wide matrix
  unadjusted_prices <- etfs_wide %>%
    dplyr::select(date, tidyr::starts_with("close_")) %>%
    dplyr::rename_with(.fn = ~ stringr::str_remove(., "close_")) %>%
    data.matrix()

  # simulation parameters
  initial_cash <- 10000
  capitalise_profits <- FALSE
  trade_buffer <- 0.04

  # simulation
  results_df <- min_commission_backtest(
    prices = backtest_prices,
    unadjusted_prices = unadjusted_prices,
    target_weights = target_weights,
    trade_buffer = trade_buffer,
    initial_cash = initial_cash,
    capitalise_profits = FALSE,
    commission_fun = us_tiered_commission,
    max_pct_per_order = 0.01,
    min_dollars_per_order = 1,
    dollars_per_share = 0.005
  )

  nav_at_end <- results_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(NAV = sum(exposure)) %>%
    dplyr::pull(NAV) %>%
    tail(1)

  expected_max_nav <- 10

  num_shares_at_end <- results_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(num_shares = sum(shares)) %>%
    dplyr::pull(num_shares) %>%
    tail(1)

  expect_true(all(
    num_shares_at_end == 0,
    nav_at_end <= expected_max_nav
  ))
})

test_that("Known results are correct", {


})

# TODO:
# misaligned prices/weights errors
# different dimensions of prices/weights matrixes gives error
# pretty sure leverage isn't working as expected - test leveraged weights gives different results
# test margin call behaviour - leveraged and unleveraged (shouldn't get margin called in the latter)
# known data gives known results

# results_df %>%
#   ggplot(aes(x = date, y = exposure, colour = ticker)) +
#   geom_bar(stat = "identity")
#
#
# results_df %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(NAV = sum(exposure)) %>%
#   ggplot(aes(x = date, y = NAV)) +
#     geom_line()

# # TODO: data has changed - update expected results
#
# testthat::test_that("New positions are correct for different values of trade_buffer", {
#   # get into initial positions
#   current_prices <- c(100, 60, 20)
#   current_theo_weights <- c(0.5, 0.3, -0.2)
#   current_positions <- rep(0, 3)
#   cap_equity <- 1000
#   testthat::expect_equal(
#     positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.),
#     c(5., 5., -10.)
#   )
#
#   # buy, sell, no change
#   current_weights <- c(0.3, 0.35, -0.23)
#   current_positions <- current_weights*cap_equity/current_prices
#   testthat::expect_equal(
#     positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.04),
#     c(0.46*cap_equity/100, 0.34*cap_equity/60, -0.23*cap_equity/20.) # c(buy up to 0.5-0.04, sell down to 0.3+0.04, no change)
#   )
#
#   # edge case: current position is equivalent to theo weight +/- trade_buffer
#   current_weights <- c(0.55, 0.25, -0.25)
#   current_positions <- current_weights*cap_equity/current_prices
#   testthat::expect_equal(
#     positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer = 0.05),
#     current_positions  # no change
#   )
#
#   # edge case: sign of theo weight is different to current weight but inside trade buffer
#   # is it realistic to be concerned about this? will only be an issue when allocations are tiny
#   current_theo_weights <- c(-0.03, -0.02, 0.02)
#   current_weights <- c(0.02, 0.03, -0.01)
#   current_positions <- current_weights*cap_equity/current_prices
#   trade_buffer <- 0.04
#   testthat::expect_equal(
#     positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer),
#     c(0.01*cap_equity/100, 0.02*cap_equity/60, -0.01*cap_equity/20)
#   )
# })
#
# testthat::test_that("Inputs to cash_backtest are legitimate", {
#   # setup data
#   load("~/rsims/data/backtest_df.RData")
#
#   # get weights as a wide matrix
#   backtest_theo_weights <- backtest_df %>%
#     dplyr::select(date, starts_with("theo_weight_")) %>%
#     data.matrix()
#
#   # NA weights should be zero
#   backtest_theo_weights[is.na(backtest_theo_weights)] <- 0
#
#   # get prices as a wide matrix
#   backtest_prices <- backtest_df %>%
#     dplyr::select(date, starts_with("price_")) %>%
#     data.matrix()
#
#   # simulation parameters
#   initial_cash <- 1000
#   capitalise_profits <- FALSE  # remain fully invested?
#   commission_pct <- 0.001
#
#   # negative trade buffer throws error
#   trade_buffer <- -0.05
#   testthat::expect_error(cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits))
#
#   # misaligned dates throws error
#   prices_bad_date <- backtest_prices
#   prices_bad_date[1, 1] <- backtest_theo_weights[2, 1]
#   trade_buffer <- 0.05
#   testthat::expect_error(cash_backtest(prices_bad_date, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits))
#
#   # unequal matrix dimensions throws error
#   prices_bad_cols <- cbind(backtest_prices, rep(1, nrow(backtest_prices)))
#   testthat::expect_error(cash_backtest(prices_bad_cols, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits))
# })
#
# testthat::test_that("Commission calculations are correct", {
#   # setup data
#   load("~/rsims/data/backtest_df.RData")
#
#   # get weights as a wide matrix
#   backtest_theo_weights <- backtest_df %>%
#     dplyr::select(date, starts_with("theo_weight_")) %>%
#     data.matrix()
#
#   # NA weights should be zero
#   backtest_theo_weights[is.na(backtest_theo_weights)] <- 0
#
#   # get prices as a wide matrix
#   backtest_prices <- backtest_df %>%
#     dplyr::select(date, starts_with("price_")) %>%
#     data.matrix()
#
#   # simulation parameters
#   initial_cash <- 1000
#   capitalise_profits <- FALSE  # remain fully invested?
#   commission_pct <- 0.001
#   trade_buffer <- 0.05
#
#   results_df <- cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits)
#
#   # can't be negative
#   testthat::expect_true(sum(as.numeric(results_df$Commission < 0), na.rm = TRUE) == 0)
#
#   # commission is absolute value of percent traded value
#   testthat::expect_true(sum(as.numeric(abs(results_df$TradeValue*commission_pct) != results_df$Commission), na.rm = TRUE) == 0)
# })
#
# testthat::test_that("Backtest returns known results", {
#   # make a simple backtest of 3-4 days and compute results separately
#   # setup data
#   load("~/rsims/data/backtest_df.RData")
#
#   # get weights as a wide matrix
#   backtest_theo_weights <- backtest_df %>%
#     dplyr::slice(1:5) %>%
#     dplyr::select(date, starts_with("theo_weight_")) %>%
#     dplyr::select(1:4) %>%
#     data.matrix()
#
#   # NA weights should be zero
#   backtest_theo_weights[is.na(backtest_theo_weights)] <- 0
#
#   # get prices as a wide matrix
#   backtest_prices <- backtest_df %>%
#     dplyr::slice(1:5) %>%
#     dplyr::select(date, starts_with("price_")) %>%
#     dplyr::select(1:4) %>%
#     data.matrix()
#
#   # simulation parameters
#   initial_cash <- 1000
#   capitalise_profits <- FALSE
#   commission_pct <- 0.001
#   trade_buffer <- 0.05
#   results_df <- cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits)
#
#   # load a csv created from an excel backtest using the same simulation parameters
#   # excel backtest model in tests/test_data.xlsc
#   test_results <- readr::read_csv("~/rsims/tests/test_data.csv")
#
#   # wrangle into same shape and format as results_df
#   test_results <- test_results %>%
#     dplyr::select(date, starts_with(c("price", "pos", "value", "trades", "tradevalue", "comm", "cash", "tottradevalue"))) %>%
#     tidyr::pivot_longer(cols = -date, names_to = c(".value", "ticker"), names_sep = "_") %>%
#     tidyr::replace_na(list(ticker = "Cash", price = 1., comm = 0)) %>%
#     dplyr::mutate(
#       pos = dplyr::case_when(ticker == "Cash" ~ cash, TRUE ~ pos),
#       value = dplyr::case_when(ticker == "Cash" ~ cash, TRUE ~ value),
#       trades = dplyr::case_when(ticker == "Cash" ~ tottradevalue, TRUE ~ trades),
#       tradevalue = dplyr::case_when(ticker == "Cash" ~ tottradevalue, TRUE ~ tradevalue)
#     ) %>%
#     dplyr::mutate(date = as.Date(date, origin ="1970-01-01")) %>%
#     dplyr::rename("Date" = date, "Close" = price, "Position" = pos, "Value" = value, "Trades" = trades, "TradeValue" = tradevalue, "Commission" = comm) %>%
#     dplyr::select(-cash, -tottradevalue) %>%
#     dplyr::relocate(ticker) %>%
#     dplyr::arrange(Date, ticker)
#
#   results_df <- results_df %>%
#     dplyr::arrange(Date, ticker)
#
#   testthat::expect_equal(results_df, test_results, tolerance = 1e-6)
# })
