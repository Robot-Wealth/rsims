# Tests for fx_backtest function

# Helper function to create simple test data for XXX/USD pairs
create_simple_fx_data <- function(n_days = 10, initial_price = 1.10) {
  dates <- as.numeric(as.Date("2024-01-01") + 0:(n_days - 1))

  # Create simple price series with small daily changes
  prices_vec <- initial_price + cumsum(rnorm(n_days, 0, 0.001))

  prices <- matrix(
    c(dates, prices_vec),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Constant weight of 0.5 (50% allocation to EURUSD)
  weights <- matrix(
    c(dates, rep(0.5, n_days)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # For XXX/USD pair, base_currency_rates = price, quote_currency_rates = 1
  base_rates <- prices
  quote_rates <- matrix(
    c(dates, rep(1.0, n_days)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  list(
    prices = prices,
    weights = weights,
    base_rates = base_rates,
    quote_rates = quote_rates
  )
}

# =============================================================================
# Phase 1 Tests: Basic functionality with XXX/USD pairs
# =============================================================================

test_that("fx_backtest handles single XXX/USD pair", {
  data <- create_simple_fx_data()

  result <- fx_backtest(
    prices = data$prices,
    theo_weights = data$weights,
    base_currency_rates = data$base_rates,
    quote_currency_rates = data$quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  # Check output structure

  expect_s3_class(result, "tbl_df")
  expect_true("ticker" %in% colnames(result))
  expect_true("Date" %in% colnames(result))
  expect_true("Position" %in% colnames(result))
  expect_true("Value" %in% colnames(result))
  expect_true("PnlUsd" %in% colnames(result))

  # Should have Cash and EURUSD rows for each date
  expect_equal(nrow(result), 10 * 2)  # 10 days * 2 tickers
  expect_true("Cash" %in% result$ticker)
  expect_true("EURUSD" %in% result$ticker)
})

test_that("fx_backtest validates input dimensions", {
  data <- create_simple_fx_data()

  # Create weights with wrong dimensions
  wrong_weights <- data$weights[1:5, ]


  expect_error(
    fx_backtest(
      prices = data$prices,
      theo_weights = wrong_weights,
      base_currency_rates = data$base_rates,
      quote_currency_rates = data$quote_rates
    ),
    "same dimensions"
  )
})

test_that("fx_backtest validates timestamp alignment", {
  data <- create_simple_fx_data()

  # Create weights with misaligned timestamps
  wrong_weights <- data$weights
  wrong_weights[5, 1] <- wrong_weights[5, 1] + 1  # Shift one timestamp

  expect_error(
    fx_backtest(
      prices = data$prices,
      theo_weights = wrong_weights,
      base_currency_rates = data$base_rates,
      quote_currency_rates = data$quote_rates
    ),
    "misaligned"
  )
})

test_that("fx_backtest errors on NA prices with non-zero weights", {
  data <- create_simple_fx_data()

  # Set one price to NA where weight is non-zero
  bad_prices <- data$prices
  bad_prices[5, 2] <- NA

  expect_error(
    fx_backtest(
      prices = bad_prices,
      theo_weights = data$weights,
      base_currency_rates = data$base_rates,
      quote_currency_rates = data$quote_rates
    ),
    "NA prices detected"
  )
})

test_that("fx_backtest requires non-negative trade_buffer", {
  data <- create_simple_fx_data()

  expect_error(
    fx_backtest(
      prices = data$prices,
      theo_weights = data$weights,
      base_currency_rates = data$base_rates,
      quote_currency_rates = data$quote_rates,
      trade_buffer = -0.1
    ),
    "trade_buffer must be greater than or equal to zero"
  )
})

test_that("capitalise_profits=FALSE uses initial_cash for sizing", {
  # Create data where we expect profits
  dates <- as.numeric(as.Date("2024-01-01") + 0:4)

  # Price increases each day
  prices <- matrix(
    c(dates, c(1.10, 1.12, 1.14, 1.16, 1.18)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  weights <- matrix(
    c(dates, rep(0.5, 5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, rep(1.0, 5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  # With capitalise_profits = FALSE, position size should be based on initial_cash
  # Target value = 10000 * 0.5 = 5000 USD
  # Position at first price (1.10) = 5000 / 1.10 = 4545.45 EUR
  eurusd_rows <- result %>% dplyr::filter(ticker == "EURUSD")
  first_position <- eurusd_rows$Position[1]

  expected_position <- (10000 * 0.5) / 1.10
  expect_equal(first_position, expected_position, tolerance = 0.01)

  # Check that position doesn't grow much even as we make profits
  # (should still be sized based on initial_cash of 10000)
  last_position <- eurusd_rows$Position[5]
  expected_last <- (10000 * 0.5) / 1.18
  expect_equal(last_position, expected_last, tolerance = 0.01)
})

test_that("capitalise_profits=TRUE uses current equity for sizing", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:4)

  # Price increases each day
  prices <- matrix(
    c(dates, c(1.10, 1.12, 1.14, 1.16, 1.18)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  weights <- matrix(
    c(dates, rep(0.5, 5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, rep(1.0, 5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result_compound <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = TRUE
  )

  result_flat <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  # With compounding, total equity should be higher at the end
  compound_equity <- result_compound %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::summarise(total = sum(Value)) %>%
    dplyr::pull(total)

  flat_equity <- result_flat %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::summarise(total = sum(Value)) %>%
    dplyr::pull(total)

  # In an uptrend, compounding should result in higher final equity
  expect_gt(compound_equity, flat_equity)
})

test_that("commission is calculated correctly", {
  data <- create_simple_fx_data(n_days = 5)

  # Set constant weights to force trades
  commission_pct <- 0.001  # 0.1%

  result <- fx_backtest(
    prices = data$prices,
    theo_weights = data$weights,
    base_currency_rates = data$base_rates,
    quote_currency_rates = data$quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = commission_pct,
    capitalise_profits = FALSE
  )

  eurusd_rows <- result %>% dplyr::filter(ticker == "EURUSD")

  # First trade should have commission
  first_trade_value <- abs(eurusd_rows$TradeValue[1])
  first_commission <- eurusd_rows$Commission[1]

  expect_equal(first_commission, first_trade_value * commission_pct, tolerance = 0.001)
})

test_that("trade buffer reduces unnecessary trades", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:9)

  # Price oscillates slightly around 1.10
  prices <- matrix(
    c(dates, c(1.100, 1.101, 1.099, 1.102, 1.098, 1.101, 1.100, 1.099, 1.101, 1.100)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  weights <- matrix(
    c(dates, rep(0.5, 10)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, rep(1.0, 10)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # With no buffer, should trade every day
  result_no_buffer <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  # With large buffer, should trade less
  result_with_buffer <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0.10,  # 10% buffer
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  trades_no_buffer <- result_no_buffer %>%
    dplyr::filter(ticker == "EURUSD") %>%
    dplyr::summarise(total_trades = sum(abs(Trades))) %>%
    dplyr::pull(total_trades)

  trades_with_buffer <- result_with_buffer %>%
    dplyr::filter(ticker == "EURUSD") %>%
    dplyr::summarise(total_trades = sum(abs(Trades))) %>%
    dplyr::pull(total_trades)

  # With buffer, should have fewer total trades
  expect_lt(trades_with_buffer, trades_no_buffer)
})

test_that("include_initial_state=TRUE prepends correct row", {
  data <- create_simple_fx_data(n_days = 5)

  result <- fx_backtest(
    prices = data$prices,
    theo_weights = data$weights,
    base_currency_rates = data$base_rates,
    quote_currency_rates = data$quote_rates,
    initial_cash = 10000,
    include_initial_state = TRUE
  )

  # Should have 6 days worth of rows (5 + 1 initial)
  expect_equal(nrow(result), 6 * 2)

  # First Cash row should have initial_cash value
  first_cash <- result %>%
    dplyr::filter(ticker == "Cash") %>%
    dplyr::slice(1)

  expect_equal(first_cash$Value, 10000)
  expect_equal(first_cash$Position, 10000)

  # First EURUSD row should have zero position
  first_eurusd <- result %>%
    dplyr::filter(ticker == "EURUSD") %>%
    dplyr::slice(1)

  expect_equal(first_eurusd$Position, 0)
  expect_equal(first_eurusd$Value, 0)
})

test_that("zero weight exits position completely", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:4)

  prices <- matrix(
    c(dates, c(1.10, 1.11, 1.12, 1.13, 1.14)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Start with 50% weight, then go to 0
  weights <- matrix(
    c(dates, c(0.5, 0.5, 0, 0, 0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, rep(1.0, 5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0.10,  # Even with buffer, zero weight should exit
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  eurusd_rows <- result %>% dplyr::filter(ticker == "EURUSD")

  # Days 3, 4, 5 should have zero position
  expect_equal(eurusd_rows$Position[3], 0)
  expect_equal(eurusd_rows$Position[4], 0)
  expect_equal(eurusd_rows$Position[5], 0)
})

# =============================================================================
# Phase 2 Tests: Full pair support (to be implemented)
# =============================================================================

test_that("USD/XXX pair P&L converts correctly to USD", {
  # For USD/JPY, position is in USD, but P&L is in JPY
  # Need to convert JPY P&L to USD using 1/USDJPY rate

  dates <- as.numeric(as.Date("2024-01-01") + 0:2)

  # USDJPY prices
  prices <- matrix(
    c(dates, c(110.0, 111.0, 112.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "USDJPY"))
  )

  weights <- matrix(
    c(dates, c(0.5, 0.5, 0.5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "USDJPY"))
  )

  # For USD/JPY: base is USD, so base_rate = 1.0
  base_rates <- matrix(
    c(dates, c(1.0, 1.0, 1.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "USDJPY"))
  )

  # For USD/JPY: quote is JPY, so quote_rate = 1/USDJPY
  quote_rates <- matrix(
    c(dates, 1 / c(110.0, 111.0, 112.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "USDJPY"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  usdjpy_rows <- result %>% dplyr::filter(ticker == "USDJPY")

  # Day 1: Position should be 10000 * 0.5 / 1.0 = 5000 USD
  expect_equal(usdjpy_rows$Position[1], 5000, tolerance = 0.01)

  # Day 2: P&L in quote (JPY) = 5000 * (111 - 110) = 5000 JPY
  # P&L in USD = 5000 / 111 = 45.05 USD
  expected_pnl_usd_day2 <- 5000 * (111 - 110) / 111
  expect_equal(usdjpy_rows$PnlUsd[2], expected_pnl_usd_day2, tolerance = 0.1)
})

test_that("cross pair (EUR/JPY) P&L converts correctly to USD", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:2)

  # EURJPY prices
  prices <- matrix(
    c(dates, c(130.0, 131.0, 132.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURJPY"))
  )

  weights <- matrix(
    c(dates, c(0.5, 0.5, 0.5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURJPY"))
  )

  # For EUR/JPY: base is EUR, so base_rate = EURUSD
  # Assume EURUSD = 1.10
  base_rates <- matrix(
    c(dates, c(1.10, 1.10, 1.10)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURJPY"))
  )

  # For EUR/JPY: quote is JPY, so quote_rate = 1/USDJPY
  # Assume USDJPY = 118.18 (so EURJPY = 130 with EURUSD = 1.10)
  quote_rates <- matrix(
    c(dates, 1 / c(118.18, 119.09, 120.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURJPY"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  eurjpy_rows <- result %>% dplyr::filter(ticker == "EURJPY")

  # Day 1: Position should be 10000 * 0.5 / 1.10 = 4545.45 EUR
  expected_position <- (10000 * 0.5) / 1.10
  expect_equal(eurjpy_rows$Position[1], expected_position, tolerance = 0.01)

  # Day 2: P&L in quote (JPY) = 4545.45 * (131 - 130) = 4545.45 JPY
  # P&L in USD = 4545.45 / 119.09 = 38.17 USD
  expected_pnl_jpy <- expected_position * (131 - 130)
  expected_pnl_usd <- expected_pnl_jpy / 119.09
  expect_equal(eurjpy_rows$PnlUsd[2], expected_pnl_usd, tolerance = 0.5)
})

# =============================================================================
# Phase 3 Tests: Swap costs (to be implemented)
# =============================================================================

test_that("long positions accrue swap_long rate", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:2)

  prices <- matrix(
    c(dates, c(1.10, 1.10, 1.10)),  # Flat price to isolate swap effect
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  weights <- matrix(
    c(dates, c(0.5, 0.5, 0.5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, c(1.0, 1.0, 1.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Swap rates: long pays 0.01% per day, short receives 0.005% per day
  swap_rates <- matrix(
    c(dates, rep(-0.0001, 3), rep(0.00005, 3)),
    ncol = 3,
    dimnames = list(NULL, c("date", "EURUSD_long", "EURUSD_short"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    swap_rates = swap_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  eurusd_rows <- result %>% dplyr::filter(ticker == "EURUSD")

  # Long position should accrue negative swap (we pay)
  # Position value = ~5000 USD, swap = -5000 * 0.0001 = -0.50 USD per day
  # But first day has no prior position, so swap starts from day 2
  expect_lt(eurusd_rows$Swap[2], 0)  # Should be negative (we pay)
})

test_that("short positions accrue swap_short rate", {
  dates <- as.numeric(as.Date("2024-01-01") + 0:2)

  prices <- matrix(
    c(dates, c(1.10, 1.10, 1.10)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Negative weights = short position
  weights <- matrix(
    c(dates, c(-0.5, -0.5, -0.5)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(dates, c(1.0, 1.0, 1.0)),
    ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Swap rates: long pays, short receives
  swap_rates <- matrix(
    c(dates, rep(-0.0001, 3), rep(0.00005, 3)),
    ncol = 3,
    dimnames = list(NULL, c("date", "EURUSD_long", "EURUSD_short"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    swap_rates = swap_rates,
    trade_buffer = 0,
    initial_cash = 10000,
    commission_pct = 0,
    capitalise_profits = FALSE
  )

  eurusd_rows <- result %>% dplyr::filter(ticker == "EURUSD")

  # Short position should accrue positive swap (we receive)
  expect_gt(eurusd_rows$Swap[2], 0)  # Should be positive (we receive)
})

test_that("swap_rates=NULL works (no swap costs)", {
  data <- create_simple_fx_data()

  # Should not error with NULL swap_rates
  result <- fx_backtest(
    prices = data$prices,
    theo_weights = data$weights,
    base_currency_rates = data$base_rates,
    quote_currency_rates = data$quote_rates,
    swap_rates = NULL,
    initial_cash = 10000
  )

  # All swap values should be zero
  expect_true(all(result$Swap == 0))
})

# =============================================================================
# Phase 4 Tests: Helper functions
# =============================================================================

test_that("make_fx_prices_matrix creates correct structure", {
  prices_df <- data.frame(
    pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
    close = c(1.10, 1.11, 110, 111)
  )

  result <- make_fx_prices_matrix(prices_df)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)  # date + 2 pairs
  expect_equal(nrow(result), 2)  # 2 dates
  expect_true("EURUSD" %in% colnames(result))
  expect_true("USDJPY" %in% colnames(result))
})

test_that("make_fx_weights_matrix creates correct structure", {
  weights_df <- data.frame(
    pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
    weight = c(0.5, 0.5, 0.5, 0.5)
  )

  result <- make_fx_weights_matrix(weights_df)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 2)
})

test_that("make_fx_base_rates_matrix handles multiple pair types", {
  prices_df <- data.frame(
    pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY", "EURJPY", "EURJPY"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02",
                     "2024-01-01", "2024-01-02")),
    close = c(1.10, 1.11, 110, 111, 121, 122.1)
  )

  pair_base <- c(EURUSD = "EUR", USDJPY = "USD", EURJPY = "EUR")

  usd_rates_df <- data.frame(
    currency = c("EUR", "EUR", "USD", "USD"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
    rate = c(1.10, 1.11, 1.0, 1.0)
  )

  result <- make_fx_base_rates_matrix(prices_df, pair_base, usd_rates_df)

  expect_true(is.matrix(result))
  expect_equal(ncol(result), 4)  # date + 3 pairs

  # USD base rate should be 1.0
  expect_equal(as.numeric(result[1, "USDJPY"]), 1.0)

  # EUR base rate should match EURUSD
  expect_equal(as.numeric(result[1, "EURUSD"]), 1.10)
  expect_equal(as.numeric(result[1, "EURJPY"]), 1.10)  # EURJPY also uses EUR base
})

test_that("make_fx_quote_rates_matrix handles multiple pair types", {
  prices_df <- data.frame(
    pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
    close = c(1.10, 1.11, 110, 111)
  )

  pair_quote <- c(EURUSD = "USD", USDJPY = "JPY")

  usd_rates_df <- data.frame(
    currency = c("USD", "USD", "JPY", "JPY"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
    rate = c(1.0, 1.0, 1/110, 1/111)  # JPY rate is 1/USDJPY
  )

  result <- make_fx_quote_rates_matrix(prices_df, pair_quote, usd_rates_df)

  expect_true(is.matrix(result))

  # USD quote rate should be 1.0
  expect_equal(as.numeric(result[1, "EURUSD"]), 1.0)

  # JPY quote rate should be 1/USDJPY
  expect_equal(as.numeric(result[1, "USDJPY"]), 1/110, tolerance = 0.0001)
})

test_that("make_fx_swap_matrix creates paired columns", {
  swap_df <- data.frame(
    pair = c("EURUSD", "EURUSD"),
    date = as.Date(c("2024-01-01", "2024-01-02")),
    swap_long = c(-0.0001, -0.0001),
    swap_short = c(0.00005, 0.00005)
  )

  result <- make_fx_swap_matrix(swap_df)

  expect_true(is.matrix(result))
  expect_true("EURUSD_long" %in% colnames(result))
  expect_true("EURUSD_short" %in% colnames(result))
  expect_equal(as.numeric(result[1, "EURUSD_long"]), -0.0001)
  expect_equal(as.numeric(result[1, "EURUSD_short"]), 0.00005)
})

test_that("helper functions produce matrices compatible with fx_backtest", {
  # Create test data using helper functions
  prices_df <- data.frame(
    pair = rep("EURUSD", 5),
    date = as.Date("2024-01-01") + 0:4,
    close = c(1.10, 1.11, 1.12, 1.11, 1.10)
  )

  weights_df <- data.frame(
    pair = rep("EURUSD", 5),
    date = as.Date("2024-01-01") + 0:4,
    weight = rep(0.5, 5)
  )

  usd_rates_df <- data.frame(
    currency = rep(c("EUR", "USD"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    rate = c(1.10, 1.11, 1.12, 1.11, 1.10, rep(1.0, 5))
  )

  pair_base <- c(EURUSD = "EUR")
  pair_quote <- c(EURUSD = "USD")

  prices <- make_fx_prices_matrix(prices_df)
  weights <- make_fx_weights_matrix(weights_df)
  base_rates <- make_fx_base_rates_matrix(prices_df, pair_base, usd_rates_df)
  quote_rates <- make_fx_quote_rates_matrix(prices_df, pair_quote, usd_rates_df)

  # Should not error
  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000
  )

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

# ============================================================================
# Phase 4: Edge Cases and Documentation
# ============================================================================

test_that("sign change from long to short works correctly", {
  # Test flipping from long to short position
  prices <- matrix(
    c(1, 2, 3, 4, 5,
      1.10, 1.11, 1.12, 1.11, 1.10),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Start long (0.5), then go short (-0.5)
  weights <- matrix(
    c(1, 2, 3, 4, 5,
      0.5, 0.5, -0.5, -0.5, -0.5),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(1, 2, 3, 4, 5,
      1.0, 1.0, 1.0, 1.0, 1.0),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000,
    trade_buffer = 0  # Force exact rebalancing
  )

  # Get EURUSD positions
  eurusd_pos <- result %>% dplyr::filter(ticker == "EURUSD")

  # Day 1-2: long position (positive)
  expect_true(eurusd_pos$Position[1] > 0)
  expect_true(eurusd_pos$Position[2] > 0)

  # Day 3-5: short position (negative)
  expect_true(eurusd_pos$Position[3] < 0)
  expect_true(eurusd_pos$Position[4] < 0)
  expect_true(eurusd_pos$Position[5] < 0)
})

test_that("sign change from short to long works correctly", {
  prices <- matrix(
    c(1, 2, 3, 4, 5,
      1.10, 1.11, 1.12, 1.11, 1.10),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # Start short (-0.5), then go long (0.5)
  weights <- matrix(
    c(1, 2, 3, 4, 5,
      -0.5, -0.5, 0.5, 0.5, 0.5),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(1, 2, 3, 4, 5,
      1.0, 1.0, 1.0, 1.0, 1.0),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000,
    trade_buffer = 0
  )

  eurusd_pos <- result %>% dplyr::filter(ticker == "EURUSD")

  # Day 1-2: short position (negative)
  expect_true(eurusd_pos$Position[1] < 0)
  expect_true(eurusd_pos$Position[2] < 0)

  # Day 3-5: long position (positive)
  expect_true(eurusd_pos$Position[3] > 0)
  expect_true(eurusd_pos$Position[4] > 0)
  expect_true(eurusd_pos$Position[5] > 0)
})

test_that("large position leverage works correctly", {
  # Test 2x leverage
  prices <- matrix(
    c(1, 2, 3,
      1.10, 1.11, 1.12),
    nrow = 3, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  # 2x leverage = weight of 2.0
  weights <- matrix(
    c(1, 2, 3,
      2.0, 2.0, 2.0),
    nrow = 3, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(1, 2, 3,
      1.0, 1.0, 1.0),
    nrow = 3, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000,
    trade_buffer = 0
  )

  eurusd <- result %>% dplyr::filter(ticker == "EURUSD")
  cash <- result %>% dplyr::filter(ticker == "Cash")

  # Position value should be ~2x initial cash (20000 USD worth of EUR)
  expect_equal(eurusd$Value[1], 20000, tolerance = 1)

  # Cash should be negative (borrowed)
  expect_true(cash$Value[1] < 0)
})

test_that("output is compatible with calc_port_returns after renaming", {
  # Test that results can be used with post_processing functions
  # (after renaming columns to match expected format)
  prices <- matrix(
    c(1, 2, 3, 4, 5,
      1.10, 1.11, 1.12, 1.11, 1.10),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  weights <- matrix(
    c(1, 2, 3, 4, 5,
      0.5, 0.5, 0.5, 0.5, 0.5),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(1, 2, 3, 4, 5,
      1.0, 1.0, 1.0, 1.0, 1.0),
    nrow = 5, ncol = 2,
    dimnames = list(NULL, c("date", "EURUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000
  )

  # Rename columns to match post_processing.R expectations
  result_renamed <- result %>%
    dplyr::rename(date = Date, exposure = Value)

  # calc_port_returns should work
  port_returns <- result_renamed %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(totalequity = sum(exposure, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(returns = totalequity / dplyr::lag(totalequity) - 1)

  expect_true("returns" %in% colnames(port_returns))
  expect_equal(nrow(port_returns), 5)

  # First return should be NA, rest should be numeric
  expect_true(is.na(port_returns$returns[1]))
  expect_true(all(!is.na(port_returns$returns[-1])))
})

test_that("multiple assets trade independently", {
  # Test that trades on one asset don't affect another
  prices <- matrix(
    c(1, 2, 3,
      1.10, 1.11, 1.12,
      1.20, 1.21, 1.22),
    nrow = 3, ncol = 3,
    dimnames = list(NULL, c("date", "EURUSD", "GBPUSD"))
  )

  # EURUSD changes weight, GBPUSD stays constant
  weights <- matrix(
    c(1, 2, 3,
      0.3, 0.5, 0.3,
      0.3, 0.3, 0.3),
    nrow = 3, ncol = 3,
    dimnames = list(NULL, c("date", "EURUSD", "GBPUSD"))
  )

  base_rates <- prices
  quote_rates <- matrix(
    c(1, 2, 3,
      1.0, 1.0, 1.0,
      1.0, 1.0, 1.0),
    nrow = 3, ncol = 3,
    dimnames = list(NULL, c("date", "EURUSD", "GBPUSD"))
  )

  result <- fx_backtest(
    prices = prices,
    theo_weights = weights,
    base_currency_rates = base_rates,
    quote_currency_rates = quote_rates,
    initial_cash = 10000,
    trade_buffer = 0
  )

  eurusd <- result %>% dplyr::filter(ticker == "EURUSD")
  gbpusd <- result %>% dplyr::filter(ticker == "GBPUSD")

  # EURUSD should have 3 different position values (trades on days 1, 2, 3)
  expect_false(eurusd$Position[1] == eurusd$Position[2])

  # GBPUSD should maintain roughly constant weight
  gbp_weights <- gbpusd$Value / (result %>% dplyr::filter(ticker == "Cash") %>% dplyr::pull(Value) + sum(result %>% dplyr::filter(ticker != "Cash") %>% dplyr::group_by(Date) %>% dplyr::summarise(v = sum(Value)) %>% dplyr::pull(v)) / 3)

  # GBPUSD trades should be smaller (just price drift rebalancing)
  expect_true(abs(gbpusd$Trades[2]) < abs(eurusd$Trades[2]) || gbpusd$Trades[2] == 0)
})
