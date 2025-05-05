test_that("Misaligned prices and weights timestamps causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = wrangled$date,
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[2:21, ]
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
    ),
    "Timestamps of prices and weights matrixes are misaligned"
  )
})

test_that("Misaligned prices and rates timestamps causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  rates <- rates[2:21, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
    ),
    "Timestamps of prices and rates matrixes are misaligned"
  )
})

test_that("Contracts are traded only in integer amounts", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = 5*rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  results <- fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 1000000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
    )
  expect_true(all(results$contracts == as.integer(results$contracts)))
})

test_that("Margin is never negative", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  results <- fixed_commission_futs_backtest(
    prices = sim_prices,
    target_weights = target_weights,
    interest_rates = rates,
    trade_buffer = 0.20,
    initial_cash = 100000,
    margin = 0.05,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
  )
  expect_true(all(results$margin >= 0))

})

test_that("Asset-wise column order mismatch in prices and weights matrixes causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  colnames(target_weights) <- c("date", "ZB", "GC", "ES")
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
    ),
    "Asset-wise column order mismatch in prices and weights matrixes"
  )
})

test_that("Specification of non-fixed commission model causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = us_tiered_commission
    ),
    "Commission function not yet implemented - choose futs_per_contract_commission or fixed_percent"
  )
})

test_that("Providing incorrect arg to futs_per_contract_commission causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      fixed_percet_commission = 0.01/100
    ),
    "Wrong arguments specified. futs_per_contract_commission requires 1 argument, per_contract_commission"
  )
})

test_that("Providing incorrect number of args to futs_per_contract_commission causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85),
      fixed_percet_commission = 0.01/100
    ),
    "Wrong arguments specified. futs_per_contract_commission requires 1 argument, per_contract_commission"
  )
})

test_that("Misspecifying per_contract_commission arg to futs_per_contract_commission causes error", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  sim_prices <- sim_prices[1:20, ]
  target_weights <- target_weights[1:20, ]
  rates <- rates[1:20, ]

  expect_error(
    fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = 0.85
    ),
    "per_contract_commission must be a named vector whose names correspond to the contract symbols in target_weights"
  )
})

test_that("Roll happens on correct day at correct prices with correct commissions", {
  # Expect to roll out of GC-2021G on 2021-01-20 at 186650.0
  # Expect to roll into GC-2021J on 2021-01-20 at 187020.0

  expected_close <- 187020.0
  expected_roll_price <- 186650.0

  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal dollar weight in ES/GC/ZB
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = 5*rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)

  results <- fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 1000000,
      margin = 0.05,
      capitalise_profits = TRUE,
      commission_fun = futs_per_contract_commission,
      per_contract_commission = per_contract_commission
  )

  expected_commission <- results %>%
    dplyr::filter(symbol == "GC", date == "2021-01-20") %>%
    dplyr::mutate(expected_commission = per_contract_commission["GC"]*(abs(contract_trades) + abs(rolled_contracts))) %>%
    dplyr::pull(expected_commission)

  expect_true(all(
   results %>% dplyr::filter(symbol == "GC", date == "2021-01-20") %>% dplyr::pull(close) == expected_close,
   results %>% dplyr::filter(symbol == "GC", date == "2021-01-20") %>% dplyr::pull(roll_price) == expected_roll_price,
   results %>% dplyr::filter(symbol == "GC", date == "2021-01-20") %>% dplyr::pull(commission) == expected_commission
  ))
})

test_that("Insanely leveraged short stock index loses money and eventually can't trade anymore", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: insanely leveraged short ES
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = 10000*rep(c(-1, 0, 0), nrow(wrangled)/3)
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)

  results <- fixed_commission_futs_backtest(
    prices = sim_prices,
    target_weights = target_weights,
    interest_rates = rates,
    trade_buffer = 0.0,
    initial_cash = 1000000,
    margin = 0.05,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = per_contract_commission
  )

  # we should not hold any contracts at the end of the simulation
  num_contracts_at_end <- results %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(total_contracts = sum(contracts)) %>%
    tail(1) %>%
    dplyr::pull(total_contracts)
  expected_num_contracts_at_end <- 0

  # we should lose most of our money
  nav_at_end <- results %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(margin = sum(margin)) %>%
    dplyr::left_join(
      results %>% dplyr::select(date, symbol, exposure) %>% dplyr::filter(symbol == "Cash"),
      by = "date"
    ) %>%
    dplyr::mutate(NAV = margin + exposure) %>%
    tail(1) %>%
    dplyr::pull(NAV)
  expected_max_nav <- 15000

  expect_true(all(
    num_contracts_at_end == expected_num_contracts_at_end,
    nav_at_end <= expected_max_nav
  ))

})

test_that("Trade buffer set to zero produces correct rebalances", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: insanely leveraged short ES
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = 5*rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
  margin <- 0.05
  initial_cash <- 1000000

  results <- fixed_commission_futs_backtest(
    prices = sim_prices,
    target_weights = target_weights,
    interest_rates = rates,
    trade_buffer = 0.0,
    initial_cash = initial_cash,
    margin = margin,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = per_contract_commission
  )

  # expected contracts held on day 1 with zero trade buffer
  num_contracts_zero_trade_buffer <- results %>%
    head(4) %>%  # get rows for day 1 for each asset and cash
    tail(3) %>%  # drop cash
    dplyr::select(symbol, contracts) %>%
    dplyr::pull(contracts, symbol)  # converts to named vector
  expected_num_contracts_zero_trade_buffer <- trunc(target_weights[1, -1]*initial_cash/sim_prices[1, c(2:4)])

  expect_equal(num_contracts_zero_trade_buffer, expected_num_contracts_zero_trade_buffer)
})

# test_that("Non-zero trade buffer produces correct rebalances", {
#   futures <- readRDS(test_path("fixtures", "futures.rds"))
#   wrangled <- wrangle_contracts_on_oi(futures)
#   sim_prices <- make_sim_prices_matrix(wrangled)
#
#   # example data: insanely leveraged short ES
#   target_weights <- data.frame(
#     date = wrangled$date,
#     symbol = wrangled$symbol,
#     target_weight = 5*rep(1./3, nrow(wrangled))
#   ) %>%
#     make_sim_weights_matrix()
#
#   # example interest rate data
#   broker_spread <- 0.005
#   rates <- data.frame(
#     date = sort(unique(wrangled$date)),
#     rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
#   ) %>%
#     data.matrix()
#
#   per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
#   margin <- 0.05
#   initial_cash <- 1000000
#   trade_buffer <- 0.2
#
#   results <- fixed_commission_futs_backtest(
#     prices = sim_prices,
#     target_weights = target_weights,
#     interest_rates = rates,
#     trade_buffer = trade_buffer,
#     initial_cash = initial_cash,
#     margin = margin,
#     capitalise_profits = TRUE,
#     commission_fun = futs_per_contract_commission,
#     per_contract_commission = per_contract_commission
#   )
#
#   # expected contracts held on day 1 with non-zero trade buffer
#   num_contracts_non_zero_trade_buffer <- results %>%
#     head(4) %>%  # get rows for day 1 for each asset and cash
#     tail(3) %>%  # drop cash
#     dplyr::select(symbol, contracts) %>%
#     dplyr::pull(contracts, symbol)  # converts to named vector
#
#   expected_num_contracts_non_zero_trade_buffer <- trunc((target_weights[1, -1] - trade_buffer)*initial_cash/sim_prices[1, c(2:4)])
#
#   expect_equal(num_contracts_non_zero_trade_buffer, expected_num_contracts_non_zero_trade_buffer)
# })

test_that("Cash accounting accuracy is within acceptable tolerance", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  wrangled <- wrangle_contracts_on_oi(futures)
  sim_prices <- make_sim_prices_matrix(wrangled)

  # example data: equal weight
  target_weights <- data.frame(
    date = wrangled$date,
    symbol = wrangled$symbol,
    target_weight = 5*rep(1./3, nrow(wrangled))
  ) %>%
    make_sim_weights_matrix()

  # example interest rate data
  broker_spread <- 0.005
  rates <- data.frame(
    date = sort(unique(wrangled$date)),
    rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) %>%
    data.matrix()

  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
  margin <- 0.05
  initial_cash <- 1000000
  trade_buffer <- 0.2

  results <- fixed_commission_futs_backtest(
    prices = sim_prices,
    target_weights = target_weights,
    interest_rates = rates,
    trade_buffer = trade_buffer,
    initial_cash = initial_cash,
    margin = margin,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = per_contract_commission
  )

  # Cash at T+1 should be equal to:
  # cash_t0 + margin_t0 - margin_t1 + settled_cash_t1 - commission_t1 + interest_t1
  reconciled_cash <- results %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      total_margin = sum(margin),
      total_settled_cash = sum(settled_cash),
      total_commission = sum(commission)
    ) %>%
    dplyr::left_join(results %>% dplyr::filter(symbol == "Cash"), by = "date") %>%
    dplyr::select(date, exposure, total_margin, total_settled_cash, total_commission, exposure, interest) %>%
    dplyr::mutate(
      lag_cash = dplyr::lag(exposure),
      lag_total_margin = dplyr::lag(total_margin)
    ) %>%
    dplyr::mutate(
      lag_cash = replace(lag_cash, is.na(lag_cash), initial_cash),
      lag_total_margin = replace(lag_total_margin, is.na(lag_total_margin), 0)
    ) %>%
    dplyr::mutate(expected_cash = lag_cash + lag_total_margin - total_margin + total_settled_cash - total_commission + interest) %>%
    dplyr::mutate(
      diff = expected_cash - exposure,
      pct_diff = diff/exposure
    )

  expect_lte(max(abs(reconciled_cash$pct_diff)), 1e-6)

})
