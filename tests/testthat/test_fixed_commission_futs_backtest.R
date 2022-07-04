# TODO:
  # test that margin is freed and applied correctly around the roll and around regular trading
  # test that we rebalance back to the trade buffer correctly
  # test insane leverage - should go to zero but no lower

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

  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)

  results <- fixed_commission_futs_backtest(
      prices = sim_prices,
      target_weights = target_weights,
      interest_rates = rates,
      trade_buffer = 0.20,
      initial_cash = 100000,
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

# TODO.. well, it won't necessarily blow up the account as we keep force reducing the positions we can put on...
test_that("Insane leverage blows up account", {
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
    trade_buffer = 0.20,
    initial_cash = 100000,
    margin = 0.05,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = per_contract_commission
  )

})
