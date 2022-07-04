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

# test that
  # margin is never negative DONE
  # fails when data is misaligned DONE
  # fails when non-fixed tier commission model specified DONE
  # fails when args for specified commission model are incorrect
    # futs_fixed_commission
      # wrong arg DONE
      # too may args DONE
      # misspecified arg (float instead of vector) DONE
    # fixed_percent
  # rolls on correct day, rolls correct amount
  # maybe do a 1- or 2-row backtest and ensure correct results


# test_that("Column order mismatch causes error", {
#   futures <- readRDS(test_path("fixtures", "futures.rds"))
#   wrangled <- wrangle_contracts_on_oi(futures)
#   sim_prices <- make_sim_prices_matrix(wrangled)
#
#   # example data: equal dollar weight in ES/GC/ZB
#   target_weights <- data.frame(
#     date = wrangled$date,
#     symbol = wrangled$symbol,
#     target_weight = rep(1./3, nrow(wrangled))
#   ) %>%
#     make_sim_weights_matrix())
#
#   expect_error(days_to_expiry(futures), "date column must exist and be of type Date")
# })
#
# futures <- readRDS(test_path("fixtures", "futures.rds"))
# wrangled <- wrangle_contracts_on_oi(futures)
# sim_prices <- make_sim_prices_matrix(wrangled)
#
# # example data: equal dollar weight in ES/GC/ZB
# target_weights <- data.frame(
#   date = wrangled$date,
#   symbol = wrangled$symbol,
#   target_weight = rep(1./3, nrow(wrangled))
# ) %>%
#   make_sim_weights_matrix()
#
# # example data: leveraged equal dollar weight in ES/GC/ZB
# leverage <- 10
# target_weights <- data.frame(
#   date = wrangled$date,
#   symbol = wrangled$symbol,
#   target_weight = leverage*rep(1./3, nrow(wrangled))
# ) %>%
#   make_sim_weights_matrix()
#
# # example interest rate data
# broker_spread <- 0.005
# rates <- data.frame(
#   date = sort(unique(wrangled$date)),
#   rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
# ) %>%
#   data.matrix()
#
# fixed_commission_futs_backtest(
#   prices = sim_prices,
#   target_weights = target_weights,
#   interest_rates = rates,
#   trade_buffer = 0.20,
#   initial_cash = 100000,
#   margin = 0.05,
#   capitalise_profits = TRUE,
#   commission_fun = futs_per_contract_commission,
#   per_contract_commission = c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
# ) -> results
#
# View(results)
#
# results %>%
#   ggplot(aes(x = date, y = exposure, colour = symbols)) +
#     geom_line() +
#     facet_wrap(~symbols, scales = "free_y", ncol = 1)
#
# margin_usage <- results %>%
#   dplyr::group_by(date) %>%
#   dplyr::summarise(total_margin = sum(margin))
#
# results %>%
#   dplyr::filter(symbols == "Cash") %>%
#   dplyr::select(date, symbols, exposure) %>%
#   dplyr::left_join(margin_usage, by = "date") %>%
#   dplyr::mutate(total_cash = exposure + total_margin) %>%
#   ggplot(aes(x = date, y = total_cash)) +
#     geom_line()
#
#

# f <- function(...){
#   args <- list(...)
#
#   if(! c(substitute(my_var)) %in% args && !length(args) == 1)
#     stop("wrong arguments specified")
#
# }
#
# myvar <- 1
# f(myvar)
