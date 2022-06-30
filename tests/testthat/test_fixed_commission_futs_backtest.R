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

# example data: leveraged equal dollar weight in ES/GC/ZB
leverage <- 10
target_weights <- data.frame(
  date = wrangled$date,
  symbol = wrangled$symbol,
  target_weight = leverage*rep(1./3, nrow(wrangled))
) %>%
  make_sim_weights_matrix()

# example interest rate data
broker_spread <- 0.005
rates <- data.frame(
  date = wrangled$date,
  rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
) %>%
  data.matrix()

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
) -> results

View(results)

results %>%
  ggplot(aes(x = date, y = exposure, colour = symbols)) +
    geom_line() +
    facet_wrap(~symbols, scales = "free_y", ncol = 1)

margin_usage <- results %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(total_margin = sum(margin))

results %>%
  dplyr::filter(symbols == "Cash") %>%
  dplyr::select(date, symbols, exposure) %>%
  dplyr::left_join(margin_usage, by = "date") %>%
  dplyr::mutate(total_cash = exposure + total_margin) %>%
  ggplot(aes(x = date, y = total_cash)) +
    geom_line()


# test that margin is never negative
