# profiling
library(profvis)
library(microbenchmark)
library(tidyverse)

load("~/rsims/data/backtest_df_long.RData")

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
  times = 50
)
# Unit: milliseconds
#      min       lq     mean   median       uq      max  neval
# 136.7269 142.2624 172.5471 150.1005 223.7039 247.2416     10

profvis({
  cash_backtest(backtest_prices, backtest_theo_weights, trade_buffer, initial_cash, commission_pct, capitalise_profits)
}, interval = 0.005)



gbm_sim <- function(nsim = 100, t = 25, mu = 0, sigma = 0.1, S0 = 100, dt = 1./252) {

  # matrix of random draws - one for each day for each simulation
  epsilon <- matrix(rnorm(t*nsim), ncol = nsim, nrow = t)

  # get GBM paths
  gbm <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt)) - 1

  # convert to price paths
  gbm <- apply(rbind(rep(S0, nsim), gbm), 2, cumprod)

  return(gbm)
}

# returns process
years <- 10
universe <- 5
x <- 1
tickers <- vector()
repeat{
  tickers[[x]] <- paste0(sample(LETTERS, 5, replace = TRUE), collapse = "")
  x <- x + 1
  if(x == universe + 1)
    break
}
n_distinct(tickers)
date <- seq(as.numeric(as.Date("1980-01-01")), as.numeric(as.Date("1980-01-01"))+(years*365))
date <-
prices <- cbind(date, gbm_sim(nsim = universe, t = years*365, mu = 0.1, sigma = 0.1))
colnames(prices) <- c("date", tickers)
weights <- cbind(dates, rbind(rep(0, universe), matrix(rnorm(years*365*universe), nrow = years*365)))
colnames(weights) <- c("date", tickers)

head(weights, c(5, 5))
head(prices, c(5, 5))
dim(weights)
dim(prices)
library(tidyr)
btdf <- prices %>%
  as.data.frame() %>%
  mutate(date = as.Date(date, origin ="1970-01-01")) %>%
  pivot_longer(-date, names_to = "ticker", values_to = "price") %>%
  left_join(
    weights %>%
      as.data.frame() %>%
      mutate(date = as.Date(date, origin ="1970-01-01")) %>%
      pivot_longer(-date, names_to = "ticker", values_to = "theo_weight"),
    by = c("date", "ticker")
  )

profvis({
  cash_backtest(
    prices,
    weights,
    trade_buffer = 0.,
    initial_cash = 1000,
    commission_pct = 0.001,
    capitalise_profits = FALSE
  )
}, interval = 0.01)

# simulation benchmarking and optimisation
microbenchmark(
  cash_backtest(
    prices,
    weights,
    trade_buffer = 0.,
    initial_cash = 1000,
    commission_pct = 0.001,
    capitalise_profits = FALSE
  ),
  times = 10
)
# Unit: milliseconds
# min       lq     mean   median       uq      max neval
# 973.0783 978.0431 1003.592 1000.076 1018.796 1061.693    10


years <- 10
universe <- 1000
dates <- seq(as.numeric(as.Date("1980-01-01")), as.numeric(as.Date("1980-01-01"))+(years*365))
prices <- cbind(dates, gbm_sim(nsim = universe, t = years*365, mu = 0.1, sigma = 0.1))
weights <- cbind(dates, rbind(rep(0, universe), matrix(rnorm(years*365*universe), nrow = years*365)))

# simulation benchmarking and optimisation
microbenchmark(
  cash_backtest(
    prices,
    weights,
    trade_buffer = 0.,
    initial_cash = 1000,
    commission_pct = 0.001,
    capitalise_profits = FALSE
  ),
  times = 10
)
# Unit: seconds
# min       lq     mean  median       uq      max neval
# 1.531121 1.557212 1.626897 1.57228 1.696152 1.849934    10

years <- 10
universe <- 2000
dates <- seq(as.numeric(as.Date("1980-01-01")), as.numeric(as.Date("1980-01-01"))+(years*365))
prices <- cbind(dates, gbm_sim(nsim = universe, t = years*365, mu = 0.1, sigma = 0.1))
weights <- cbind(dates, rbind(rep(0, universe), matrix(rnorm(years*365*universe), nrow = years*365)))

# simulation benchmarking and optimisation
microbenchmark(
  cash_backtest(
    prices,
    weights,
    trade_buffer = 0.,
    initial_cash = 1000,
    commission_pct = 0.001,
    capitalise_profits = FALSE
  ),
  times = 10
)
# Unit: seconds
# min      lq      mean     median      uq      max      neval
# 2.939508 2.98787 3.229485 3.133614 3.49373 3.703139    10

years <- 10
universe <- 3000
dates <- seq(as.numeric(as.Date("1980-01-01")), as.numeric(as.Date("1980-01-01"))+(years*365))
prices <- cbind(dates, gbm_sim(nsim = universe, t = years*365, mu = 0.1, sigma = 0.1))
weights <- cbind(dates, rbind(rep(0, universe), matrix(rnorm(years*365*universe), nrow = years*365)))

# simulation benchmarking and optimisation
microbenchmark(
  cash_backtest(
    prices,
    weights,
    trade_buffer = 0.,
    initial_cash = 1000,
    commission_pct = 0.001,
    capitalise_profits = FALSE
  ),
  times = 10
)
# Unit: seconds
# min       lq     mean   median       uq      max neval
# 6.867289 6.915961 7.134657 6.941712 7.099699 8.229169    10

library(rsims)

get_mean_time <- function(days, universe, times = 5) {
  dates <- seq(as.numeric(as.Date("1980-01-01")), as.numeric(as.Date("1980-01-01"))+(days))
  prices <- cbind(dates, gbm_sim(nsim = universe, t = days, mu = 0.1, sigma = 0.1))
  weights <- cbind(dates, rbind(rep(0, universe), matrix(rnorm(days*universe), nrow = days)))

  res <- microbenchmark(
    cash_backtest(
      prices,
      weights,
      trade_buffer = 0.,
      initial_cash = 1000,
      commission_pct = 0.001,
      capitalise_profits = FALSE
    ),
    times = times
  )
  mean(res$time)/1e9
}

num_assets <- seq(100, 1000, 100)
num_days <- c(10, 20, 30, 40)*252

means <- list()
for(universe in num_assets) {
  print(glue::glue("Doing universe size {universe}"))
  for(days in num_days) {
    print(glue::glue("Doing {days} days"))
    means <- c(means, get_mean_time(days, universe, times = 10))
  }
}

df <- as.data.frame(matrix(unlist(means), ncol = length(num_assets))) %>%
  mutate(days = num_days)

colnames(df) <- c(num_assets, "days")

df %>%
  pivot_longer(cols = -days, names_to = "universe_size", values_to = "mean_sim_time") %>%
  mutate(universe_size = as.numeric(universe_size)) %>%
  ggplot(aes(x = universe_size, y = mean_sim_time, colour = factor(days))) +
    geom_line() +
    geom_point() +
    labs(
      x = "Universe size",
      y = "Mean simulation time, seconds",
      title = "Mean simulation time from 10 iterations",
      colour = "Time Steps"
    ) +
    theme_bw()


# larger universes
num_assets <- seq(1000, 5000, 1000)
num_days <- c(10, 20, 30, 40)*252

means2 <- list()
for(universe in num_assets) {
  print(glue("Doing universe size {universe}"))
  for(days in num_days) {
    print(glue("Doing {days} days"))
    means2 <- c(means2, get_mean_time(days, universe))
  }
}

df <- as.data.frame(matrix(unlist(means2), ncol = length(num_assets))) %>%
  mutate(days = num_days)

colnames(df) <- c(num_assets, "days")

df %>%
  pivot_longer(cols = -days, names_to = "universe_size", values_to = "mean_sim_time") %>%
  mutate(universe_size = as.numeric(universe_size)) %>%
  ggplot(aes(x = universe_size, y = mean_sim_time, colour = factor(days))) +
  geom_line() +
  geom_point()




