
# tests for days_to_expiry ------------------------------------------------

test_that("Days to expiry errors if date column is not Date type", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures$date <- as.character(futures$date)

  expect_error(days_to_expiry(futures), "date column must exist and be of type Date")
})

test_that("Days to expiry errors if ticker column is not present", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures <- dplyr::rename(futures, "Ticker" = ticker)

  expect_error(days_to_expiry(futures), "ticker column must exist")
})

test_that("Days to expiry is never negative", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures <- days_to_expiry(futures)

  expect_gte(min(futures$dte), 0)

})

test_that("Days to expiry is zero on expiry date", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures <- days_to_expiry(futures)
  expiries <- futures %>%
    dplyr::filter(date == expiry) %>%
    dplyr::pull(dte) %>%
    as.numeric()  # as.numeric() to convert from vector of durations

  expect_equal(expiries, rep(0, length(expiries)))
})

test_that("Days to expiry is five when five days from expiry date", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  test_dtes <- futures %>%
    days_to_expiry() %>%
    dplyr::filter(dte == 5) %>%
    dplyr::select(ticker, date, expiry, dte) %>%
    dplyr::mutate(test_dte = expiry - date) %>%
    dplyr::pull(test_dte) %>%
    as.numeric()

  expect_equal(test_dtes, rep(5, length(test_dtes)))
})


# tests for roll_on_dte ---------------------------------------------------

test_that("Incorrect date column errors", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures$date <- as.character(futures$date)

  expect_error(roll_on_dte(futures), "date column must exist and be of type Date")
})

test_that("Missing ticker column errors", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures <- dplyr::rename(futures, "Ticker" = ticker)

  expect_error(roll_on_dte(futures), "ticker column must exist")
})

test_that("Missing close column errors", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  futures <- dplyr::rename(futures, "Close" = close)

  expect_error(roll_on_dte(futures), "close column must exist")
})

test_that("cumulative returns are correct without costs", {
  futures <- readRDS(test_path("fixtures", "futures.rds"))

  # GC should get the return to contract GC-2021F on 2021-01-26 (1 dte)
  gc_close_2dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
  gc_close_1dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  expected_gc_return_1dte <- log(gc_close_1dte/gc_close_2dte)

  calculated_return_1dte <- futures %>%
    select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1) %>%
    filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
    pull(log_return)

  expect_equal(calculated_return_1dte, expected_gc_return_1dte)

  # GC should get the return to contract GC-2021G on 2021-01-27 (roll day)
  gc_close_before_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  gc_close_on_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-27"), "close", drop = TRUE]
  expected_gc_return_roll_day <- log(gc_close_on_roll_day/gc_close_before_roll_day) # return to GC-2021G on roll day

  calculated_return_roll_day <- futures %>%
    select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1) %>%
    filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-27"))) %>%
    pull(log_return)

  expect_equal(calculated_return_roll_day, expected_gc_return_roll_day)
})

test_that("cumulative returns are correct with costs", {
  # subtracts costs from roll day only
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  cost <- 0.1/100

  # TODO: first test here fails

  # GC should get the return to contract GC-2021F on 2021-01-26 (1 dte) and should not incur costs
  gc_close_2dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
  gc_close_1dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  expected_gc_return_1dte <- log(gc_close_1dte/gc_close_2dte) # should not incur trading costs

  calculated_return_1dte <- futures %>%
    select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1, cost = cost) %>%
    filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
    pull(log_return)

  expect_equal(calculated_return_1dte, expected_gc_return_1dte)

  # GC should get the return to contract GC-2021G on 2021-01-27 (roll day) less trading costs
  gc_close_before_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  gc_close_on_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-27"), "close", drop = TRUE]
  expected_gc_return_roll_day <- log(gc_close_on_roll_day/gc_close_before_roll_day) - costs # return to GC-2021G on roll day, less costs

  calculated_return_roll_day <- futures %>%
    select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1, cost = costs) %>%
    filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-27"))) %>%
    pull(log_return)

  expect_equal(calculated_return_roll_day, expected_gc_return_roll_day)
})

# test for roll_dte > 1 too

futures %>%
  select(ticker, date, close) %>%
  filter(stringr::str_extract(ticker, "[^-]+") %in% c("ES", "GC")) %>%
  days_to_expiry() %>%
  dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%   # extract everything before first "-"
  dplyr::group_by(date, symbol) %>%
  # when contracts$dte == roll_dte, sell the current contract at the close, and buy the next contract at the close
  # that is, when dte == roll_dte, we get the return for the current expiry, and on the next day we get the return for the next expiry
  # current contract is the one with the minimum dte that is >= roll_dte
  # filter current contract in several steps for clarity
  mutate(current_contract = case_when(dte >= 1 & dte == min(dte) ~ TRUE, TRUE ~ FALSE)) %>%
  filter(current_contract == TRUE) %>%
  select(-current_contract)


# make a dataframe of two symbols + 4 tickers
# simulate the roll - 2 days before and after expiry
# return should be the return to the contract itself, not the return of close-to-close
# prices of subsequent contracts
futs_subset <- futures %>%
  # days_to_expiry() %>%
  filter(
    (ticker %in% c("GC-2021F", "GC-2021G") & date %in% lubridate::as_date(c("2021-01-24", "2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29"))) |
    (ticker %in% c("ES-2021H", "ES-2021M") & date %in% lubridate::as_date(c("2021-03-16", "2021-03-17", "2021-03-18", "2021-03-19", "2021-03-20", "2021-03-21")))
  ) %>%
  select(ticker, date, close) ##, dte)

# GC should get the return to contract GC-2021F on 2021-01-26 (1 dte) and the return to contract GC-2021G on 2021-01-27
gc_close_2dte <- futs_subset[futs_subset$ticker == "GC-2021F" & futs_subset$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
gc_close_1dte <- futs_subset[futs_subset$ticker == "GC-2021F" & futs_subset$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
expected_gc_return_1dte <- log(gc_close_1dte/gc_close_2dte)

calculated_return_1dte <- futures %>%
  select(ticker, date, close) %>%
  roll_on_dte() %>%
  filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
  pull(log_return)

calculated_return_1dte == expected_gc_return_1dte

# closing prices of GC-2021G
gc_close_before_roll_day <- futs_subset[futs_subset$ticker == "GC-2021G" & futs_subset$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
gc_close_on_roll_day <- futs_subset[futs_subset$ticker == "GC-2021G" & futs_subset$date == lubridate::as_date("2021-01-27"), "close", drop = TRUE]
expected_gc_return_roll_day <- log(gc_close_on_roll_day) # return to GC-2021G on roll day

calculated_return_roll_day <- futures %>%
  select(ticker, date, close) %>%
  roll_on_dte() %>%
  filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-27"))) %>%
  pull(log_return)


