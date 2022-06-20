
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
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_1dte, expected_gc_return_1dte)

  # GC should get the return to contract GC-2021G on 2021-01-27 (roll day)
  gc_close_before_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  gc_close_on_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-27"), "close", drop = TRUE]
  expected_gc_return_roll_day <- log(gc_close_on_roll_day/gc_close_before_roll_day) # return to GC-2021G on roll day

  calculated_return_roll_day <- futures %>%
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-27"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_roll_day, expected_gc_return_roll_day)
})

test_that("cumulative returns are correct with costs", {
  # subtracts costs from roll day only
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  cost <- 0.1/100

  # GC should get the return to contract GC-2021F on 2021-01-26 (1 dte) and should not incur costs
  gc_close_2dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
  gc_close_1dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  expected_gc_return_1dte <- log(gc_close_1dte/gc_close_2dte) # should not incur trading costs

  calculated_return_1dte <- futures %>%
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1, roll_cost = cost) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_1dte, expected_gc_return_1dte)

  # GC should get the return to contract GC-2021G on 2021-01-27 (roll day) less trading costs
  gc_close_before_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  gc_close_on_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-27"), "close", drop = TRUE]
  expected_gc_return_roll_day <- log(gc_close_on_roll_day/gc_close_before_roll_day) - cost # return to GC-2021G on roll day, less costs

  calculated_return_roll_day <- futures %>%
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = 1, roll_cost = cost) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-27"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_roll_day, expected_gc_return_roll_day)
})

test_that("cumulative returns are correct with costs for roll_dte > 1", {
  # subtracts costs from roll day only
  roll_dte <- 2
  futures <- readRDS(test_path("fixtures", "futures.rds"))
  cost <- 0.1/100

  # GC should get the return to contract GC-2021F on 2021-01-25 (1 dte) and should not incur costs
  gc_close_2dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-22"), "close", drop = TRUE]
  gc_close_1dte <- futures[futures$ticker == "GC-2021F" & futures$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
  expected_gc_return_1dte <- log(gc_close_1dte/gc_close_2dte) # should not incur trading costs

  calculated_return_1dte <- futures %>%
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = roll_dte, roll_cost = cost) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-25"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_1dte, expected_gc_return_1dte)

  # GC should get the return to contract GC-2021G on 2021-01-26 (roll day) less trading costs
  gc_close_before_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-25"), "close", drop = TRUE]
  gc_close_on_roll_day <- futures[futures$ticker == "GC-2021G" & futures$date == lubridate::as_date("2021-01-26"), "close", drop = TRUE]
  expected_gc_return_roll_day <- log(gc_close_on_roll_day/gc_close_before_roll_day) - cost # return to GC-2021G on roll day, less costs

  calculated_return_roll_day <- futures %>%
    dplyr::select(ticker, date, close) %>%
    roll_on_dte(roll_dte = roll_dte, roll_cost = cost) %>%
    dplyr::filter(symbol == "GC" & date == lubridate::as_date(c("2021-01-26"))) %>%
    dplyr::pull(log_return)

  expect_equal(calculated_return_roll_day, expected_gc_return_roll_day)
})
