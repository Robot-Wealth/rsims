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




