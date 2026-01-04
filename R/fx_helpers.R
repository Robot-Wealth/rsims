#' Make FX Prices Matrix
#'
#' @description Convert a long-format dataframe of FX prices to a matrix suitable
#' for fx_backtest. The first column will be the date (as numeric) and remaining
#' columns will be the exchange rates for each pair.
#'
#' @param prices_df Long dataframe with columns: pair, date, close
#' @return Wide matrix suitable for fx_backtest (date column + pair columns)
#' @export
#'
#' @examples
#' \dontrun{
#' prices_df <- data.frame(
#'   pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
#'   close = c(1.10, 1.11, 110, 111)
#' )
#' prices_matrix <- make_fx_prices_matrix(prices_df)
#' }
make_fx_prices_matrix <- function(prices_df) {
  prices_df %>%
    dplyr::select(pair, date, close) %>%
    dplyr::arrange(date, pair) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from = pair,
      values_from = close
    ) %>%
    dplyr::mutate(date = as.numeric(date)) %>%
    data.matrix()
}

#' Make FX Weights Matrix
#'
#' @description Convert a long-format dataframe of weights to a matrix suitable
#' for fx_backtest.
#'
#' @param weights_df Long dataframe with columns: pair, date, weight
#' @return Wide matrix suitable for fx_backtest (date column + pair columns)
#' @export
#'
#' @examples
#' \dontrun{
#' weights_df <- data.frame(
#'   pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
#'   weight = c(0.5, 0.5, 0.5, 0.5)
#' )
#' weights_matrix <- make_fx_weights_matrix(weights_df)
#' }
make_fx_weights_matrix <- function(weights_df) {
  weights_df %>%
    dplyr::select(pair, date, weight) %>%
    dplyr::arrange(date, pair) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from = pair,
      values_from = weight
    ) %>%
    dplyr::mutate(date = as.numeric(date)) %>%
    data.matrix()
}

#' Make FX Base Currency Rates Matrix
#'
#' @description Build a matrix of base currency rates for position sizing.
#' For each FX pair, this provides the XXX/USD rate for the pair's base currency,
#' which is used to convert USD equity to base currency units for position sizing.
#'
#' @param prices_df Long dataframe with pair, date, close columns
#' @param pair_base_currencies Named vector mapping pair names to their base currency
#'   e.g., c(EURUSD = "EUR", USDJPY = "USD", EURJPY = "EUR")
#' @param usd_rates_df Long dataframe with columns: currency, date, rate
#'   where rate is the XXX/USD rate (how many USD per 1 unit of currency).
#'   USD should have rate = 1.0
#' @return Wide matrix suitable for fx_backtest base_currency_rates parameter
#' @export
#'
#' @examples
#' \dontrun{
#' pair_base <- c(EURUSD = "EUR", USDJPY = "USD", EURJPY = "EUR")
#'
#' usd_rates_df <- data.frame(
#'   currency = c("EUR", "EUR", "USD", "USD"),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
#'   rate = c(1.10, 1.11, 1.0, 1.0)
#' )
#'
#' base_rates <- make_fx_base_rates_matrix(prices_df, pair_base, usd_rates_df)
#' }
make_fx_base_rates_matrix <- function(prices_df, pair_base_currencies, usd_rates_df) {
  # Get unique dates from prices_df
  dates <- unique(prices_df$date)

  # Get unique pairs
  pairs <- names(pair_base_currencies)

  # Build long dataframe of base rates for each pair
  result_list <- list()
  for (pair in pairs) {
    base_currency <- pair_base_currencies[pair]

    # Get rates for this currency
    currency_rates <- usd_rates_df %>%
      dplyr::filter(currency == base_currency) %>%
      dplyr::select(date, rate)

    if (nrow(currency_rates) == 0) {
      stop(glue::glue("No rates found for currency '{base_currency}' (base of {pair})"))
    }

    result_list[[pair]] <- currency_rates %>%
      dplyr::mutate(pair = pair) %>%
      dplyr::rename(base_rate = rate)
  }

  # Combine and pivot
  dplyr::bind_rows(result_list) %>%
    dplyr::arrange(date, pair) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from = pair,
      values_from = base_rate
    ) %>%
    dplyr::mutate(date = as.numeric(date)) %>%
    data.matrix()
}

#' Make FX Quote Currency Rates Matrix
#'
#' @description Build a matrix of quote currency conversion rates for P&L reporting.
#' For each FX pair, this provides a multiplier to convert quote-currency P&L to USD.
#' For pairs where the quote currency is USD, this is 1.0.
#' For other pairs, this is 1/USDXXX (e.g., 1/USDJPY for JPY-denominated P&L).
#'
#' @param prices_df Long dataframe with pair, date, close columns
#' @param pair_quote_currencies Named vector mapping pair names to their quote currency
#'   e.g., c(EURUSD = "USD", USDJPY = "JPY", EURJPY = "JPY")
#' @param usd_rates_df Long dataframe with columns: currency, date, rate
#'   where rate is the XXX/USD rate. For quote currency conversion,
#'   the function will compute 1/USDXXX = XXX/USD rate.
#'   USD should have rate = 1.0
#' @return Wide matrix suitable for fx_backtest quote_currency_rates parameter
#' @export
#'
#' @examples
#' \dontrun{
#' pair_quote <- c(EURUSD = "USD", USDJPY = "JPY", EURJPY = "JPY")
#'
#' # Note: For JPY, you need the USD/JPY rate data, which will be inverted
#' usd_rates_df <- data.frame(
#'   currency = c("USD", "USD", "JPY", "JPY"),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
#'   rate = c(1.0, 1.0, 0.009091, 0.009009)  # JPY rate is 1/USDJPY
#' )
#'
#' quote_rates <- make_fx_quote_rates_matrix(prices_df, pair_quote, usd_rates_df)
#' }
make_fx_quote_rates_matrix <- function(prices_df, pair_quote_currencies, usd_rates_df) {
  # Get unique pairs
  pairs <- names(pair_quote_currencies)

  # Build long dataframe of quote rates for each pair
  result_list <- list()
  for (pair in pairs) {
    quote_currency <- pair_quote_currencies[pair]

    # Get rates for this currency
    currency_rates <- usd_rates_df %>%
      dplyr::filter(currency == quote_currency) %>%
      dplyr::select(date, rate)

    if (nrow(currency_rates) == 0) {
      stop(glue::glue("No rates found for currency '{quote_currency}' (quote of {pair})"))
    }

    result_list[[pair]] <- currency_rates %>%
      dplyr::mutate(pair = pair) %>%
      dplyr::rename(quote_rate = rate)
  }

  # Combine and pivot
  dplyr::bind_rows(result_list) %>%
    dplyr::arrange(date, pair) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from = pair,
      values_from = quote_rate
    ) %>%
    dplyr::mutate(date = as.numeric(date)) %>%
    data.matrix()
}

#' Make FX Swap Rates Matrix
#'
#' @description Convert a long-format dataframe of swap rates to a matrix suitable
#' for fx_backtest. The resulting matrix has paired columns for each pair:
#' {PAIR}_long and {PAIR}_short.
#'
#' @param swap_df Long dataframe with columns: pair, date, swap_long, swap_short
#'   swap_long is the daily rate for long positions (positive = credit, negative = debit)
#'   swap_short is the daily rate for short positions
#' @return Wide matrix suitable for fx_backtest swap_rates parameter
#' @export
#'
#' @examples
#' \dontrun{
#' swap_df <- data.frame(
#'   pair = c("EURUSD", "EURUSD", "USDJPY", "USDJPY"),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
#'   swap_long = c(-0.0001, -0.0001, 0.00005, 0.00005),
#'   swap_short = c(0.00005, 0.00005, -0.0001, -0.0001)
#' )
#' swap_matrix <- make_fx_swap_matrix(swap_df)
#' }
make_fx_swap_matrix <- function(swap_df) {
  # Create long columns and short columns separately
  swap_long <- swap_df %>%
    dplyr::select(pair, date, swap_long) %>%
    dplyr::mutate(pair = paste0(pair, "_long")) %>%
    dplyr::rename(value = swap_long)

  swap_short <- swap_df %>%
    dplyr::select(pair, date, swap_short) %>%
    dplyr::mutate(pair = paste0(pair, "_short")) %>%
    dplyr::rename(value = swap_short)

  # Combine and pivot
  dplyr::bind_rows(swap_long, swap_short) %>%
    dplyr::arrange(date, pair) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from = pair,
      values_from = value
    ) %>%
    dplyr::mutate(date = as.numeric(date)) %>%
    data.matrix()
}
