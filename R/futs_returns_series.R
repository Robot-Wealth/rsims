#' Number of Days to Expiry
#'
#' @description Add dte column to dataframe of futures contract prices. Here,
#' dte refers to calendar days to expiry
#'
#' @param contracts dataframe of futures contract with at least
#' ticker/date fields. date must be a date object.
#'
#' @return dataframe of futures contracts with date/dte fields
#' @export
#'
#' @examples
days_to_expiry <- function(contracts) {
  stopifnot(
    "date column must exist and be of type Date" = class(contracts$date) == "Date",
    "ticker column must exist" = "ticker" %in% colnames(contracts)
    )

  contracts %>%
    dplyr::left_join(
      contracts %>%
        dplyr::group_by(ticker) %>%
        dplyr::summarise(expiry = dplyr::last(date)),
      by = "ticker"
    ) %>%
    dplyr::mutate(dte = expiry - date)
}


#' Roll on Days to Expiry
#'
#' @description Create a continuous returns series from a set of futures
#' contracts by rolling on a given number of days to expiry.
#'
#' @param contracts List of dfs of futures with at least ticker/date/close columns
#' @param roll_dte Calendar days to expiry to roll into next contract
#' @param cost Percent cost of trading into next contract (total cost of
#' trading out of current contract and into next contract)
#'
#' @return A continuous futures return series
#' @export
#'
#' @examples
roll_on_dte <- function(contracts, roll_dte = 1, roll_cost = 0) {
  stopifnot(
    "date column must exist and be of type Date" = class(contracts$date) == "Date",
    "ticker column must exist" = "ticker" %in% colnames(contracts),
    "close column must exist" = "close" %in% colnames(contracts)
  )
  contracts <- days_to_expiry(contracts)

  contracts %>%
    # calculate returns separately for each contract (ticker)
    # do this first before we throw away any data based on dte
    dplyr::group_by(ticker) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      log_return = log(close/dplyr::lag(close)),
      simple_return = (close - dplyr::lag(close))/dplyr::lag(close)
    ) %>%
    na.omit() %>%
    # make new variable corresponding to base symbol (ES, GC, etc)
    dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%   # extract everything before first "-"
    dplyr::group_by(date, symbol) %>%
    # when contracts$dte == roll_dte, sell the current contract at the close, and buy the next contract at the close
    # that is, when dte == roll_dte, we get the return for the current expiry, and on the next day we get the return for the next expiry
    # current contract is the one with the minimum dte that is >= roll_dte
    # filter current contract in several steps for clarity
    dplyr::filter(dte >= roll_dte) %>%
    dplyr::mutate(current_contract = dplyr::case_when(dte == min(dte) ~ TRUE, TRUE ~ FALSE)) %>%
    # also calculate a costs column here based on change in current contract by ticker (contract, not symbol)
    dplyr::group_by(ticker) %>%
    # this will also accrue costs when we roll into a contract on its first day of existence, or on the first day in our contracts dataframe - because lag(current_contract) will be NA, which is captured in the case_when below.
    dplyr::mutate(was_current_contract = dplyr::case_when(dplyr::lag(current_contract) == TRUE ~ TRUE, TRUE ~ FALSE)) %>%
    dplyr::mutate(roll_cost = dplyr::case_when(current_contract == TRUE & was_current_contract == FALSE ~ roll_cost, TRUE ~ 0)) %>%
    dplyr::filter(current_contract == TRUE) %>%
    dplyr::mutate(
      log_return = log_return - roll_cost,
      simple_return = simple_return - roll_cost
      ) %>%
    # aggregate returns by linking together consecutive contracts for each symbol
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      cum_return = 1 + cumsum(log_return),
      cum_simp_return = cumprod(1 + simple_return)
    ) %>%
    dplyr::select(symbol, ticker, date, dte, current_contract, was_current_contract, roll_cost, log_return, simple_return, cum_return, cum_simp_return)
}

#' Roll on Open Interest
#'
#' @description Create a continuous returns series from a set of futures
#' contracts by rolling into the adjacent contract with the highest open interest.
#'
#' "Sees the future" in that it trades into the next contract at the close of
#' the day on which max open interest switches. This is OK because it is usually
#' obvious from intraday volumes that max open interest has switched.
#'
#' Example: GC - roll from G to J on 19 Jan 2021. On the 19th, sell G and buy J.
#' Get the (15-19 close-2-close - 19th is a Monday) return to G on the 19th, and the
#' (19-20 close-2-close) return to J on the 20th.
#'
#' Costs show up in the return on the day after the roll. This is largely an
#' arbitrary accounting decision: we trade out of one contract and into another
#' at the close on on roll day, which affects:
#' * the return to the contract we trade out of on roll day, and
#' * the return to the contract we trade into on roll day plus 1.
#' Here, the entire cost shows up in the return to the new contract on roll day
#' plus 1. This is a simplifying but arbitrary accounting decision.
#'
#' @param contracts List of dfs of futures date/price/open interest
#' @param cost Percent cost of trading into next contract
#'
#' @return A continuous futures return series
#' @export
#'
#' @examples
roll_on_oi <- function(contracts, roll_cost = 0) {
  stopifnot(
    "date column must exist and be of type Date" = class(contracts$date) == "Date",
    "ticker column must exist" = "ticker" %in% colnames(contracts),
    "close column must exist" = "close" %in% colnames(contracts),
    "open_interest column must exist" = "open_interest" %in% colnames(contracts)
  )
  contracts <- days_to_expiry(contracts)

  contracts %>%
    # calculate returns separately for each contract (ticker)
    dplyr::group_by(ticker) %>%
    dplyr::mutate(
      log_return = log(close/dplyr::lag(close)),
      simple_return = (close - dplyr::lag(close))/dplyr::lag(close)
    ) %>%
    na.omit() %>%
    # make new variable corresponding to base symbol (ES, GC, etc)
    # extract everything before first "-"
    dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%
    # get yesterday's open interest by ticker
    dplyr::mutate(lag_open_interest = dplyr::lag(open_interest)) %>%
    # get the contract with yesterday's highest open interest for each symbol for each day
    # need yesterday's highest open interest in order to get the return from the prior close
    # for the current contract
    dplyr::group_by(date, symbol) %>%
    dplyr::mutate(current_contract = dplyr::case_when(lag_open_interest == max(lag_open_interest) ~ TRUE, TRUE ~ FALSE)) %>%
    # calculate a costs column here based on change in current contract by ticker (contract, not symbol)
    dplyr::group_by(ticker) %>%
    # this will also accrue costs when we roll into a contract on its first day of existence, or on the first day in our contracts dataframe - because lag(current_contract) will be NA, which is captured in the case_when below.
    dplyr::mutate(was_current_contract = dplyr::case_when(dplyr::lag(current_contract) == TRUE ~ TRUE, TRUE ~ FALSE)) %>%
    dplyr::mutate(roll_cost = dplyr::case_when(current_contract == TRUE & was_current_contract == FALSE ~ roll_cost, TRUE ~ 0)) %>%
    dplyr::filter(current_contract == TRUE) %>%
    # filter(open_interest == max(open_interest), open_interest > 0) %>%
    dplyr::mutate(
      log_return = log_return - roll_cost,
      simple_return = simple_return - roll_cost
    ) %>%
    # aggregate returns by linking together consecutive contracts for each symbol
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      cum_return = 1 + cumsum(log_return),
      cum_simp_return = cumprod(1 + simple_return)
    ) %>%
    dplyr::select(symbol, ticker, date, dte, open_interest, lag_open_interest, current_contract, was_current_contract, roll_cost, log_return, simple_return, cum_return, cum_simp_return)
}
