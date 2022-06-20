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
    # TODO: this will fail when we roll into a contract on its first day of existence
      # ie lag(current_contract) won't be false... it'll be NA
    dplyr::mutate(was_current_contract = dplyr::case_when(dplyr::lag(current_contract) == TRUE ~ TRUE, TRUE ~ FALSE)) %>%
    dplyr::mutate(roll_cost = dplyr::case_when(current_contract == TRUE & was_current_contract == FALSE ~ roll_cost, TRUE ~ 0)) %>%
    dplyr::filter(current_contract == TRUE) %>%
    dplyr::select(-current_contract) %>%
    dplyr::mutate(
      log_return = log_return - roll_cost,
      simple_return = simple_return - roll_cost
      ) %>%
    # aggregate returns by jamming together consecutive contracts for each symbol
    # aggregate returns by symbol
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      cum_return = 1 + cumsum(log_return),
      cum_simp_return = cumprod(1 + simple_return)
    )
}

#' Roll on Open Interest
#'
#' @description Create a continuous returns series from a set of futures
#' contracts by rolling into the adjacent contract with the highest open interest.
#'
#' @param contracts List of dfs of futures date/price/open interest
#' @param cost Percent cost of trading into next contract
#'
#' @return A continuous futures return series
#' @export
#'
#' @examples
roll_on_oi <- function(contracts, cost) {
  contracts <- days_to_expiry(contracts)

  contracts %>%
    # calculate returns separately for each contract (ticker)
    group_by(ticker) %>%
    mutate(
      log_return = log(close/dplyr::lag(close)),
      simple_return = (close - dplyr::lag(close))/dplyr::lag(close)
    ) %>%
    na.omit() %>%
    # make new variable corresponding to base symbol (ES, GC, etc)
    mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%   # extract everything before first "-"
    group_by(date, symbol) %>%
    # get the contract with the highest open interest
    filter(open_interest == max(open_interest), open_interest > 0) %>%
    # aggregate returns by jamming together consecutive contracts for each symbol
    group_by(symbol) %>%
    mutate(
      cum_return = 1 + cumsum(log_return),
      cum_simp_return = cumprod(1 + simple_return)
    )
}
