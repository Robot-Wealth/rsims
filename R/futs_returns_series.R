#' Number of Days to Expiry
#'
#' @description Add dte column to dataframe of futures contract prices. Here,
#' dte refers to calendar days to expiry
#'
#' @param contracts dataframe of futures contract with at least
#' ticker/date fields. date must be a date object.
#'
#' @return dataframe of futures contract prices/dates/dte
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
        dplyr::summarise(expiry = last(date)),
      by = "ticker"
    ) %>%
    dplyr::mutate(dte = expiry - date)
}


#' Roll on Days to Expiry
#'
#' @description Create a continuous returns series from a set of futures
#' contracts by rolling on a given number of days to expiry.
#'
#' @param contracts List of dfs of futures date/price
#' @param roll_dte Calendar days to expiry to roll into next contract
#' @param cost Percent cost of trading into next contract
#'
#' @return A continuous futures return series
#' @export
#'
#' @examples
roll_on_dte <- function(contracts, roll_dte, cost) {

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
roll_on_dte <- function(contracts, cost) {

}
