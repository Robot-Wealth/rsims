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
